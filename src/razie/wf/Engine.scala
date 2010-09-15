/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.{ AA, Debug, Log }
import razie.base.{ ActionContext => AC }
import razie.g._
import scala.actors._

//-------------------- graph processing engine

object Audit {
  private def audit(aa: AA) = razie.Audit(aa)

  val EV = "Event"
  val PR = "process"
  val AC = "activity"
  val VAL = "value"

  def recCreate(a: Process) = audit(razie.AA(EV, "CREATE", PR, a))
  def recStart(a: Process, v: Any) = audit(razie.AA(EV, "START", PR, a, VAL, v.asInstanceOf[AnyRef]))
  def recDone(a: Process, v: Any) = audit(razie.AA(EV, "DONE", PR, a, VAL, v.asInstanceOf[AnyRef]))
  def recExecBeg(a: WfActivity, in: Any) =
    audit(razie.AA(EV, "EXEC.beg", AC, a, "in", in.asInstanceOf[AnyRef]))
  def recExecEnd(a: WfActivity, in: Any, out: Any, paths: Int) =
    audit(razie.AA(EV, "EXEC.end", AC, a, "in", in.asInstanceOf[AnyRef], "out", out.asInstanceOf[AnyRef], "paths", paths))
  def recResNotFound(a: WfActivity, res: GRef) =
    audit(razie.AA(EV, "ERROR.resNotFound", AC, a, "res", res))
}

/** 
 * a process thread encapsulates the state of a running workflow branch. You can see this as an 
 * actor as well. Depending on the engine, each PT may have its own processor, thread or whatever...
 * conceptually these are independent paths of execution, for the PAR/JOIN branches for instance
 */
class ProcessThread(val parent: Process, val start: WfActivity, val startLink: Option[WfLink], val ctx: AC, val startValue: Any) {
  // should i keep state inside the activities or outside in a parallel graph, built as it's traversed?

  var currV: Any = startValue

  // the activity in progress
  var currAct: WfActivity = null

  // current resource waiting for...if any
  var currReq: Option[WRes.Req] = None

  // the activities to start on the next step
  var nextAct = start
  var nextLink = startLink

  protected def preProcessing = {
    currAct = nextAct
    currAct.procState = ProcState.INPROGRESS
    nextLink map (_.linkState = LinkState.DONE)
  }

  protected def postProcessing = {
    currAct.procState = ProcState.DONE
  }

  /** execute current action and advance: return continuations: either myself or a
   * bunch of spawns */
  def execAndAdvance : (Any, Seq[ProcessThread]) = {
    preProcessing

    Audit.recExecBeg(currAct, currV)
    val (out, next) = currAct.traverse(nextLink, ctx, currV)
    Audit.recExecEnd(currAct, currV, out, next.size)
    currV = out

    next map (_.linkState = LinkState.SELECTED)

    val ret = if (next.size == 1) {
      nextLink = next.headOption
      nextAct = next.head.z
      this :: Nil // myself continues
    } else {
      nextAct = null
      if (next.size > 1)
        // TODO what value to use for start here?
        next.map(l => new ProcessThread(parent, l.z, Some(l), ctx, currV))
      // note that I'm done in this case and replace myself with these spawns
      else
        Nil // done - no continuations
    }

    postProcessing
    (currV, ret)
  }

  def done(): Boolean = { val b = nextAct == null; Debug("done: " + b); b }
}

/** 
 * a process instance encapsulates the state of the running workflow 
 * 
 * the state needed to recover is: the definition graph, the graph of states with links to specs and 
 * the values context.
 * 
 * I don't think the history should be kept, unless we're running in some sort of "debug" or "audit" mode.
 */
class Process(val start: WfActivity, val ctx: AC, startV: Any) {
  val id = GRef.id("WfProcess", GRef.uid)
  var lastV = startV

  // the threads in progress these are all in parallel
  var currThreads: Seq[ProcessThread] = new ProcessThread(this, start, None, ctx, startV) :: Nil
  private[this] var countThreads = 0 // yeah, 0 is correct
  var oldThreads: Seq[ProcessThread] = Nil // don't know why i keep these

  def setThreadCount(i: Int) = synchronized { this.countThreads += i }

  // progress one thread - this would normally happen on its own thread
  def execAndAdvance(t: ProcessThread): Seq[ProcessThread] = {
    val (v, n) = t.execAndAdvance
    synchronized {
      if (!t.currAct.isInstanceOf[AndJoin] || n.size <= 0)
        lastV = v
    }
    Debug("execAndAdvance() lastV == " + lastV)
    n
  }

  // if ran sync, theaads list is relevant . when async, count is relevant
  def done(): Boolean = synchronized { val b = /* currThreads.isEmpty || */ countThreads <= 0; Debug("done=" + b + " currThreads=" + currThreads.size + " countThreads=" + countThreads); b }

  def persist = "" // TODO
  def recover(s: Any) {} // TODO
}

/** the engine is more complicated in this case, basically graph traversal */
class Engine {

  import Actor._

  val processes = new scala.collection.mutable.ListBuffer[Process]()
  var stopped = false

  // load balancer
  val processor: Actor = actor {
    var i = 0
    loop {
      react {
        case Exit => {
          processors foreach (_ ! Exit)
          exit();
        }
        case m@_ => {
          processors(i) ! m;
          i = (i + 1) % 5
        }
      }
    }
  }

  val processors: Array[Actor] = Array.fill(5) {
    actor {
      loop {
        react {
          // tick is used just to 
          case Tick(p, a) => {
            val s = p.currThreads.map(Tack(p, _, a)).toList
            p setThreadCount s.size
            s foreach (processor ! _)

            if (p.done) a ! Done(p) // how to return the value?
          }

          case Tack(p, t, a) => {
            val s = p.execAndAdvance(t) // this carries out the actual work...

            t.currAct match {

              case re: WfResReq => { // make the actual request
                val cback = new WResUser {
                  override def key = GRef.id("blahblah", "?")
                  def notifyReply(reply: WRes.ReqReply) = processor ! Reply(p, t, reply, a)
                  def notifyScrewup(who: WRes) = {
                    processor ! Reply(p, t, null, a)
                    throw new UnsupportedOperationException("TODO") // TODO
                  }
                }

                t.currReq = Some(
                  new WRes.Req(cback, re.tok, re.what, re.attrs, re.value(t.ctx, t.currV))
                  )

                Debug("ENG.Req: " + t.currReq.get)
                val rep = re.req(t.currReq.get)
                Debug("ENG.Req returned: " + rep)
                rep match {
                  case ok@Some(WResRROK(w, tok, r)) => processor ! Reply(p, t, ok.get, a)
                  case er@Some(WResRRERR(w, tok, e)) => processor ! Reply(p, t, er.get, a)
                  case wa@Some(WResRRWAIT(w, tok)) => ; // current action waits
                  case None => Audit.recResNotFound(re, re.res)
                }
              }

              case co: WfStopOthers => { // cancel others
                Debug("ENG.cancelOthers: " + t.currReq.get)
                val s = p.currThreads.filter(_ != t).map(StopThread(p, _, a)).toList
                s foreach (processor ! _)
              }

              // otherwise, it's a normal activity, continue all remaining threads
              case _ => {
                p.synchronized {
                  p setThreadCount (s.size - 1)
                  // TODO err if first action on only thread is REQ then it will finish before reply
                  if (p.done) {
                    Debug("DOOOOOOOOOOOOOOOOOOONE");
                    a ! Done(p)
                    assert(s.size == 0)
                  } // how to return the value?
                }
              }
              s map (processor ! Tack(p, _, a))
            }
          } // case Tack

          // reply from resource - continue original thread
          case Reply(p, t, rr, a) => {
            Debug("ENG.Reply: " + rr)
            t.nextAct match {
              case re: WfResReply => {
                t.currReq = None
                re.reply(rr)
                processor ! Tack(p, t, a)
              }
              case s@_ => throw new IllegalStateException("WfResReq MUST be followed by a WfResReply, not a " + s)
            }

            //         if (p.done) a ! Done(p) // how to return the value?
          }

          // TODO stop this thread
          case StopThread(p, t, a) => {
            Debug("ENG.StopThread: " + t)
//            p.stop (t)
//            t.nextAct match {
 //             case re: WfResReply =>  // was waiting for reply
              // TODO should cancel the request with the resource
 //           }

            //         if (p.done) a ! Done(p) // how to return the value?
          }

          case Exit => exit()
        }
      }
    }
  }

  case class Start(p: Process)
  case class Tick(p: Process, result: Actor)
  case class Tack(p: Process, t: ProcessThread, result: Actor)
  case class Reply(p: Process, t: ProcessThread, rr: WRes.ReqReply, result: Actor)
  case class StopThread(p: Process, t: ProcessThread, result: Actor)
  case class Pre(p: Process)
  case class Done(p: Process)
  case class Exit ()

  def create(start: WfActivity, ctx: AC, startV: Any): GRef = {
    import wf._
    val p = new Process(wf.scope(start), ctx, startV)
    processes += p

    Audit.recCreate(p)
    p.id
  }

  def start(id: GRef, startV: Any): GRef = {
    val t = istart(id, startV, true)
    t._2 ! Start(t._1)
    t._1.id
  }

  private[this] def istart(id: GRef, startV: Any, async: Boolean): (Process, Actor) = {
    val p = processes.find(_.id == id) getOrElse (throw new IllegalArgumentException("Process not found id: " + id))
    p.lastV = startV
    Audit.recStart(p, startV)

    val me: Actor = new Actor {
      override def act() = receive {
        case Start(p) => {
          preProcess(p)
          processor ! Tick(p, this)
          receive {
            case Done(p) => {
              Audit.recDone(p, p.lastV)
              processes -= p // maybe keep it around?
            }
          }
          reply()
        }
      }
    }

    me.start

    (p, me)
  }

  def exec(start: WfActivity, ctx: AC, startV: Any): Any = {
    // wrap into begin/end
    import wf._

    val t = istart(create(start, ctx, startV), startV, false)

    t._2 !? Start(t._1) // just wait, discard reply

    t._1.lastV
  }

  /** TODO complete this sync execution - good for debugging or something...right now it's missing async resources */
  def execSync(start: WfActivity, ctx: AC, startV: Any): Any = {
    val p = new Process(start, ctx, startV)

    preProcess(p)

    // progress all threads - this is temp until I get the threading done
    while (!p.done) 
      p.currThreads = p.currThreads.flatMap(p.execAndAdvance(_)) 

    p.lastV
  }

  def preProcess(p: Process) {
    // cache z ends to AndJoins
    razie.g.Graphs.foreach(p.start, (l: WfActivity, v: Int) => {}, (l: WfLink, v: Int) => l.z match { case a: AndJoin => a addIncoming l; case _ => }
      )
  }

  def checkpoint() {}

  def stop = synchronized {
    if (!processes.isEmpty)
      throw new IllegalStateException("there are still Processes in progress")

    stopped = true
    processor ! Exit
  }
}

// TODO implement a nice pool of engines
object Engines {
  var dflt: Option[Engine] = None
  def stop = synchronized { dflt map (_.stop); dflt = None }
  def start = synchronized { stop; dflt = Some(new Engine()); dflt.get }
  def apply() = synchronized { val e = (dflt getOrElse start); if (e.stopped) start else e }
}

abstract class ProcessWaitingOnRes(eng: Engine, who: ProcessThread, res: WRes) extends WResUser {
  /** a wait finished */
  override def notifyReply(reply: WRes.ReqReply) = {}

  /** the resource screwed up - all outstanding requests were dismissed */
  override def notifyScrewup(who: WRes) = {}
}

/** the special activities that control the engine itself - hopefully as few as possible */
object wfeng {
  // TODO start a new process
  def process(root: WfActivity): WfActivity = wf.todo
  // TODO start a new sub-process of the current process
  def subprocess(root: WfActivity): WfActivity = wf.todo

  // join parallel branches - it's basically a barrier: when all branches arrive here, it will continue
  def andjoin(a: WfActivity*): WfActivity = wf.todo
}

// and-join state: track incoming links and their values
trait AJState extends WfaState {
  var prev: scala.collection.mutable.HashMap[WfLink, Any] = null

  def addLink(l: WfLink, v: Any) = {
    if (prev == null)
      prev = razie.Mapi[WfLink, Any]()
    prev.put(l, v)
  }
}

/** join parallel branches */
case class AndJoin () extends WfActivity with AJState {
  var incoming: List[WfLink] = Nil // upon starting, the engine will traverse and populate this
  def addIncoming(l: WfLink) = incoming = List(l) ::: incoming

  /** waiting for all incoming branches and concatenating their values */
  override def traverse(in: AC, v: Any): (Any, Seq[WfLink]) =
    throw new IllegalStateException("method should never be called")

  override def traverse(from: Option[WfLink], in: AC, v: Any): (Any, Seq[WfLink]) = this.synchronized {
    addLink(from.get, v) // TODO null should bomb indicating andjoin with no branches in...
    Debug("AndJoin " + "prev=" + prev.size + " incoming=" + incoming.size)
    if (prev.size == incoming.size) (prev.values.map(x => x).toList, glinks) // I'm done
    else (v, Nil) // current thread dies
  }

  override def toString = "AndJoin " + incoming.size
}
