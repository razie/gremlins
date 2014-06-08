/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins.eng

import razie.{ AA }
import razie.base.{ ActionContext => AC }
import razie.g._
import scala.actors._
import com.razie.pubstage.life._
import razie.gremlins._
import razie.gremlins.act._
import razie.wf
import razie.Logging

//-------------------- graph processing engine

/** special engine activities are tagged with this */
trait SpecOps

/** the state of a Process Thread: current value, current activity etc */
class PTState(start: WfActivity, startValue: Any, startLink: Option[WfLink]) {
  var currV: Any = startValue

  /** the activity in progress */
  var currAct: WfActivity = null

  /** current resource waiting for...if any */
  var currReq: Option[WRes.Req] = None

  /** who is processing me right now, if any - if needed to stop stuff */
  var currActor: Option[Killeable] = None

  /** the activities to start on the next step */
  var nextAct = start
  var nextLink = startLink
}

/**
 * a process thread encapsulates the state of a running workflow branch. You can see this as an
 *  actor as well. Depending on the engine, each PT may have its own processor, thread or whatever...
 *
 *  conceptually these are independent paths of execution, for the PAR/JOIN branches for instance
 */
class ProcessThread(
  val parent: Process,
  start: WfActivity,
  startLink: Option[WfLink],
  val ctx: AC, 
  startValue: Any) extends PTState(start, startValue, startLink) with Logging {
  // should i keep state inside the activities or outside in a parallel graph, built as it's traversed?

  /** pre-processing the next activity */
  protected def preProcessing = {
    // TODO keep skipping who's not ready...
    //    Graphs.colored [WfActivity, WfLink] (nextAct) { n => 
    //      n.procState != ProcState.CREATED && ! n.isInstanceOf[SpecOps]
    //    } foreachNode { (n, l) =>
    //      nextAct = n
    //      nextLink
    //    }

    // for now: limitation: skip just one single node which must have just one branch out
    nextAct.synchronized {
      // loops allow DONE actions to be executed again, so we only skip them if SKIPPED as well
      if (nextAct.procState == ProcState.DONE && nextAct.procStatus == ProcStatus.SKIPPED) {
        nextLink = nextAct.glinks.headOption
        razie Debug "skipping (%s) because it's already DONE.SKIPPED, next is: %s".format(nextAct, nextLink.get.z)
        nextAct = nextLink.get.z
      }
    }

    nextAct.synchronized {
      nextLink map (_.linkState = LinkState.DONE)
      currAct = nextAct
      currAct.procState = ProcState.INPROGRESS
    }
  }

  /** post-processing the next activity */
  protected def postProcessing = {
    currAct match {
      case aj: AndJoin => if (aj.isComplete) currAct.procState = ProcState.DONE
      case ca @ _ => currAct.procState = ProcState.DONE
    }
  }

  /**
   * execute current action and advance: return continuations: either myself or a
   *  bunch of spawns
   */
  def execAndAdvance: (Any, Seq[ProcessThread]) = {
    preProcessing
    Audit.recExecBeg(currAct, currV)

    val (out, next) = try {
      currAct.traverse(nextLink, ctx, currV)
    } catch {
      case s: Throwable => {
        // exception - just pick the first link out
        error("Exception while executing WfActivity:", s)
        s.printStackTrace()
        (currV, currAct.glinks.headOption.toList)
      }
    }

    Audit.recExecEnd(currAct, currV, out, next.size)
    currV = out

    next map (_.linkState = LinkState.SELECTED)

    val ret = if (next.size == 1) {
      // continue on this thread with this next activity
      nextLink = next.headOption
      nextAct = next.head.z
      this :: Nil // myself continues
    } else {
      // I'm done in this case and replace myself with these spawns
      nextAct = null
      if (next.size > 1)
        next.map(l => new ProcessThread(parent, l.z, Some(l), ctx, currV))
      else
        Nil // done - no continuations
    }

    postProcessing

    // if i finished, notify my root instance
    if (!currAct.isInstanceOf[AndJoin] || ret.size <= 0)
      parent.threadFinished(this, currV)

    (currV, ret)
  }

  def done(): Boolean = { val b = nextAct == null; debug("done: " + b); b }
}

/**
 * a process instance encapsulates the state of the running workflow
 *
 *  the state needed to recover is: the definition graph, the graph of states with links to specs and
 *  the values context.
 *
 *  I don't think the history should be kept, unless we're running in some sort of "debug" or "audit" mode.
 */
class Process(val start: WfActivity, startV: Any, val ctx: AC) extends Logging {
  val id = GRef.id("WfProcess", GRef.uid)
  var lastV = startV

  // the threads in progress these are all in parallel
  var currThreads: Seq[ProcessThread] = new ProcessThread(this, start, None, ctx, startV) :: Nil
  private[this] var countThreads = 0 // yeah, 0 is correct
  var oldThreads: Seq[ProcessThread] = Nil // don't know why i keep these

  def setThreadCount(i: Int) = synchronized { this.countThreads += i }
  def getThreadCount = this.countThreads

  def threadFinished(t: ProcessThread, v: Any) = {
    synchronized {
      lastV = v
    }
    debug("execAndAdvance() lastV == " + lastV)
  }

  // if ran sync, theaads list is relevant . when async, count is relevant
  def done(): Boolean = synchronized {
    val b = /* currThreads.isEmpty || */ countThreads <= 0;
    //    Debug("done=" + b + " currThreads=" + currThreads.size + " countThreads=" + countThreads); 
    b
  }

  def persist = "" // TODO
  def recover(s: Any) {} // TODO

  /** will suspend execution for further tasks. All sync activities in progress will complete */
  def suspend {
    // TODO
  }

  /** resume execution of a previously suspended process */
  def resume {
    // TODO
  }
}

/**
 * TODO use this
 * execution settings for a single wf instance
 */
class WfSettings(start: WfActivity) {
  var _optimize = true
  var _honourThreads = true

  /** optimize synchronous chains with recursive calls */
  def optimized() = { _optimize = true; this }

  /** honour as much threads as possbile - the engine will try to allocate one thread for EACH parallel branch, rather than using a pool */
  def honourThreads() = { _honourThreads = true; this }
}

/** the engine is more complicated in this case, basically graph traversal */
abstract class Engine extends MsgProcessor with EngineStrategy with Logging {
  override protected val logger = newlog(classOf[Engine])

  /** the processes in progress managed by this engine */
  val processes = new scala.collection.mutable.ListBuffer[Process]()
  var stopped = false

  /** process one tick, advancing one execution path along the way - call from either threads or actors */
  override def pmsg(msg: PMsg, killeable: Killeable) {
    msg match {
      // tick is used just to kickstart all threads
      case StartThreads(p, a) => {
        p.currThreads map pspawn
        val s = p.currThreads.map(Tack(p, _, a)).toList
        p setThreadCount s.size
        s foreach (psend(_))

        if (p.done) a ! Done(p) // how to return the value?
      }
    }
  }

  /** process one tick, advancing one execution path along the way - call from either threads or actors */
  override def ptmsg(msg: PTMsg, killeable: Killeable) {

    def processTack(p: Process, t: ProcessThread, a: Actor) {
      t.currActor = Option(killeable) // TODO Option(this)
      val s = try {
        t.execAndAdvance._2 // this carries out the actual work...
      } catch {
        case _: Throwable => {
          Nil
        }
      }
      t.currActor = None

      // some special activities are handled right here, not in their own traverse()
      t.currAct match {

        case re: WfResReq => { // make the actual request
          val cback = new WResUser {
            override def key = GRef.id("blahblah", "?")
            def notifyReply(reply: WRes.ReqReply) = psend(Reply(p, t, reply, a))
            def notifyScrewup(who: WRes) = {
              psend(Reply(p, t, null, a))
              throw new UnsupportedOperationException("TODO") // TODO
            }
          }

          t.currReq = Some(
            new WRes.Req(cback, re.tok, re.what, re.attrs, re.value(t.ctx, t.currV)))

          debug("ENG.Req: " + t.currReq.get)
          val rep = re.req(t.currReq.get)
          debug("ENG.Req returned: " + rep)
          
          rep match {
            case ok @ Some(WResRROK(w, tok, r)) => psend(Reply(p, t, ok.get, a))
            case er @ Some(WResRRERR(w, tok, e)) => psend(Reply(p, t, er.get, a))
            case wa @ Some(WResRRWAIT(w, tok)) => ; // current action waits
            case None => Audit.recResNotFound(re, re.res)
          }
        }

        //              case co: WfStopOthers => { // cancel others
        //                Debug("ENG.cancelOthers: " + t.currReq.get)
        //                val s = p.currThreads.filter(_ != t).map(StopThread(p, _, a)).toList
        //                s foreach (processor ! _)
        //              }

        // otherwise, it's a normal activity, continue all remaining threads
        case cact @ _ =>
          {
            cact match {
              case co: WfSkip => { // cancel a target
                debug("ENG.skip")

                // try right here, maybe we're lucky - otherwise we may miss this one due to competing threads
                val target = co.target
                target.synchronized { // I don't suppose this is going to get me in trouble, is it?
                  if (target.procState == ProcState.CREATED) {
                    debug("ENG.preempt.skipping: " + target)
                    target.procState = ProcState.DONE
                    target.procStatus = ProcStatus.SKIPPED
                  }
                }

                // send the message anyways...
                psend(Skip(p, t, a, target))
              }
              case _ => ;
            }

            p.synchronized {
              p setThreadCount (s.size - 1) // TODO really?
              p.currThreads = p.currThreads.filter(!s.contains(_)) ++ s
              //                trace(">>> new waveline: " + p.currThreads.map(tt => Option(tt.currAct).map(_.key).getOrElse("?")) + " - threadCount = " + p.getThreadCount)
              debug(">>> new waveline: %s - threadCount = %s".format(
                p.currThreads.map(tt => Option(tt.currAct).map(_.key).getOrElse("?")),
                p.getThreadCount))
              // TODO err if first action on only thread is REQ then it will finish before reply
              if (p.done) {
                trace("DOOOOOOOOOOOOOOOOOOONE");
                a ! Done(p)
                assert(s.size == 0)
              } // how to return the value?
            }
          }
          s filter (_ != t) map (pspawn(_))
          s map (ptt => {
            val optimize = false
            if (s.size == 1 && optimize) // sync optimization: recurse rather than async
              processTack(p, ptt, a)
            else psend(Tack(p, ptt, a))
          })
      }
    } // processTack

    msg match {
      case Tack(p, t, a) => processTack(p, t, a)

      // reply from resource - continue original thread
      case Reply(p, t, rr, a) => {
        debug("ENG.Reply: " + rr)
        if (t.done)
          warn("WfResReq DUPLICATE - the wfthread is done... " + rr)
        else t.nextAct match {
          case re: WfResReply => {
            t.currReq = None
            re.reply(rr)
            psend(Tack(p, t, a))
          }
          case s @ _ => throw new IllegalStateException("WfResReq MUST be followed by a WfResReply, not a " + s)
        }

        //         if (p.done) a ! Done(p) // how to return the value?
      }

      case Skip(p, _, a, target) => {
        debug("ENG.skipping: " + target)
        // TODO 2-2 identify target thread only and lock that one
        target.synchronized {
          trace(">>>>>>>>>>>>>>>>1.killing " + target.procState)
          target.procState match {
            case ProcState.DONE => trace("--already DONE")
            // TODO 1-2 find thread and kill it
            case x @ ProcState.INPROGRESS => {
              target.procStatus = ProcStatus.SKIPPED
              val tokill = p.currThreads.filter(kk => {
                trace(">>>>>>>>>>>>>>>>2.killing " + kk.currAct.key + "-" + target.key)
                kk.currAct.key == target.key
              }).headOption
              trace(">>>>>>>>>>>>>>>>3.killing " + tokill)
              tokill map (_.currActor map (_.kill()))
            }
            case ProcState.CREATED => {
              target.procState = ProcState.DONE
              target.procStatus = ProcStatus.SKIPPED
            }
            case s @ _ => throw new IllegalStateException("todo: for state: " + s)
          }

          //            t.nextAct match {
          //             case re: WfResReply =>  // was waiting for reply
          // TODO should cancel the request with the resource
          //           }

          //         if (p.done) a ! Done(p) // how to return the value?
        }
      }
    }
  }

  def create(start: WfActivity, startV: Any, ctx: AC = razie.base.scripting.ScriptFactory.mkContext("scala", null)): GRef = {
    val p = new Process(wf.scope(start), startV, ctx)
    engSync { processes += p }

    Audit.recCreate(p)
    p.id
  }

  def start(id: GRef, startV: Any): GRef = {
    val t = istart(id, startV, true)
    t._2 ! Start(t._1)
    t._1.id
  }

  //todo how to sync on super in inner class?
  private def engSync[T](f: => T): T = synchronized { f }

  private[this] def istart(id: GRef, startV: Any, async: Boolean): (Process, Actor) = {
    val p = processes.find(_.id == id) getOrElse (throw new IllegalArgumentException("Process not found id: " + id))
    p.lastV = startV
    Audit.recStart(p, startV)

    val me: Actor = new Actor {
      override def act() = receive {
        case Start(p) => {
          preProcess(p)
          pspawn(p)
          psend(StartThreads(p, this))
          receive {
            case Done(p) => {
              Audit.recDone(p, p.lastV)
              engSync { processes -= p } // maybe keep it around?
            }
          }
          reply()
        }
      }
    }

    me.start

    (p, me)
  }

  /** launch a workflow, wait and return the result. AVOID using this method except for tests and demos */
  def exec(start: WfActivity, startV: Any, ctx: AC = razie.base.scripting.ScriptFactory.mkContext("scala", null)): Any = {
    val t = istart(create(start, startV, ctx), startV, false)

    t._2 !? Start(t._1) // just wait, discard reply

    t._1.lastV
  }

  /**
   * start a workflow instance and do not wait for result
   *
   *  TODO start should return a Future/Promise ?
   */
  def start(start: WfActivity, startV: Any, ctx: AC = razie.base.scripting.ScriptFactory.mkContext("scala", null)): Unit = {
    val t = istart(create(start, startV, ctx), startV, false)
    t._2 ! Start(t._1) // just start, no wait
  }

  /** TODO complete this sync execution - good for debugging or something...right now it's missing async resources */
  def execSync(start: WfActivity, startV: Any, ctx: AC = razie.base.scripting.ScriptFactory.mkContext("scala", null)): Any = {
    val p = new Process(start, startV, ctx)

    preProcess(p)

    // progress all threads - this is temp until I get the threading done
    while (!p.done)
      p.currThreads = p.currThreads.flatMap(_.execAndAdvance._2)

    p.lastV
  }

  private def preProcess(p: Process) {
    // cache z ends to AndJoins
    // TODO I use dag which is not the most efficient way to do this
    razie.g.Graphs.entire[WfActivity, WfLink](p.start).dag.foreach(
      (n: WfActivity, v: Int) => {},
      (l: WfLink, v: Int) => l.z match {
        case a: AndJoin => a addIncoming l;
        case _ =>
      })
  }

  def checkpoint() {}

  /**
   * stop the engine
   *
   *  @param forced if true, the processes will be killed
   *  @param timeout how long to wait in case anyone is still running - it will sleep in 250ms chunks so not to waste your time
   */
  def stop(forced: Boolean = false, timeout: Int = 250) = {
    // wait for all to be done, sleeping in CHUNKs up to a maximum
    val CHUNK = 250
    var spent = 0
    while (synchronized { !processes.isEmpty } && spent < timeout) {
      Thread.sleep(CHUNK)
      spent += CHUNK
    }

    var ok = true

    synchronized {

      // try to kill what's left
      if (!processes.isEmpty) {
        if (forced) {
          error("there are still " + processes.size + " Processes in progress")
          val tokill = processes.flatMap(_.currThreads)
          warn(">>>>>>>>>>>>>>>>3.killing " + tokill)
          tokill map (_.currActor map (_.kill()))
          ok = false
        } else
          throw new IllegalStateException("there are still " + processes.size + " Processes in progress")
      }

      stopped = true
      pexit()
      ok
    }
  }
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

/**
 * special engine activity to skip another. The function will find the target
 *  dynamically, starting from me... see the cancel() construct
 */
class WfSkip(private val findTarget: WfActivity => WfActivity) extends WfActivity with HasDsl with SpecOps {
  def target = findTarget(this)
  override def traverse(in: AC, v: Any): (Any, Seq[WfLink]) = (v, glinks)
  // TODO 1-1
  //  override def toDsl = "skip " + target().key
  override def toDsl = throw new UnsupportedOperationException()
  override def toString: String = "WfSkip (" + key + ") target: " + target.key
}

/** stop the current branch after the given number of passes */
class WfStop(val pass: Int) extends WfActivity /*with HasDsl*/ with SpecOps {
  var currPass = 0

  override def traverse(in: AC, v: Any): (Any, Seq[WfLink]) = {
    currPass += 1
    if (currPass >= pass) {
      val m = "STOPING: WfeStoppped the curent branc at pass: " + pass + " by " + this.toString
      razie.Warn(m)
      (v, Nil)
    } else
      (v, glinks)
  }

  // TODO from DSL
  //  override def wname = "stop"
  //  override def toDsl = "stop " + pass
}

