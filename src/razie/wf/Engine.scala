/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.{AA, Debug, Log}
import razie.base.{ActionContext => AC}
import razie.g._
import scala.actors._

//-------------------- graph processing engine

object Audit {
  private def audit (aa:AA) = razie.Audit (aa)

  def recCreate   (a:Process) = audit (razie.AA("Event", "CREATE", "process", a))
  def recStart    (a:Process, v:Any) = audit (razie.AA("Event", "START", "process", a, "value", v.asInstanceOf[AnyRef]))
  def recDone     (a:Process, v:Any) = audit (razie.AA("Event", "DONE", "process", a, "value", v.asInstanceOf[AnyRef]))
  def recExecBeg  (a:WfAct, in:Any) =
     audit (razie.AA("Event", "EXEC.beg", "activity", a, "in", in.asInstanceOf[AnyRef]))
  def recExecEnd  (a:WfAct, in:Any, out:Any, paths:Int) =
     audit (razie.AA("Event", "EXEC.end", "activity", a, "in", in.asInstanceOf[AnyRef], "out",out.asInstanceOf[AnyRef], "paths", paths))
  def recResNotFound  (a:WfAct, res:GRef) =
     audit (razie.AA("Event", "ERROR.resNotFound", "activity", a, "res", res))
}

trait WfControl {
  def waiting (r:WRes, rr:WResReqReply)
}

/** 
 * a process thread encapsulates the state of a running workflow branch . You can see this as an 
 * actor as well. Depending on the engine, each PT may have its own processor, thread or whatever...
 * conceptually these are independent paths of execution, for the PAR/JOIN branches for isntance
 */
class ProcessThread (val parent:Process, val start:WfAct, val startLink:Option[WL], val ctx:AC, val startValue:Any) { 
  // should i keep state inside the activities or outside in a parallel graph, built as it's traversed?

  var currV:Any = startValue
     
  // the activity in progress
  var currAct : WfAct = null
 
  // current resource waiting for...if any
  var currReq : Option[WResReq] = None
    
  // the activities to start on the next step
  var nextAct   = start 
  var nextLink  = startLink
   
  protected def preProcessing = {
    currAct = nextAct
    currAct.procState = ProcState.INPROGRESS 
    nextLink map (_.linkState = LinkState.DONE)
  }
    
  protected def postProcessing = {
    currAct.procState = ProcState.DONE 
  }
    
  // advance to next action and return if there's more threads to spawn
  def move : (Any,Seq[ProcessThread]) = {
    preProcessing

    // execute action and return continuations: either myself or a bunch of spawns
    
    Audit.recExecBeg  (currAct, currV)
    val (out, next) = currAct.traverse(nextLink, ctx, currV)
    Audit.recExecEnd  (currAct, currV, out, next.size)
    currV = out

    next map (_.linkState = LinkState.SELECTED)
    
    val ret = if (next.size == 1) {
      nextLink = next.firstOption
      nextAct = next.first.z
      this :: Nil // myself continues
    } else {
      nextAct = null 
      if (next.size > 1) 
         next.map (l=>new ProcessThread(parent, l.z, Some(l), ctx, currV)) // // TODO what value to use for start here?
        // note that I'm done in this case and replace myself with these spawns
      else 
        Nil // done - no continuations
    }
    
    postProcessing
    (currV,ret)
  }
    
  def done () : Boolean = { val b = nextAct == null; Debug ("done: " + b); b}
}

/** 
 * a process instance encapsulates the state of the running workflow 
 * 
 * the state needed to recover is: the definition graph, the graph of states with links to specs and 
 * the values context.
 * 
 * I don't think the history should be kept, unless we're running in some sort of "debug" or "audit" mode.
 */
class Process (val start:WfAct, val ctx:AC, startV:Any) { 
  val id =  GRef.id("WfProcess", GRef.uid)
  var lastV = startV
    
  // the threads in progress these are all in parallel
  var currThreads : Seq[ProcessThread] = new ProcessThread (this, start, None, ctx, startV) :: Nil
  private[this]var countThreads = 0 // yeah, 0 is correct
  var oldThreads : Seq[ProcessThread] = Nil // don't know why i keep these

  def countThread (i:Int) = synchronized { this.countThreads += i }
  
  // progress one thread - this would normally happen on its own thread
  def tick (t:ProcessThread) : Seq[ProcessThread] = {
    val (v,n) = t.move
    synchronized {
    if (! t.currAct.isInstanceOf[AndJoin] || n.size <= 0)
      lastV = v
    }
    Debug ("tick() lastV == " + lastV)
    n
  }

  // if ran sync, theaads list is relevant . when async, count is relevant
  def done () : Boolean = synchronized { val b = /*currThreads.isEmpty ||*/ countThreads <= 0 ; Debug ("done="+b+" currThreads="+currThreads.size+" countThreads="+countThreads); b }
  
  def persist = "" // TODO
  def recover (s:Any) {} // TODO
}

/** the engine is more complicated in this case, basically graph traversal */
class Engine { 

  import Actor._

  val processes = new scala.collection.mutable.ListBuffer[Process]()
  var stopped = false
  
  // load balancer
  val processor : Actor = actor {
    var i = 0
    loop { 
       react {
         case Exit => {
            processors foreach (_ ! Exit)
            exit (); 
         }
         case m @ _=> { 
            processors(i) ! m; 
            i = (i+1) % 5 
         }
    }}
  }
    
  val processors : Array[Actor] = Array.fill (5) {actor {
    loop { react {
       case Tick(p,a) => {
         val s = p.currThreads.map(Tack(p, _, a)).toList
         p countThread s.size
         s foreach (processor ! _)
               
         if (p.done) a ! Done(p) // how to return the value?
       }
       
       case Tack(p,t,a) => {
         val s = p.tick (t)
         
         t.currAct match {

            case re : WfResReq => { // make the actual request
              val cback = new WResUser {
                override def key = GRef.id ("blahblah", "?")
                def notifyAcquired (reply:WResReqReply) = processor ! Reply (p, t, reply, a) 
                def notifyReply (reply:WResReqReply) = processor ! Reply (p, t, reply, a)
                def notifyScrewup (who:WRes) = {
                   processor ! Reply (p, t, null, a)
                   throw new UnsupportedOperationException ("TODO") // TODO
                }
                }
              
              t.currReq = Some(new WResReq (cback, "?", re.what, re.attrs, re.value(t.ctx, t.currV)))
              
              Debug ("ENG.Req: "+t.currReq.get)
              val rep = re.req(t.currReq.get) 
              Debug ("ENG.Req returned: "+rep)
              rep match {
                 case ok@Some(WResRROK (w,tok,r)) => processor ! Reply (p,t,ok.get,a)
                 case er@Some(WResRRERR (w,tok,e)) => processor ! Reply (p,t,er.get,a)
                 case wa@Some(WResRRWAIT (w,tok)) =>  ;// current action waits
                 case None => Audit.recResNotFound (re, re.res)
              }
            }
            
            // otherwise, normal activity, continue all remaining threads
            case _ => {
               p.synchronized {
                 p countThread (s.size-1)
                 // TODO err if first action on only thread is REQ then it will finish before reply
                 if (p.done) { 
                   Debug ("DOOOOOOOOOOOOOOOOOOONE"); 
                   a ! Done(p) 
                   assert (s.size == 0)
                   } // how to return the value?
                 } 
               }
               s map (processor ! Tack(p, _, a))
            }
         } // case Tack
      
       // retply from resource - continue original thread
       case Reply(p,t,rr,a) => {
         Debug ("ENG.Reply: "+rr)
         t.nextAct match {
            case re:WfResReply => {
              t.currReq = None
              re.reply (rr)
              processor ! Tack(p, t, a)
            }
            case s@_ => throw new IllegalStateException ("WfResReq MUST be followed by a WfResReply, not a " + s)
         }
          
//         if (p.done) a ! Done(p) // how to return the value?
       }
       
       case Exit => exit ()
    }}
  }
  }
    
  case class Start (p:Process)
  case class Tick (p:Process, result:Actor)
  case class Tack (p:Process, t:ProcessThread, result:Actor) 
  case class Reply (p:Process, t:ProcessThread, rr:WResReqReply, result:Actor)
  case class Pre (p:Process)
  case class Done (p:Process)
  case class Exit 

  def create (start:WfAct, ctx:AC, startV : Any) : GRef = {
    import wf._
    val p = new Process (wf.scope(start), ctx, startV)       
    processes += p

    Audit.recCreate (p)
    p.id
  }
    
  def start (id:GRef, startV:Any) : GRef = {
    val t = istart (id, startV, true)
    t._2 ! Start(t._1)
    t._1.id
  }
  
  private[this] def istart (id:GRef, startV:Any, async:Boolean) : (Process, Actor) = {
    val p = processes.find(_.id == id) getOrElse (throw new IllegalArgumentException ("Process not found id: "+id))
    p.lastV = startV
    Audit.recStart (p, startV)
    
    val me : Actor = new Actor {
      override def act () = receive {
       case Start(p) => {
          preProcess(p)
          processor ! Tick (p, this)
          receive { 
            case Done(p) => {
               Audit.recDone (p, p.lastV)
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
  
  def exec (start:WfAct, ctx:AC, startV:Any) : Any = {
    // wrap into begin/end
    import wf._
    
    val t = istart(create(start, ctx, startV), startV, false)
    
    t._2 !? Start(t._1) // just wait, discard reply

    t._1.lastV
  }

  /** TODO complete this sync execution - good for debugging or something...right now it's missing async resources */
  def execSync (start:WfAct, ctx:AC, startV:Any) : Any = {
    val p = new Process (start, ctx, startV)       

    preProcess (p)
    
    // progress all threads - this is temp until I get the threading done
    while (!p.done) 
      { p.currThreads = p.currThreads.flatMap(p.tick(_)) }
      
    p.lastV 
  }
  
  def preProcess (p:Process) {
    // cache z ends to AndJoins
    razie.g.Graphs.foreach (p.start, 
      (l:WfAct,v:Int)=>{},
      (l:WL,v:Int) => l.z match { case a:AndJoin => a addIncoming l ; case _ => }
      )
  }
     
  def checkpoint () {}
  
  def stop = synchronized {
     if (! processes.isEmpty) 
       throw new IllegalStateException ("there are still Processes in progress")

     stopped = true
     processor ! Exit
  }
}

// TODO implement a nice pool of engines
object Engines {
  var dflt : Option[Engine] = None
  def stop = synchronized { dflt map (_.stop); dflt = None }
  def start = synchronized { stop; dflt = Some(new Engine()); dflt.get }
  def apply () = synchronized { val e = (dflt getOrElse start); if (e.stopped) start else e }
}

abstract class ProcessWaitingOnRes (eng:Engine, who:ProcessThread, res:WRes) extends WResUser {
  /** an acquire finished */
  override def notifyAcquired (reply:WResReqReply) = {}
  
  /** a wait finished */
  override def notifyReply (reply:WResReqReply) = {}

  /** the resource screwed up - all outstanding requests were dismissed */
  override def notifyScrewup (who:WRes) = {}
}
  
/** the special activities that control the engine itself - hopefully as few as possible */
object wfeng {
  // TODO start a new process
  def process (root:WfAct) : WfAct = wf.todo
  // TODO start a new sub-process of the current process
  def subprocess (root:WfAct) : WfAct = wf.todo
  
  // join parallel branches - it's basically a barrier: when all branches arrive here, it will continue
  def andjoin (a:WfAct*) : WfAct = wf.todo
}

// and-join state: track incoming links and their values
trait AJState extends WfaState {
  var prev : scala.collection.mutable.HashMap[WL, Any] = null
  
  def addLink (l:WL, v:Any) = {
    if (prev == null)
       prev = razie.Mapi[WL, Any] ()
    prev.put (l, v)
  }
}

/** join parallel branches */
case class AndJoin extends WfAct with AJState { 
  var incoming : List[WL] = Nil // upon starting, the engine will traverse and populate this
  def addIncoming (l:WL) = incoming = List(l) ::: incoming 
  
  /** waiting for all incoming branches and concatenating their values */
  override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = 
     throw new IllegalStateException ("method should never be called")
  
  override def traverse (from:Option[WL], in:AC, v:Any) : (Any,Seq[WL]) = this.synchronized {
    addLink (from.get, v) // TODO null should bomb indicating andjoin with no branches in...
    Debug ("AndJoin " + "prev=" + prev.size + " incoming="+incoming.size)
    if (prev.size == incoming.size) (prev.values.map(x => x).toList, glinks) // I'm done
    else (v, Nil) // current thread dies
  }
  
  override def toString = "AndJoin " + incoming.size 
}
