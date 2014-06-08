/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins.eng

import razie.{ AA, Debug, Log }
import razie.base.{ ActionContext => AC }
import razie.g._
import scala.actors._
import com.razie.pubstage.life._
import razie.gremlins._
import razie.gremlins.act._
import razie.wf
import com.razie.pub.base.ExecutionContext

//-------------------- graph processing engine - executing Strategies

/**
 * the engine processes activities one tick/tack at a time
 *
 * the idea is that in a multi-threaded system, you need basic units of work to be bounced around,
 * triggering executions, such as message in an actor system
 */
trait MsgProcessor {
  /** process-related message */
  def pmsg(msg: PMsg, killeable: Killeable): Unit
  /** process-thread related message */
  def ptmsg(msg: PTMsg, killeable: Killeable): Unit
}

/** an active something that can be killed */
trait Killeable { def kill() }

/** the primitives for execution control: threading, you can have then threads or actors or whathaveyou */
trait EngineStrategy {

  /** send new message (work unit) to continue processing */
  def psend(msg: PEMsg): Unit

  /** spawn new execution unit - the strategy will retain the mapping of what the unit is for and select it when needed 
   *  
   *  the default does nothing as most implementation handle it in psend()
   */
  def pspawn(pt: Process): Unit = {}
  def pspawn(pt: ProcessThread): Unit = {}

  /** done - kill all threads/actors/executors/whatnot */
  def pexit(): Unit
}

/** use plain threads with "executor" pattern */
trait Threads extends EngineStrategy { this: MsgProcessor =>
  lazy val mtp = new com.razie.pubstage.life.WorkerTP(POOL_SIZE)

  /** override to set custom thread pool size */
  def POOL_SIZE = 10

  override def psend(msg: PEMsg) { mtp.put(new Processor(msg)) }

  class Processor(msg: PEMsg) extends com.razie.pubstage.life.Worker(
    razie.AI("EngineProcessor"),
    ExecutionContext.DFLT_CTX) with Killeable {
    
    override def process() = msg match {
      case p: PMsg => pmsg(p, this)
      case t: PTMsg => ptmsg(t, this)
    }
    
    override def kill() = mtp kill this
  }

  override def pexit(): Unit = {}
}

/** use mulitple actors: each process is an actor and each branch in a process is another actor */
trait Actors extends EngineStrategy { this: MsgProcessor =>
  import Actor._

  /** current actors for this engine */
  var actors = new scala.collection.mutable.HashMap[Any, Actor]()
  var maxActors = 0 // stats: keep track of how many actors were ever used at the same time

  private def dies(k: Any) = synchronized {
    actors remove k
  }

  private def lives(k: Any, a: Actor) = synchronized {
    actors put (k, a);
    if (actors.size > maxActors)
      maxActors = actors.size
  }

  private def find(k: Any): Option[Actor] = synchronized { actors get k }

  /** spawn new execution unit for given pthread */
  override def pspawn(pt: ProcessThread): Unit = {
    val a =
      actor {
        loop {
          react {
            case Exit => { dies(pt); exit() }
            case msg: PTMsg => ptmsg(msg, null)
            case m @ _ => throw new IllegalArgumentException("CANT process but t ticks " + m)
          }
        }
      }
    lives(pt, a)
  }

  /** spawn new execution unit for given process */
  override def pspawn(p: Process): Unit = {
    val a =
      actor {
        loop {
          react {
            case Exit => { dies(p); exit() }
            case msg: PMsg => pmsg(msg, null)
            case m @ _ => throw new IllegalArgumentException("CANT process but p ticks " + m)
          }
        }
      }
    lives(p, a)
  }

  override def psend(msg: PEMsg) = msg match {
    case p: PMsg => find(p.p) map (_ ! msg)
    case pt: PTMsg => find(pt.t) map (_ ! msg)
  }

  override def pexit(): Unit = { actors.values map (_ ! Exit) }
}

/** use actors with "executor" pattern */
trait Executors extends EngineStrategy { this: MsgProcessor =>
  import Actor._

  val processors: Array[Actor] = Array.fill(10) {
    actor {
      loop {
        react {
          case Exit => exit()
          case p: PMsg => pmsg(p, null)
          case pt: PTMsg => ptmsg(pt, null)
        }
      }
      //    override def kill() = mtp kill this
    }
  }

  def psend(msg: PEMsg) { processor ! msg }

  // load balancer
  val processor: Actor = actor {
    var i = 0
    loop {
      react {
        case Exit => {
          processors foreach (_ ! Exit)
          exit();
        }
        case m @ _ => {
          processors(i) ! m;
          i = (i + 1) % 5
        }
      }
    }
  }

  def pexit(): Unit = { processor ! Exit }
}

// Process-related messages
trait PEMsg // marker for engine messages
trait PMsg extends PEMsg { def p: Process }
/** start a process - will result in a StartThreads */
case class Start(p: Process) extends PMsg
/** first message of a process results in starting each of the PT threads */
case class StartThreads(p: Process, result: Actor) extends PMsg
case class Done(p: Process) extends PMsg

// ProcessThread-related messages
trait PTMsg extends PEMsg { def p: Process; def t: ProcessThread }
case class Tack(p: Process, t: ProcessThread, result: Actor) extends PTMsg
case class Reply(p: Process, t: ProcessThread, rr: WRes.ReqReply, result: Actor) extends PTMsg
case class Skip(p: Process, t: ProcessThread, result: Actor, target: WfActivity) extends PTMsg

// guess who?
case class Exit()
