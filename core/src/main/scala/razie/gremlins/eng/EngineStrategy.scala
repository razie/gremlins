/** ____    __    ____  ____  ____,,___     ____  __  __  ____
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

//-------------------- graph processing engine - executing Strategies

trait Killeable { def kill() }

/** stands for those that actually process the messages - the engine actually */
trait Doer {
  def processpTick(msg: PMsg, killeable: Killeable): Unit
  def processtTack(msg: PTMsg, killeable: Killeable): Unit
  def processTick(msg: Any, killeable: Killeable): Unit = msg match {
    case p: PMsg  => processpTick(p, killeable)
    case t: PTMsg => processtTack(t, killeable)
    case m @ _    => throw new IllegalArgumentException("CANT process but p and t ticks " + m)
  }
}

/** the primitives for execution control: threading */
trait EngineStrategy { this: Doer =>

  /** send new message (work unit) to continue processing */
  def psend(msg: Any): Unit

  /** spawn new execution unit (used by Actors) */
  def pspawn(pt: ProcessThread): Unit = {}
  def pspawn(pt: Process): Unit = {}

  /** done - kill all threads/actors/executors/whatnot */
  def pexit(): Unit
}

/** use plain threads with "executor" pattern */
trait Threads extends EngineStrategy { this: Doer =>
  lazy val mtp = new com.razie.pubstage.life.WorkerTP(POOL_SIZE)

  /** override to set custom thread pool size */
  def POOL_SIZE = 10

  def psend(msg: Any) { mtp.put(new Processor(msg)) }

  class Processor(msg: Any) extends com.razie.pubstage.life.Worker(razie.AI("EngineProcessor"),
    com.razie.pub.base.ExecutionContext.DFLT_CTX) with Killeable {
    override def process() = processTick(msg, this)
    override def kill() = mtp kill this
  }

  override def pexit(): Unit = {}
}

/** use mulitple actors: each process is an actor and each branch in a process is another actor */
trait Actors extends EngineStrategy { this: Doer =>
  import Actor._

  var actors = new scala.collection.mutable.HashMap[Any, Actor]()
  var maxActors = 0

  private def dies(k: Any) = synchronized { actors remove k }
  private def lives(k: Any, a: Actor) = synchronized { actors put (k, a); if (actors.size > maxActors) maxActors = actors.size }
  private def find(k: Any): Option[Actor] = synchronized { actors get k }

  /** spawn new execution unit (used by Actors) */
  override def pspawn(pt: ProcessThread): Unit = {
    val a =
      actor {
        loop {
          react {
            case Exit       => { dies(pt); exit() }
            case msg: PTMsg => processtTack(msg, null)
            case m @ _      => throw new IllegalArgumentException("CANT process but t ticks " + m)
          }
        }
        //    override def kill() = mtp kill this
      }
    lives(pt, a)
  }

  /** spawn new execution unit (used by Actors) */
  override def pspawn(p: Process): Unit = {
    val a =
      actor {
        loop {
          react {
            case Exit      => { dies(p); exit() }
            case msg: PMsg => processpTick(msg, null)
            case m @ _     => throw new IllegalArgumentException("CANT process but p ticks " + m)
          }
        }
        //    override def kill() = mtp kill this
      }
    lives(p, a)
  }

  def psend(msg: Any) = msg match {
    case p: PMsg   => find(p.p) map (_ ! msg)
    case pt: PTMsg => find(pt.t) map (_ ! msg)
    case m @ _     => throw new IllegalArgumentException("CANT process but p and t ticks " + m)
  }

  override def pexit(): Unit = { actors.values map (_ ! Exit) }
}

/** use actors with "executor" pattern */
trait Executors extends EngineStrategy { this: Doer =>
  import Actor._

  val processors: Array[Actor] = Array.fill(10) {
    actor {
      loop {
        react {
          case Exit    => exit()
          case msg @ _ => processTick(msg, null)
        }
      }
      //    override def kill() = mtp kill this
    }
  }

  def psend(msg: Any) { processor ! msg }

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
trait PMsg { def p: Process }
case class Start(p: Process) extends PMsg
case class Tick(p: Process, result: Actor) extends PMsg
case class Pre(p: Process) extends PMsg
case class Done(p: Process) extends PMsg

// ProcessThread-related messages
trait PTMsg { def p: Process; def t: ProcessThread }
case class Tack(p: Process, t: ProcessThread, result: Actor) extends PTMsg
case class Reply(p: Process, t: ProcessThread, rr: WRes.ReqReply, result: Actor) extends PTMsg
case class Skip(p: Process, t: ProcessThread, result: Actor, target: WfActivity) extends PTMsg

// guess who?
case class Exit()
