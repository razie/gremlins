/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.snakked

import razie.AA
import razie.g
import razie.base

class ParmDef(name: String, ttype: Manifest[_])

object Parms {
  def parm[T](name: String)(implicit ev: Manifest[T]): ParmDef = new ParmDef(name, ev)

  def defn(parms: ParmDef*) = parms
}

object SampleParms {
  import Parms._

  val person =
    parm[String]("first") ::
      parm[String]("last") :: Nil

  val alsoperson = AA("first,last")

  //  val person2 = classOf[case class GGG23(first:String, last:String)]
}

//-------------------------- sync/async

trait SAKey

/** a synchronous operation. As a client, you can simply invoke this method */
trait Sync[T <: Any] extends F {
  def apply(in: Any, ctx: AA): T
}

/** an asynchronous operations is split in two: send and receive. Semantics: you know there's no reply */
trait Notifier extends F {
  /** just fire off an event - no replies ever expected */
  def notify(in: Any, ctx: AA)
}

/** an asynchronous operations is split in two: send and receive */
trait Sender[T <: Any] extends F {
  /** actual send functionality. client's can't call this */
  protected def sendImpl(in: Any, ctx: AA)

  /** asynchronous send where the receiver is given. The receiving infrastructure is hidden behind SAMap */
  def send(in: Any, ctx: AA)(m: SAMap, k: SAKey, r: Receiver[T]) {
    m.put(k, ctx, r)
    sendImpl(in, ctx)
  }
}

/** an asynchronous operations is split in two: send and receive */
trait Receiver[T <: Any] extends F {
  // TODO should the receiver get the original context? It's harder so I'll write it like this for now
  def recv(out: T, ctx: AA): Unit
}

trait Async[T <: Any] extends Sender[T] with Receiver[T]

trait SyncAsync {
  def isAsync: Boolean
}

// TODO synchronize this 
class SAMap {
  val m = new scala.collection.mutable.HashMap[SAKey, (AA, Receiver[_])]()

  def put(k: SAKey, ctx: AA, r: Receiver[_]): Unit = m put (k, (ctx, r))
  def recv[T <: Any](k: SAKey, out: T): Unit =
    m get k foreach { x => x._2.asInstanceOf[Receiver[T]].recv(out, x._1) }
}


/** the basis of the async patterns */
class asyncpattern {
  /** the actual logic - tends to be synchronous at some basic level */
  private[this] def inc (a:Int) = a+1

  /**
   * send the message with callback - note that the receive could be spawned in separate thread
   * to complicate things*/
  def send (a:Int) (receive: Int => Unit) { receive (inc(a)) }

  val MSG = 1
  def doRest (result:Int) = println ("Got result: " + result)
}

/**
 * async 1 - stupidest sleep/pooll
 * pros: easy to distribute
 * cons: consumes resources (thread, transaction, connections etc)
 * cons: well, it's just stupid :)
 */
object async1 extends asyncpattern {
  var reply = 0

  send (MSG) { reply = _ }

  while (reply == 0) java.lang.Thread.sleep(10)

  doRest (reply)
}

/**
 * async 2 - simple wait/wake as in Java...the implementation is not the point here
 * pros: implementations are usually smart (TCP)
 * cons: consumes resources (thread, transaction, connections etc)
 */
object async2 extends asyncpattern {
  // some wait/wakt implementation
  object monitor {
    var done = false
    // note that THIS implementation doesn't have problems if wake before sleep...
    def waitHere() = while(!done) java.lang.Thread.sleep(10) 
    def wake() = done=true
  }

  var reply = 0

  send (MSG) { result => { reply = result ; monitor.wake() }}

  monitor.waitHere() 

  doRest (reply)
}

/**
 * async 3 - stateful callback. the point here is passing the state to the async framework,
 * which invokes it later
 * pros: true asynchronous programming: decouple processing steps
 * cons: hard to distribute
 */
object async3 extends asyncpattern {
  val state = new Object {
    def continue (result:Int) { doRest (result) }
  }

  send (MSG) { state continue _ }
}

/** async 4 - continuation - it's the same as above but differnet form */
object async4 extends asyncpattern {
  // TODO
}

/**
 * async 5 - stateless callback
 * pros: true asynchronous
 * cons: TODO
 */
object async5 extends asyncpattern {
}

//-------------------------- actionable

trait F {}

class C extends AA
trait E extends g.GReferenceable with base.AttrAccess
trait A extends E

trait EDef {
  def args: AA
}

trait ADef extends EDef

trait EMap[T <: E] {
  def apply(e: T, ctx: AA): Seq[T]
}

trait AMap[T <: A] extends EMap[T]

class ParmMapping(val from: ParmDef, val to: ParmDef)
case class PM(from: ParmDef, to: ParmDef)

//------------------------- play

/** map a parm to another parm: dest -> source */

trait Mapping {
  def apply(edest: E, esrc: E, c: C)
}

trait MappingDefn {
  implicit def c(s: String) = Const(s)
  def p(s: String) = PSP(s)
  def e(s: String) = Expr(s)
}

abstract class ParmSource extends F
case class Const(s: String) extends ParmSource with Sync[String] {
  override def apply(in: Any, c: AA): String = { s }
}
case class PSP(s: String) extends ParmSource with Sync[String] {
  override def apply(in: Any, c: AA): String = { in.asInstanceOf[base.AttrAccess].sa(s) }
}
case class Expr(s: String) extends ParmSource with Sync[Any] {
  override def apply(in: Any, c: AA): Any = { 1 }
}
//case class AsyncExpr(s: String) extends ParmSource with Async[Any] {
  //override def sendImple(in: Any, c: AA) {}
//}

class PMap(m: Map[String, ParmSource]) extends Mapping {
  def isAsync = m.values.foldLeft(true)((a, b) => a && (b.isInstanceOf[Sync[_]] || b.isInstanceOf[Notifier]))
  override def apply(edest: E, esrc: E, c: C) {
    val isa = isAsync
    m foreach (t => t._2 match {
      case s: Sync[_] => edest.set(t._1, s.apply(esrc, c).asInstanceOf[AnyRef])
      case a: Async[_] => {
        if (!isa) throw new IllegalStateException("I was supposed to be sync...but I'm not???")
        //edest.set(t._1, a.send(esrc, c).asInstanceOf[AnyRef])
        throw new UnsupportedOperationException("TODO")
      }
      case n: Notifier => (t._1, n.notify(esrc, c))
    })
  }

}

object SampleMapping extends Application with MappingDefn {
  val p1 = new PMap(Map("a" -> "val1", "b" -> "val2"))
}

