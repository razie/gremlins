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

//-------------------------- actionable

trait F {
  def isAsync: Boolean
}

trait Sync extends F {
  def apply(in: Any, ctx: AA): Any
  override def isAsync = false
}

// don't like
trait Async extends F {
  def send(in: Any, ctx: AA)
  def recv(out: Any): Any
  override def isAsync = true
}

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
case class Const(s: String) extends ParmSource with Sync {
  override def apply(in: Any, c: AA): Any = { s }
}
case class PSP(s: String) extends ParmSource with Sync {
  override def apply(in: Any, c: AA): Any = { in.asInstanceOf[base.AttrAccess].sa(s) }
}
case class Expr(s: String) extends ParmSource with Sync {
  override def apply(in: Any, c: AA): Any = { 1 }
}
case class AsyncExpr(s: String) extends ParmSource with Async {
  override def send(in: Any, c: AA) {}
  override def recv(out: Any): Any = { 1 }
}

class PMap(m: Map[String, ParmSource]) extends Mapping {
  def isAsync = m.values.foldLeft(true)((a, b) => a && b.isAsync)
  override def apply(edest: E, esrc: E, c: C) {
    if (!isAsync)
      m foreach (t => edest.set(t._1, t._2.asInstanceOf[Sync].apply(esrc, c).asInstanceOf[AnyRef]))
    else
      m foreach (t => t._2 match {
        case s: Sync => edest.set(t._1, s.apply(esrc, c).asInstanceOf[AnyRef])
        case a: Async => edest.set(t._1, a.send(esrc, c).asInstanceOf[AnyRef])
      })
  }

}

object SampleMapping extends Application with MappingDefn {
  val p1 = new PMap(Map("a" -> "val1", "b" -> "val2"))
}

