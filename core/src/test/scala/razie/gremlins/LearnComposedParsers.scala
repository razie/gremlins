package razie.gremlins

import scala.util.parsing.combinator._

class Act (val s:String)

trait WBase extends JavaTokenParsers {
  def activities() : WBase#Parser[Act]
}

class Lib1 extends WBase {
  override def activities() : WBase#Parser[Act] = a1
  def a1 : Parser[Act] = """a1""".r  ^^ (new Act(_))
}

class Lib2 extends WBase {
  override def activities() : WBase#Parser[Act] = a2
  def a2 : Parser[Act] = """a2""".r  ^^ (new Act(_))
}

/** simple extension example */
//class WParser extends WBase {
//  type Gen = () => WBase#Parser[Act]
//  type P = WBase#Parser[Act]
//  
//  val libs = new collection.mutable.ListBuffer[Gen]()
// 
//  this += (() => new Lib1().activities())
//  this += (() => new Lib2().activities())
//  
//  def += (lib:Gen) = { libs append lib; this }
//  
//  override def activities() : Parser[Act] = libs map (_()) reduceLeft (_ | _).asInstanceOf[WBase#Parser[Act]]
//}

