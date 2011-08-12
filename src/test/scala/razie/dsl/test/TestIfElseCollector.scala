//package razie.dsl.test
//
//import razie.dsl._
//
////** we're collecting expressions like this: */
//object Expr {
//  val expr = $if(true) {
//    v("a") := 1
//    v("b") := 2
//  } $else $if(true) {
//    v("b") := 1
//    v("a") := 2
//  }
//
//  def v(s:String) = new TVar(s)
//  def $if (cond: => Boolean) (body: => Unit) : Token = If (cond, body)
//
//  class Token
//  trait Collectable {
////    DslCollector.collect (this)
//  }
//  
//  case class TVar (name:String) {
//    def := (expr: => Any) = new TAssign (this, expr)
//  }
//  
//  case class TAssign (v:TVar, e:) extends Token with Collectable { }
//}
//
//class TestDslCollector {
//
//}
//
