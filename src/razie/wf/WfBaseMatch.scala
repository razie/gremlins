/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.AA
import razie.base.{ActionContext => AC}
import razie.wf._

////----------- pattern based
//  
//abstract case class WAM () extends WfActivity {
//  def mlinks : Seq[WfLinkM] // links 
//   
//  /** executing these means maybe doing something (in=>out) AND figuring out who's next */
//  def traverse (in:AC, v:Any) : (Any,Seq[WfLink])
//}
//
//trait Matcher {
//  def matches (x:Any) : Boolean = true
//}
//
///** may want to store some color here...or have the link do something */
//case class WfLinkM (override val a:WA, override val z:WA, val m:Matcher) extends WfLink(a,z) { }
//  
//  /** simple activities just do their thing */
//  case class WfmSimple extends WfActivity { 
//    override def activities : Seq[WA] = Nil
//    override def links : Seq[WfLink] = Nil
//   
//    /** executing these means maybe doing something (in=>out) AND figuring out who's next */
//    override def traverse (in:AC, v:Any) : (Any,Seq[WfLink]) = this match {
//       case a : WfExec => (a.exec(in, v), links)
//       case _ => (v,links)
//    }
//    
////    def mlinks : 
//  }
//
////---------------------- END matcher-based  
//
//----------------------- match/guard
  
/** match AT MOST one branch */
class WfMatch1 (val expr : () => Any, e:WfCases1) extends WfActivity {
  gnodes = Nil
  glinks = e.l.map (new WfLink(this,_))
   
  /** return either branch, depending on cond */
  override def traverse (in:AC, v:Any) : (Any,Seq[WfLink]) = {
    val e = expr()
    var ret:Seq[WfLink] = Nil
    
//    (v, links.flatMap(l => l.z.asInstanceOf[WfCase1].apply(e).map(WfLink(this,_))))
    for (i <- Range (0, glinks.size-1)) 
      if (ret.isEmpty) {
        val r = glinks(i).z.asInstanceOf[WfCase1].apply(e).map (new WfLink(this,_))
        if (! r.isEmpty)
          ret = r.toList
      }
    (v, ret)
  }
}

/** like wmatch but wguard matches ALL possibilities */
class WfGuard1 (_expr : () => Any, _e:WfCases1) extends WfMatch1 (_expr, _e) {
  /** return either branch, depending on cond */
  override def traverse (in:AC, v:Any) : (Any,Seq[WfLink]) = {
    val e = expr()
    (v, glinks.flatMap(l => l.z.asInstanceOf[WfCase1].apply(e).map(new WfLink(this,_))))
  }
}

/** a case of a wmatch or wguard */
class WfCase1(pattern : => PartialFunction[Any, WfActivity]) extends WfSimple { 
   def apply(value: Any) : Option[WfActivity] = 
     if (value == null || !pattern.isDefinedAt(value)) 
       None
     else 
       Some(pattern.apply(value))
       
   def + (b:WfCase1) : WfCases1 = new WfCases1 (List(this) ::: List(b))
   def + (b:List[WfCase1]) : WfCases1 = new WfCases1(List(this) ::: b)
}

/** match anything */
class WfCaseAny1 (a:WfActivity) extends WfCase1(null) { 
   override def apply(value: Any) : Option[WfActivity] = Some(a)
}

protected class WfCases1 (val l : List[WfCase1]) extends WfSimple {
  def + (b:WfCase1) : WfCases1 = new WfCases1 (l ::: List(b))
}

//-------------------------------matchers 2

class WfCase2[T <: Any] (val t : T) (a:WfActivity) extends WfSimple { 
   gnodes = a :: Nil
   glinks = new WfLink(this,a) :: Nil
   
   def apply(value: Any) : Boolean = value != null && value.isInstanceOf[T] && value == t
  
   def + (b:WfCase2[_ <: Any]) : WfCases2 = new WfCases2 (List(this) ::: List(b))
   def + (b:List[WfCase2[_ <: Any]]) : WfCases2 = new WfCases2(List(this) ::: b)
}

class WfCase2a[T <: Any] (a:WfActivity) extends WfCase2 (null) (a) { 
   override def apply(value: Any) : Boolean = value != null && value.isInstanceOf[T]
}

class WfCase2p[T <: Any] (cond : T => Boolean) (a : WfActivity) extends WfCase2 (null) (a) { 
   override def apply(value: Any) : Boolean = value != null && value.isInstanceOf[T] && cond(value.asInstanceOf[T])
}

class WfCase2ap[T <: Any] (cond : T => Boolean) (a : WfActivity) extends WfCase2a (a) { 
   override def apply(value: Any) : Boolean = value != null && value.isInstanceOf[T] && cond(value.asInstanceOf[T])
}

class WfCases2 (val l : List[WfCase2[_ <: Any]]) extends WfSimple {
  def + (b:WfCase2[_ <: Any]) : WfCases2 = new WfCases2 (l ::: List(b))
}

/** match anything */
class WfCaseAny2 (a:WfActivity) extends WfCase2(null)(a) { 
   override def apply(value: Any) : Boolean = true
}

abstract class WfMatchBase extends WfActivity {
  gnodes = Nil
  glinks = branches.map (new WfLink(this,_))
  
  val expr : AExpr
  val branches : Seq[WfCase2[_]]
  
  lazy val any = glinks.filter(_.z.isInstanceOf[WfCaseAny2])
  lazy val others = glinks.filter(! _.z.isInstanceOf[WfCaseAny2])
}

case class WfMatch2 (
      val expr : AExpr,
      val branches : Seq[WfCase2[_]]
      ) extends WfMatchBase {
  override def traverse (in:AC, v:Any) : (Any,Seq[WfLink]) = {
    val e = expr.apply (in, v)
    val res = others.filter (g => g.z.asInstanceOf[WfCase2[_]].apply(e))
    
    (v,
    if (res.isEmpty) any else res
    )
  }
}
