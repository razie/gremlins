/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.AA
import razie.base.{ActionContext => AC}

//-------------------------------- basic activities

  /** simple activities just do their thing */
  case class WfSimple extends WfAct { 
    /** executing these means maybe doing something (in=>out) AND figuring out who's next */
    override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = this match {
       case a : WfExec => (a.exec(in, v), glinks)
       case _ => (v,glinks)
    }
  }

  /** simple activities just do their thing */
  case class WfWrapper (wrapped:WfExec) extends WfSimple with WfExec { 
    override def exec (in:AC, prevValue:Any) = wrapped.exec(in, prevValue)
    override def toString : String = "wf." + wrapped.wname
    override def wname = wrapped.wname
  }

  //------------------- begin / end subgraph

  case class WfStart (a:WfAct*) extends WfSimple {  a map (this --> _) }

  case class WfEnd (a:WfAct*) extends WfSimple { 
    // find all leafs and connect them to me
    (a flatMap ( x => razie.g.GStuff.filterNodes[WA,WL](x) {z => z.glinks.isEmpty} )) foreach (i => i +-> this)
  }

  /** 
   * the new proxy: contains a sub-graph.
   * will point to the entry point of its sub-graph and connect the end of it to itself.
   */
  case class WfScope (aa:WfAct, var l:WL*) extends WfStart (aa) { 
    WfScopeEnd (aa, l:_*)
  }

  /** 
   * special activity - ends a scope and points to where the scope was meant to point to
   */
  case class WfScopeEnd (s:WfAct, var l:WL*) extends WfEnd (s) { 
    glinks = l.map(x=>{if (x.a == s) WL(this, x.z) else x})
  }

  /** note that this proxy is stupid... see WfElse to understand why... */
  case class WfProxy (a:WfAct, var l:WL*) extends WfSimple { 
    // if the depy was from a to someone, update it to be this to someone...?
    glinks = l.map(x=>{if (x.a == a) WL(this, x.z) else x})
    
//    override def exec (in:AC, v:Any) : Any = a.exec(in, v)
    /** executing these means maybe doing something (in=>out) AND figuring out who's next */
    override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = (a.traverse(in, v)._1, glinks)
    
    override def toString : String = 
      this.getClass().getSimpleName + "()"
  }

  //------------------------------- seq / par
  
  /** a sequence contains a list of proxies */
  case class WfSeq (a:WfAct*) extends WfAct {
    gnodes = build_Wtf_?
    glinks = gnodes.firstOption.map(WL(this,_)).toList
    
    // wrap each in a proxy and link them in sequence
    // named this way in amazement that ? was accepted...
    def build_Wtf_? : List[WfAct] = {
      a.foldRight (Nil:List[WfAct])((x,l) => wf.bound(x,l.headOption.map(WL(x,_)).toList:_*) :: l)
    }
   
    /** does nothing just returns the first in list */
    override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = (v, glinks)
   
    // to avoid seq(seq(t)) we redefine to just link to what i already have
    override def + (e:WfAct) = {
       val p = WfProxy (e)
       if (glinks.lastOption.isDefined)
         gnodes.lastOption.map(a=>a.asInstanceOf[WfProxy].l = WL(a,p))
       else 
         glinks = List(WL(this,p)) 
       gnodes = gnodes.toList ::: List(p) 
       this
       }
  }

  /** TODO fork-join. The end will wait for all processing threads to be done */
  case class WfPar (a:WfAct*) extends WfAct {
    gnodes = a
    glinks = a.map (x => WL(this,x))
   
    /** does nothing just returns the spawns */
    override def traverse (in:AC, v:Any) : (Any,Seq[WL]) =  (v, glinks)
  }

  /** scala code with no input nor return values */
  case class WfScala (val f : () => Unit) extends WfSimple with WfExec { 
     override def exec (in:AC, v:Any) : Any = { f(); v }
  }   

  /** scala code with a return value but no input */
  case class WfScalaV0 (val f : () => Any) extends WfSimple with WfExec { 
     override def exec (in:AC, v:Any) : Any = f()
  }   

  /** scala code with input and return values */
  case class WfScalaV1 (val f : (Any) => Any) extends WfSimple with WfExec { 
     override def exec (in:AC, v:Any) : Any = f(v)
  }   

  /** scala code with input and return values */
  case class WfScalaV1u (val f : (Any) => Unit) extends WfSimple with WfExec { 
     override def exec (in:AC, v:Any) : Any = { f(v); v }
  }   

////----------- pattern based
//  
//abstract case class WAM () extends WfAct {
//  def mlinks : Seq[WLM] // links 
//   
//  /** executing these means maybe doing something (in=>out) AND figuring out who's next */
//  def traverse (in:AC, v:Any) : (Any,Seq[WL])
//}
//
//trait Matcher {
//  def matches (x:Any) : Boolean = true
//}
//
///** may want to store some color here...or have the link do something */
//case class WLM (override val a:WA, override val z:WA, val m:Matcher) extends WL(a,z) { }
//  
//  /** simple activities just do their thing */
//  case class WfmSimple extends WfAct { 
//    override def activities : Seq[WA] = Nil
//    override def links : Seq[WL] = Nil
//   
//    /** executing these means maybe doing something (in=>out) AND figuring out who's next */
//    override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = this match {
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
case class WfMatch1 (val expr : () => Any, e:WfCases1) extends WfAct {
  gnodes = Nil
  glinks = e.l.map (WL(this,_))
   
  /** return either branch, depending on cond */
  override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = {
    val e = expr()
    var ret:Seq[WL] = Nil
    
//    (v, links.flatMap(l => l.z.asInstanceOf[WfCase1].apply(e).map(WL(this,_))))
    for (i <- Range (0, glinks.size-1)) 
      if (ret.isEmpty) {
        val r = glinks(i).z.asInstanceOf[WfCase1].apply(e).map (WL(this,_))
        if (! r.isEmpty)
          ret = r.toList
      }
    (v, ret)
  }
}

/** like wmatch but wguard matches ALL possibilities */
case class WfGuard1 (_expr : () => Any, _e:WfCases1) extends WfMatch1 (_expr, _e) {
  /** return either branch, depending on cond */
  override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = {
    val e = expr()
    (v, glinks.flatMap(l => l.z.asInstanceOf[WfCase1].apply(e).map(WL(this,_))))
  }
}

/** a case of a wmatch or wguard */
class WfCase1(pattern : => PartialFunction[Any, WfAct]) extends WfSimple { 
   def apply(value: Any) : Option[WfAct] = 
     if (value == null || !pattern.isDefinedAt(value)) 
       None
     else 
       Some(pattern.apply(value))
       
   def + (b:WfCase1) : WfCases1 = new WfCases1 (List(this) ::: List(b))
   def + (b:List[WfCase1]) : WfCases1 = new WfCases1(List(this) ::: b)
}

/** match anything */
class WfCaseAny1 (a:WfAct) extends WfCase1(null) { 
   override def apply(value: Any) : Option[WfAct] = Some(a)
}

protected class WfCases1 (val l : List[WfCase1]) extends WfSimple {
  def + (b:WfCase1) : WfCases1 = new WfCases1 (l ::: List(b))
}

//-------------------------------matchers 2

class WfCase2[T <: Any] (val t : T) (a:WfAct) extends WfProxy(a) { 
   glinks = WL(this,a) :: Nil
   
   def apply(value: Any) : Option[WfAct] =
     if (value == null || !value.isInstanceOf[T] || value != t)
       None
     else 
       Some(a)
       
  
   def + (b:WfCase2[_ <: Any]) : WfCases2 = new WfCases2 (List(this) ::: List(b))
   def + (b:List[WfCase2[_ <: Any]]) : WfCases2 = new WfCases2(List(this) ::: b)
}

class WfCase2a[T <: Any] (a:WfAct) extends WfCase2 (null) (a) { 
   override def apply(value: Any) : Option[WfAct] =
     if (value == null || !value.isInstanceOf[T])
       None
     else 
       Some(a)
}

class WfCase2p[T <: Any] (cond : T => Boolean) (a : WfAct) extends WfCase2 (null) (a) { 
   override def apply(value: Any) : Option[WfAct] =
     if (value == null || !value.isInstanceOf[T] || !cond(value.asInstanceOf[T]))
       None
     else 
       Some(a)
}

class WfCase2ap[T <: Any] (cond : T => Boolean) (a : WfAct) extends WfCase2a (a) { 
   override def apply(value: Any) : Option[WfAct] =
     if (value == null || !value.isInstanceOf[T] || !cond(value.asInstanceOf[T]))
       None
     else 
       Some(a)
}

class WfCases2 (val l : List[WfCase2[_ <: Any]]) extends WfSimple {
  def + (b:WfCase2[_ <: Any]) : WfCases2 = new WfCases2 (l ::: List(b))
}

/** match anything */
class WfCaseAny2 (a:WfAct) extends WfCase2(null)(a) { 
   override def apply(value: Any) : Option[WfAct] = Some(a)
}

abstract class WfMatchBase extends WfAct {
  gnodes = Nil
  glinks = branches.map (WL(this,_))
  
  val expr : () => Any
  val branches : Seq[WfCase2[_]]
  
}

case class WfMatch2 (
      val expr : () => Any, 
      val branches : Seq[WfCase2[_]]
      ) extends WfMatchBase {
   
  override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = {
    val e = expr()
    var ret:Seq[WL] = Nil
    
//    (v, links.flatMap(l => l.z.asInstanceOf[WfCase1].apply(e).map(WL(this,_))))
    for (i <- Range (0, glinks.size-1)) 
      if (ret.isEmpty) {
        val r = glinks(i).z.asInstanceOf[WfCase2[_]].apply(e).map (WL(this,_))
        if (! r.isEmpty)
          ret = r.toList
      }
    (v, ret)
  }
}


//---------------- if

case class WfElse (t:WfAct)  extends WfScope (t) {
//  glinks = WL(this,t) :: Nil
}
  
case class WfIf   (val cond : Any => Boolean, t:WfAct, var e:WfElse*) extends WfAct {
  gnodes = List(t) ::: e.toList
  glinks = List (WL(this,t)) ::: e.map(WL(this,_)).toList
   
  /** return either branch, depending on cond */
  def traverse (in:AC, v:Any) : (Any,Seq[WL]) = 
    if (cond(v)) 
      (v, glinks.first :: Nil)
    else
      (v, e.firstOption.map(WL(this,_)).toList)
        
   def welse (a:WfAct) : WfIf = {
     if (e.isEmpty) {
       this.e = WfElse (a)
       this +-> e
     } else // oops, is this an welse wif welse?
       this.e.first.t match {
        case i : WfIf => i.welse(a)
        case _ => razie.Error ("a second welse clause") // TODO cause syntax error
     }
     this
   }
}
