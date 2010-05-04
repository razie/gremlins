/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.AA
import razie.base.{ActionContext => AC}
import razie.wf._

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
  case class WfWrapper (wrapped:WfExec) extends WfSimple with WfExec with HasDsl { 
    override def exec (in:AC, prevValue:Any) = wrapped.exec(in, prevValue)
    override def toString : String = "wf." + wrapped.wname
    override def wname = wrapped.wname
    
    override def toDsl = wf toDsl wrapped
  }

  //------------------- begin / end subgraph

  /** sub-graph root, control node: create new control node linking to all others */
  case class WfStart (a:WfAct*) extends WfSimple {  a map (this --> _) }

  /** sub-graph end, control node: find all ends of subgraph and point to this end */
  case class WfEnd (a:WfAct*) extends WfSimple { 
    // find all leafs and connect them to me
    (a flatMap ( x => razie.g.Graphs.filterNodes[WfAct,WL](x) {z => z.glinks.isEmpty} )) foreach (i => i +-> this)
  }

  /** 
   * the new proxy: contains a sub-graph.
   * will point to the entry point of its sub-graph and connect the end of it to itself.
   */
  case class WfScope (aa:WfAct, var l:WL*) extends WfStart (aa) with HasDsl { 
    WfScopeEnd (aa, l:_*)
    override def toDsl = "scope " + (wf toDsl aa)
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

  //------------------------- selector

object Selblahblah {
  def expr (in:AC, v:Any) = v
}
  
/** selector activity - the basis for many other */
case class WfSelOne (expr:(AC,Any) => Any = Selblahblah.expr) extends WfSimple { 

  /** overwrite this if you wish...rather than pass it in */
  def eval (in:AC, v:Any) : Any = expr (in, v)
  
  /** executing these means maybe doing something (in=>out) AND figuring out who's next */
  override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = {
    val sel = expr (in, v)
    (v, glinks.filter(l => l.isInstanceOf[WLV] && l.asInstanceOf[WLV].selector == sel).headOption.toList)
  }
    
  /** par depy a -> (b,c) */
  def --> [T <: Any] (z:Map[T,WfAct]) = {
    glinks = z.map (p => new WLV(this,p._2,p._1)).toList
    this
  } 
  /** par depy a -> (b,c) */
  def +-> [T <: Any] (z:Map[T,WfAct]) = {
    glinks = glinks.toList ::: z.map (p => new WLV(this,p._2,p._1)).toList
    this
  } 
}

/** selector activity - the basis for many other */
case class WfSelMany (expr:(AC,Any) => Any) extends WfSimple { 

   override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = {
    val sel = expr (in, v)
    (v, glinks.filter(l => l.isInstanceOf[WLV] && l.asInstanceOf[WLV].selector == sel))
  }
}

  //-------------------------funky
  
  case class WfLabel (name:String, aa:WfAct) extends WfProxy (aa) {
     
  }
  
  case class WfWhen (name:String, aa:WfAct) extends WfProxy (aa) {
    // TODO needs to find the labeled one
    // TODO needs to wait 
  }
  
  //------------------------------- seq / par
  
  /** a sequence contains a list of proxies */
  case class WfSeq (a:WfAct*) extends WfAct with HasDsl {
    // wrap each in a proxy and link them in sequence
    gnodes = 
      a.foldRight (Nil:List[WfAct])((x,l) => wf.scope(x,l.headOption.map(WL(x,_)).toList:_*) :: l)
    glinks = gnodes.firstOption.map(WL(this,_)).toList
    
   
    /** does nothing just returns the first in list */
    override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = (v, glinks)
   
    // to avoid seq(seq(t)) we redefine to just link to what i already have
    override def + (e:WfAct) = {
      val p = wf.scope(e)
      if (glinks.lastOption.isDefined)
        gnodes.last --> p
      else 
       glinks = List(WL(this,p)) 
     gnodes = gnodes.toList ::: List(p) 
     this
     }
    
    override def toDsl = "seq {\n" + gnodes.map(wf toDsl _).mkString("\n") + "\n}"
  }

  /** TODO fork-join. The end will wait for all processing threads to be done */
  case class WfPar (a:WfAct*) extends WfSimple with HasDsl {
    lazy val aj = new AndJoin()
    
    gnodes = a map wf.scope
    glinks = gnodes map (x => WL(this,x))
    this --| aj
    
    // to avoid par(par(t)) we redefine to just link to what i already have
    override def | (e:WfAct) = {
      val p = wf.scope(e)
      glinks = List(WL(this,p)) ::: glinks.toList
      gnodes = gnodes.toList ::: List(p) 
      p --| aj
      this
    }
    
    override def toDsl = "par {\n" + gnodes.map(wf toDsl _).mkString("\n") + "\n}"
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

//---------------- if

case class WfElse (t:WfAct)  extends WfScope (t) 
  
case class WfIf   (val cond : WFunc[Boolean], t:WfAct, var e:Option[WfElse] = None) extends WfAct with HasDsl {
  gnodes = List(t) ::: e.toList
  glinks = List (WL(this,t)) ::: e.map(WL(this,_)).toList
   
  /** return either branch, depending on cond */
  override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = 
    if (cond.exec(in, v)) 
      (v, glinks.first :: Nil)
    else
      (v, e.map(WL(this,_)).toList) // TODO not make new links, reuse those in the definition
        
   def welse (a:WfAct) : WfIf = {
     if (e.isEmpty) {
       this.e = Some(WfElse (a))
       this +-> e.get
     } else // oops, is this an welse wif welse?
       this.e.first.t match {
        case i : WfIf => i.welse(a)
        case _ => razie.Error ("a second welse clause") // TODO cause syntax error
     }
     this
   }
  
    override def toDsl = 
       "if (" + (wf toDsl cond) + ") then " + (wf toDsl t) + 
       (e map (" else " + _.toDsl)).mkString 
}

case class WfIf2   (val cond : Any => Boolean, t:WfAct, var e:Option[WfElse]) 
extends WfSelOne ((in,v)=>cond(v)) {

  this +-> Map(true -> t)
  e map (x => this +-> Map(false -> x))

  // syntax helper
   def welse (a:WfAct) : WfIf2 = {
     if (e.isEmpty) {
       this.e = Some(WfElse (a)) // to make sure that for next welse is not empty
       this +-> Map(false -> this.e.get)
     } else // oops, is this an welse wif welse?
       this.e.first.t match {
        case i : WfIf => i.welse(a)
        case _ => razie.Error ("a second welse clause") // TODO cause syntax error
     }
     this
   }
  
}
