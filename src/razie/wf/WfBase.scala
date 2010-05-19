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
       case a : WfExec => (a.apply(in, v), glinks)
       case _ => (v,glinks)
    }
  }

  /** simple activities just do their thing */
  case class WfWrapper (wrapped:WfExec) extends WfSimple with WfExec with HasDsl { 
    override def apply (in:AC, prevValue:Any) = wrapped.apply(in, prevValue)
    override def toString : String = wrapped match {
       case d:notisser => "wf." + wrapped.wname
       case d:HasDsl => d.toDsl
       case _ => "wf." + wrapped.wname
    }
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

/** selector activity - the basis for many other */
case class WfSelOne (expr: WFunc[_] = WFuncNil) extends WfSimple { 

  /** executing these means maybe doing something (in=>out) AND figuring out who's next */
  override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = {
    val sel = expr.apply (in, v)
    (v, glinks.filter(l => l.isInstanceOf[WLV] && l.asInstanceOf[WLV].selector == sel).headOption.toList)
  }
    
  /** depy a -> (b,c) */
  def --> [T <: Any] (z: => Map[T,WfAct]) = {
    glinks = z.map (p => new WLV(this,p._2,p._1)).toList
    this
  } 
  /** depy a -> (b,c) */
  def +-> [T <: Any] (z: => Map[T,WfAct]) = {
    glinks = glinks.toList ::: z.map (p => new WLV(this,p._2,p._1)).toList
    this
  } 
  /** depy a -> (b,c) */
  def +-> [T <: Any] (z:(T,WfAct)*) = {
    glinks = glinks.toList ::: z.map (p => new WLV(this,p._2,p._1)).toList
    this
  } 
}

/** selector activity - the basis for many other */
case class WfSelMany (expr:WFunc[_]) extends WfSimple { 

   override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = {
    val sel = expr.apply (in, v)
    (v, glinks.filter(l => l.isInstanceOf[WLV] && l.asInstanceOf[WLV].selector == sel))
  }
}

  //-------------------------funky
  
  case class WfLabel (name:String, aa:WfAct) extends WfProxy (aa)
  
  case class WfWhen (name:String, aa:WfAct) extends WfProxy (aa) {
    // TODO needs to find the labeled one
    // TODO needs to wait 
  }
  
  //------------------------------- seq / par
 
  /** do not break */
  abstract class WfBound extends WfSimple {
    def lastAct : WfAct
     
  /** reroute */
    override def --> [T<:WfAct] (z:T)(implicit linkFactory: LFactory) : WfAct = {
      lastAct.glinks = linkFactory(lastAct,z) :: Nil
      this
    }
  /** add a new dependency */
    override def +-> [T<:WfAct](z:T)(implicit linkFactory: LFactory) : WfAct = {
      lastAct.glinks = lastAct.glinks.toList.asInstanceOf[List[WL]] ::: List(linkFactory (lastAct, z))
      this
    }
  /** par depy a -> (b,c) */
    override def --> [T<:WfAct] (z:Seq[T])(implicit linkFactory: LFactory) : WfAct = {
      lastAct.glinks = z.map (linkFactory(lastAct,_)).toList
      this
    }   
  /** par depy a -> (b,c) */
    override def +-> [T<:WfAct] (z:Seq[T])(implicit linkFactory: LFactory) : WfAct = {
      lastAct.glinks = lastAct.glinks.toList.asInstanceOf[List[WL]] ::: z.map (linkFactory(lastAct,_)).toList
      this
    } 
  }
  
  /** a sequence contains a list of proxies 
   * 
   * NOTE this is a scoped activity - it will scope the enclosed sub-graphs
   */
  case class WfSeq (a:WfAct*) extends WfBound with HasDsl {
    override def lastAct : WfAct = glinks.lastOption.map(x=>gnodes.last).getOrElse(this)
    
    // wrap each in a proxy and link them in sequence
    gnodes = 
      a.foldRight (Nil:List[WfAct])((x,l) => wf.scope(x,l.headOption.map(WL(x,_)).toList:_*) :: l)
    glinks = gnodes.firstOption.map(WL(this,_)).toList
    
    // to avoid seq(seq(t)) we redefine to just link to what i already have
    override def + (e:WfAct) = {
      val p = wf.scope(e)
      if (glinks.lastOption.isDefined)
        gnodes.last --| p
      else 
       glinks = List(WL(this,p)) 
      gnodes = gnodes.toList ::: List(p) 
      this
      }
    
    override def toDsl = "seq {\n" + gnodes.map(wf toDsl _).mkString("\n") + "\n}"
  }

  /** fork-join. The end will wait for all processing threads to be done 
   * 
   * NOTE this is a scoped activity - it will scope the enclosed sub-graphs
   */
  case class WfPar (a:WfAct*) extends WfBound with HasDsl {
    lazy val aj = new AndJoin()
    
    gnodes = a map wf.scope
    glinks = gnodes map (x => WL(this,x))
    this --| aj
    
    override def lastAct : WfAct = glinks.lastOption.map(x=>gnodes.last).getOrElse(this)
    
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
     override def apply (in:AC, v:Any) : Any = { f(); v }
  }   

  /** scala code with a return value but no input */
  case class WfScalaV0 (val f : () => Any) extends WfSimple with WfExec { 
     override def apply (in:AC, v:Any) : Any = f()
  }   

  /** scala code with input and return values */
  case class WfScalaV1 (val f : (Any) => Any) extends WfSimple with WfExec { 
     override def apply (in:AC, v:Any) : Any = f(v)
  }   

  /** scala code with input and return values */
  case class WfScalaV1u (val f : (Any) => Unit) extends WfSimple with WfExec { 
     override def apply (in:AC, v:Any) : Any = { f(v); v }
  }   

//---------------- if

case class WfElse (t:WfAct)  extends WfScope (t) 

/** 
 * if-then-else
 * 
 * NOTE this is a scoped activity - it will scope the enclosed sub-graphs when serialized
 * TODO NOTE this is NOT a scoped activity - it will NOT scope the enclosed sub-graphs if not serialized
 */
case class WfIf   (val cond : WFunc[Boolean], t:WfAct, var e:Option[WfElse] = None) 
extends WfSelOne (cond) with HasDsl {

  this +-> (true -> t)
  e map (x => this +-> (false -> x))

  // syntax helper
   def welse (a:WfAct) : WfIf = {
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
  
  override def toDsl = 
    "if (" + (wf toDsl cond) + ") then " + (wf toDsl t) + 
    (e map (" else " + _.toDsl)).mkString 
}
