package razie.wf.study5

import razie.AA
import razie.base.{ActionContext => AC}
import razie.wf.{WfExec, WfaState, ProcStatus, ProcState, WfLib}

/** 
 * study 4's main goal is to settle the underlying structure, based on matching
 * 
 * also, should be so close it may end up being final
 * 
 * 1. separate exec into WfExec from traverse in WA. traverse () is graph processor whikle exec () 
 * is actual actions
 * 
 * @author razvanc
 */
object wf extends WfLib {
   
  //----------------- base activitities
   
  // TODO this doesn't work if implicit...see ScaBug1
  implicit def w   (f : => Unit) = WfScala (()=>f)
  implicit def w   (f : => Any) = WfScalaV0 (()=>f)
  implicit def wa  (f : Any => Any) = WfScalaV1 ((x)=>f(x))
  implicit def wau (f : Any => Unit) = WfScalaV1u ((x)=>f(x))
  
  def apply (f : => Unit) = w(f)
  def apply (f : => Any) = w(f)
  
  //----------------- if
  
  type Cond0 = Unit => Boolean
  type Cond1 = Any => Boolean

  implicit def wc0 (cond : => Boolean) : Cond1 = (x) => cond
  
  def wif  (cond : Cond1, t:WfAct)        = WfIf (cond, t)
  def wuif (cond : Cond1) (f: => Unit)    = WfIf (cond, WfScala(()=>f))
  def wif  (cond : Cond1) (f: => Any)     = WfIf (cond, WfScalaV0(()=>f))
  def waif (cond : Cond1) (f: Any => Any) = WfIf (cond, WfScalaV1((x)=>f(x)))
  def wauif (cond : Cond1) (f: Any => Unit)= WfIf (cond, WfScalaV1((x)=>f(x)))
  
  //----------------- match
  
  // def wmatch1 (expr : =>Any) (f: PartialFunction[Any, Unit]) = WfMatch1 (()=>expr, WfCaseB (()=>expr, (x:Any)=>f.apply(x)))
  def wmatch1 (expr : =>Any) (f: WfCases1) = WfMatch1 (()=>expr, f)
  def wguard1 (expr : =>Any) (f: WfCases1) = WfGuard1 (()=>expr, f)
  def wcase1 (f: => PartialFunction[Any, WfAct]) = new WfCase1(f)
  def wcaseany1 (f: WfAct) = new WfCaseAny1(f)
  
  def wmatch2 (expr : =>Any) (f: WfCases2) = WfMatch2 (()=>expr, f.l)
//  def wguard1 (expr : =>Any) (f: WfCases1) = WfGuard1 (()=>expr, f)
  def wcase2[T] (t:T) (f: => Unit) = new WfCase2[T](t)(w(f))
  def wcase2[T] (cond: T => Boolean) (f: => Unit) = new WfCase2p[T](cond)(w(f))
  // this should work because it is only called when the value actually matches...
  def wcase2a[T <: Any] (f: T => Unit) = new WfCase2a[T](wau(x => f(x.asInstanceOf[T])))
  def wcase2a[T <: Any] (cond:T => Boolean)(f: T => Unit) = new WfCase2ap[T](cond)(wau(x => f(x.asInstanceOf[T])))
  def wcaseany2 (f: WfAct) = new WfCaseAny2(f)
  
  def wmatch (expr : =>Any) (f: WfCases2) = wmatch2 (expr)(f)
  def wcase[T] (t:T) (f: => Unit) = wcase2(t)(f)
  def wcase[T] (cond: T => Boolean) (f: => Unit) = wcase2(cond)(f)
  def wcasea[T <: Any] (f: T => Unit) = wcase2a(f)
  def wcaseany (f: WfAct) = wcaseany2(f)

  def seq (a : WfAct*) = new WfSeq (a:_*)
}

  /** simple activities just do their thing */
  case class WfSimple extends WfAct { 
    override def activities : Seq[WA] = Nil
    override def links : Seq[WL] = Nil
   
    /** executing these means maybe doing something (in=>out) AND figuring out who's next */
    override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = this match {
       case a : WfExec => (a.exec(in, v), links)
       case _ => (v,links)
    }
  }

  /** simple activities just do their thing */
  case class WfWrapper (wrapped:WfExec) extends WfSimple with WfExec { 
    override def exec (in:AC, prevValue:Any) = wrapped.exec(in, prevValue)
    override def toString : String = "wf." + wrapped.wname
    override def wname = wrapped.wname
  }

  /** note that this proxy is stupid... see WfElse to understand why... */
  case class WfProxy (a:WfAct, var l:WL*) extends WfSimple { 
//    override def exec (in:AC, v:Any) : Any = a.exec(in, v)
    /** executing these means maybe doing something (in=>out) AND figuring out who's next */
    override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = (a.traverse(in, v)._1, links)
    
    // if the depy was from a to someone, update it to be this to someone...?
    override def links : Seq[WL] = l.map(x=>{if (x.a == a) WL(this, x.z) else x})
    
    override def toString : String = 
      this.getClass().getSimpleName + "()"
  }

  /** a sequence contains a list of proxies */
  case class WfSeq (a:WfAct*) extends WfAct {
    var _activities : List[WfAct] = build_Wtf_?
    var _links : List[WL] = _activities.firstOption.map(WL(this,_)).toList
    
    override def activities = _activities
    override def links = _links

    // wrap each in a proxy and link them in sequence
    // named this way in amazement that ? was accepted...
    def build_Wtf_? : List[WfAct] = {
      a.foldRight (Nil:List[WfAct])((x,l) => WfProxy(x,l.headOption.map(WL(x,_)).toList:_*) :: l)
    }
   
    /** does nothing just returns the first in list */
    override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = (v, links)
   
    // to avoid seq(seq(t)) we redefine to just link to what i already have
    override def + (e:WfAct) = {
       val p = WfProxy (e)
       if (_links.lastOption.isDefined)
         _activities.lastOption.map(a=>a.asInstanceOf[WfProxy].l = WL(a,p))
       else 
         _links = List(WL(this,p)) 
       _activities = _activities ::: List(p) 
       this
       }
  }

  /** fork-join. The end will wait for all processing threads to be done */
  case class WfPar (a:WfAct*) extends WfAct {
    override def activities : Seq[WA] = a
    override val links : Seq[WL] = a.map (x => WL(this,x))
   
    /** does nothing just returns the spawns */
    override def traverse (in:AC, v:Any) : (Any,Seq[WL]) =  (v, links)
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
  override def activities : Seq[WA] = Nil
  override def links : Seq[WL] = e.l.map (WL(this,_))
   
  /** return either branch, depending on cond */
  override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = {
    val e = expr()
    var ret:Seq[WL] = Nil
    
//    (v, links.flatMap(l => l.z.asInstanceOf[WfCase1].apply(e).map(WL(this,_))))
    for (i <- Range (0, links.size-1)) 
      if (ret.isEmpty) {
        val r = links(i).z.asInstanceOf[WfCase1].apply(e).map (WL(this,_))
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
    (v, links.flatMap(l => l.z.asInstanceOf[WfCase1].apply(e).map(WL(this,_))))
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
   def apply(value: Any) : Option[WfAct] =
     if (value == null || !value.isInstanceOf[T] || value != t)
       None
     else 
       Some(a)
       
   override def links : Seq[WL] = WL(this,a) :: Nil
  
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
  val expr : () => Any
  val branches : Seq[WfCase2[_]]
  
  override def activities : Seq[WA] = Nil
  override def links : Seq[WL] = branches.map (WL(this,_))
}

case class WfMatch2 (
      val expr : () => Any, 
      val branches : Seq[WfCase2[_]]
      ) extends WfMatchBase {
   
  override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = {
    val e = expr()
    var ret:Seq[WL] = Nil
    
//    (v, links.flatMap(l => l.z.asInstanceOf[WfCase1].apply(e).map(WL(this,_))))
    for (i <- Range (0, links.size-1)) 
      if (ret.isEmpty) {
        val r = links(i).z.asInstanceOf[WfCase2[_]].apply(e).map (WL(this,_))
        if (! r.isEmpty)
          ret = r.toList
      }
    (v, ret)
  }
}


//---------------- if

case class WfElse (t:WfAct)  extends WfProxy (t) {
  override def links : Seq[WL] = WL(this,t) :: Nil
}
  
case class WfIf   (val cond : Any => Boolean, t:WfAct, var e:WfElse*) extends WfAct {
  override def activities : Seq[WA] = List(t) ::: e.toList
  override def links : Seq[WL] = List (WL(this,t)) ::: e.map(WL(this,_)).toList
   
  /** return either branch, depending on cond */
  def traverse (in:AC, v:Any) : (Any,Seq[WL]) = 
    if (cond(v)) 
      (v, links.first :: Nil)
    else
      (v, e.firstOption.map(WL(this,_)).toList)
        
   def welse (a:WfAct) : WfIf = {
     if (e.isEmpty)
       this.e = WfElse (a); 
     else // oops, is this an welse wif welse?
       this.e.first.t match {
        case i : WfIf => i.welse(a)
        case _ => razie.Error ("a second welse clause") // TODO cause syntax error
     }
     this
   }
}

//--------------------- samples

object Wf4Main extends Application {
   import wf._

   var acc = ""

  // test
  lazy val if1 = wuif (1==1) {
     acc += "it's "
     acc += "true "
     acc += "..."
  } 
  
  lazy val if2 = wif (1==2) {
     var lacc = ""
     lacc += "it's "
     lacc += "true "
     lacc += "..."
     lacc
  } welse 
     wf {println ("it's")} + 
     wf {println ("false")} +
     wf {println ("!!!")}
  
  lazy val if3a = wif (1==2) {
     var lacc = ""
     lacc += "it's "
     lacc += "true "
     lacc += "..."
     lacc
  } welse wif (3==2) {
     var lacc = ""
     lacc += "it's "
     lacc += "true "
     lacc += "..."
     lacc
  } welse 
     wa {_.toString + " it's"} + 
     wa {s:Any => s.toString + " false"} + 
     wa {s:Any => s.toString + " ..."} 

  // the trouble with this is that the branches are only known as it runs...can't see the defn
  lazy val match1 = wmatch1 ("stuka") {
     wcase1 {case 1 => log ("matched 1")} +
     wcase1 {case 2 => println ("matched 2")} +
     wcase1 {case 3 => println ("matched 2")} +
     wcase1 {case 4 => println ("matched 2")} +
     wcase1 {case s:String => println ("matched s=" + s)} +
     wcase1 {case l@List(1,2,3) => println ("matched list " + l)} +
     wcaseany1 {println ("matched none")}
  }
  
  lazy val match2 = wmatch2 ("stuka") {
     wcase2[Int] (1) {println ("matched 1")} +
     wcase2[Int] (2) {println ("matched 2")} +
     wcase2 ("Gigi") {println ("matched Gigi")} +
     wcase2a {s:String => println ("matched s=" + s)} +
     wcase2 (List(1,2,3)) {println ("matched list 1,2,3")} +
     wcase2  {l:Seq[Int] => l(2) == 2} {println ("matched list with secnod elem 2")} +
     wcase2a {l:Seq[Int] => l == List(1,2,3)} {l:Seq[Int] => println ("matched list " + l)} +
     wcaseany2 {println ("matched none")}
  }
  
  this e if1
  this e if2
  this e if3a
  this e match1
  this e match2
  
  def e (w : WfAct) = {
    acc = ""
    println ("")
    println ("Workflow is: " + w.mkString)
    acc += "Running: "
    println (">>>>>>>> RESULT is " + Engines().exec(w, razie.base.scripting.ScriptFactory.mkContext(), ""))
    println (acc)
    println ("=========================================================")
  }
  
}
