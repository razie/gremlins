package razie.wf.study4

import razie.AA
import razie.base.{ActionContext => AC}
import razie.wf.{WfActBase, WfaState, ProcStatus, ProcState}

/** 
 * study 3 deals with return values: each acitivty just like scala, returns a value
 * 
 * also, should be so close it may end up being final
 * 
 * @author razvanc
 */
object Wf {
  //  type Cond : => Boolean
   
  //----------------- activitities
  
  def wif  (cond : => Boolean, t:WfAct)        = WfIf (()=>cond, t)
  def wuif (cond : => Boolean) (f: => Unit)    = WfIf (()=>cond, WfScala(()=>f))
  def wif  (cond : => Boolean) (f: => Any)     = WfIf (()=>cond, WfScalaV0(()=>f))
  def waif (cond : => Boolean) (f: Any => Any) = WfIf (()=>cond, WfScalaV1((x)=>f(x)))
  
  def wmatch (expr : =>Any) (f: PartialFunction[Any, Unit]) = WfMatch1 (()=>expr, WfCaseB (()=>expr, (x:Any)=>f.apply(x)))
  
//  def wmatch (expr : =>Any) (f: PartialFunction[Any, WfAct]) = WMatch (expr, )
  
  // TODO this doesn't work if implicit...see ScaBug1
  implicit def wf  (f : => Unit) = WfScala (()=>f)
  implicit def wf  (f : => Any) = WfScalaV0 (()=>f)
  implicit def waf (f : Any => Any) = WfScalaV1 ((x)=>f(x))
  
}

  /** simple activities just do their thing */
  case class WfSimple extends WfAct { 
    override def activities : Seq[WA] = Nil
    override def links : Seq[WL] = Nil
   
    override def execute (in:AC, v:Any) : (Any,Seq[WL]) =  (exec(in, v), links)
  }

  /** note that this proxy is stupid... see WfElse to understand why... */
  case class WfProxy (a:WfAct, var l:WL*) extends WfAct { 
    override def exec (in:AC, v:Any) : Any = a.exec(in, v)
    
    override def activities : Seq[WA] = Nil
    // if the depy was from a to someone, update it to be this to someone...?
    override def links : Seq[WL] = l.map(x=>{if (x.a == a) WL(this, x.z) else x})
   
    /** executing these means maybe doing something (in=>out) AND figuring out who's next */
    override def execute (in:AC, v:Any) : (Any,Seq[WL]) =  (exec(in, v), links)
  }

  /** a sequence contains a list of proxies */
  case class WfSeq (a:WfAct*) extends WfAct {
    var _activities : List[WfAct] = build
    var _links : List[WL] = _activities.firstOption.map(WL(this,_)).toList
    
    override def activities = _activities
    override def links = _links

    // wrap each in a proxy and link them in sequence
    def build : List[WfAct] = {
      a.foldRight (Nil:List[WfAct])((x,l) => WfProxy(x,l.headOption.map(WL(x,_)).toList:_*) :: l)
      
//      var acc = scala.collection.mutable.ListBuffer[WfAct]()
//      var nxt:WfAct = null
//      for (x <- a.reverse) {
//        val nx = WfProxy(x)
//        val l : Seq[WL] = if (nxt == null) Nil else WL(nx, nxt) :: Nil
//        nx.l = l:_*
//        acc prepend nx
//        nxt=nx
//      }
//      acc.toList
    }
   
    /** does nothing just returns the first in list */
    override def execute (in:AC, v:Any) : (Any,Seq[WL]) = (v, links)
   
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
  
  case class WfPar (a:WfAct*) extends WfAct {
    override def activities : Seq[WA] = a
    override val links : Seq[WL] = a.map (x => WL(this,x))
   
    /** does nothing just returns the spawns */
    override def execute (in:AC, v:Any) : (Any,Seq[WL]) =  (v, links)
  }

  /** scala code with no input nor return values */
  case class WfScala (val f : () => Unit) extends WfSimple { 
     override def exec (in:AC, v:Any) : Any = { f(); v }
  }   

  /** scala code with a return value but no input */
  case class WfScalaV0 (val f : () => Any) extends WfSimple { 
     override def exec (in:AC, v:Any) : Any = f()
  }   

  /** scala code with input and return values */
  case class WfScalaV1 (val f : (Any) => Any) extends WfSimple { 
     override def exec (in:AC, v:Any) : Any = f(v)
  }   

  case class WfElse (t:WfAct)  extends WfProxy (t) {
    override def links : Seq[WL] = WL(this,t) :: Nil
  }
  
  case class WfIf   (val cond : () => Boolean, t:WfAct, var e:WfElse*) extends WfAct {
    override def activities : Seq[WA] = List(t) ::: e.toList
    override def links : Seq[WL] = List (WL(this,t)) ::: e.map(WL(this,_)).toList
   
    /** return either branch, depending on cond */
    def execute (in:AC, v:Any) : (Any,Seq[WL]) = 
      if (cond()) 
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
//     def welif (cond : => Boolean, t:WfAct) = {this.e = WfElIf (()=>cond, t); this}
//     def welif (cond : => Boolean) (f: => Unit) = {this.e=WfElIf (()=>cond, WfScala(()=>f)); this}
  }

//  case class WfElIf (cc : () => Boolean, tt:WfAct, ee:WfAct*) extends WfIf (cc, tt, ee:_*) 

case class WfMatch1 (val expr : () => Any, e:WfCaseB) extends WfAct {
  override def activities : Seq[WA] =  e :: Nil
  override def links : Seq[WL] = WL(this,e) :: Nil
   
  /** return either branch, depending on cond */
  override def execute (in:AC, v:Any) : (Any,Seq[WL]) = (v, links)
}

case class WfCaseB (val x: () => Any, val f : (Any) => Unit) extends WfSimple { 
  override def exec (in:AC, v:Any) : Any = f(x())
}   

//case class WfMatch (val expr : () => Any, e:WfAct*) extends WfAct {
//  override def activities : Seq[WA] = e.toList
//  override def links : Seq[WL] = e.map(WL(this,_)).toList
//   
//  /** return either branch, depending on cond */
//  def execute (in:AC) : (AC,Seq[WL]) = 
//    if (cond()) 
//      (in, links.first :: Nil)
//    else
//      (in, e.firstOption.map(WL(this,_)).toList)
//        
//  def welse (a:WfAct) = {this.e = WfElse (a); this}
//  def welif (cond : => Boolean, t:WfAct) = {this.e = WfElIf (()=>cond, t); this}
//  def welif (cond : => Boolean) (f: => Unit) = {this.e=WfElIf (()=>cond, WfScala(()=>f)); this}
//}

object Wf3Main extends Application {
   import Wf._

   var acc = ""

  // test
  lazy val if1 = wif (1==1) {
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
     waf {_.toString + " it's"} + 
     waf {s:Any => s.toString + " false"} + 
     waf {s:Any => s.toString + " ..."} 

  lazy val match1 = wmatch (1) {
     case 1 => println ("matched 1")
     case 2 => println ("matched 2")
     case s:String => println ("matched s")
     case l@List(1,2,3) => println ("matched list " + l)
     case _ => println ("matched none")
  }
  
  lazy val match2 = wmatch (true) {
     case true => println ("true")
     case _ => println ("matched none")
  }
  
  lazy val match3 = wmatch (1) {
     case 1 => println ("matched 1")
     case 2 => println ("matched 2")
     case s:String => println ("matched s")
     case l@List(1,2,3) => println ("matched list " + l)
     case _ => println ("matched none")
  }
  
  this e if1
  this e if2
//  this e if3
  this e if3a
  this e match1
  
  def e (w : WfAct) = {
    acc = ""
    println ("")
    println ("Workflow is: " + w.mkString)
    acc += "Running: "
    println (">>>>>>>> RESULT is " + new Engine().exec(w, razie.base.scripting.ScriptFactory.mkContext(), ""))
    println (acc)
    println ("=========================================================")
  }
}
