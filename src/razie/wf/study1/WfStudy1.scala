package razie.wf.study1

import razie.AA
import razie.base.{ActionContext => AC}


/** 
 * just playing with the syntax. this implementation is simple mapping to scala. It is not good
 * because it's lacking the model building and visualization.
 * 
 * although visualization could be added, also, state persistancy and distribution are not easy, possibly
 * via continuations a la swarm
 * 
 * interestingly, in this case, state doesn't make much sense, since activities are strait code...
 * 
 * @author razvanc
 */
object Wf1 {
  //  type Cond : => Boolean
   
  def wif (cond : => Boolean) (t:WfAct) = WfIf (()=>cond, t)
  implicit def wf  (f : => Unit) = WfScala (()=>f)

}

/** starting to add syntax niceties, implementation-transparent */
trait WfAct extends WfExec {
  def + (e:WfAct) = WfSeq (this,e)
  def | (e:WfAct) = WfPar (this,e)
}

  class wabase[T<:WfExec] {
    def log (f: => String) = new WfExec { override def exec (x:Any) = println(f) }
  }

  case class WfProxy (proxied:WfAct) extends WfAct { 
     override def exec (in:AC, prevValue:Any) : Any = proxied.exec(in, prevValue)
  }
  
  case class WfSeq (a:WfAct*) extends WfAct {
     override def exec (in:AC, prevValue:Any) : Any = a.foldLeft(prevValue)((x,y)=>y.exec(in, x))
  }
  case class WfPar (a:WfAct*) extends WfAct {
     // TODO not right :)
     override def exec (in:AC, prevValue:Any) : Any = a.foldLeft(prevValue)((x,y)=>y.exec(in, x))
  }

  case class WfScala (val f : () => Unit) extends WfAct { 
     override def exec (in:AC, prevValue:Any) : AC = { f(); in }
  }   

  case class WfElse (a:WfAct)  extends WfProxy (a)
  
  case class WfIf (val cond : () => Boolean, t:WfAct, var e:WfElse*) extends WfAct {
     override def exec (in:AC, prevValue:Any) : Any = {
        if (cond()) 
           t.exec(in, prevValue)
        else 
           e.firstOption.map(_.exec(in, prevValue)).getOrElse(in)
     }
     
     def welse (a:WfAct) = { this.e = WfElse (a); this}
  }
 
  // very simple engine, eh?
  class Engine {
     def exec (start:WfAct, ctx:AC) =  start.exec(ctx, null)
  }
  

object Wf1Main extends Application {
  import Wf1._
   
  // test
  val wd = wif (1==2)({
     println ("it's")
     println ("true")
  }).welse({
     println ("it's")
     println ("false")
  })
  
  println ("pinkie")
  new Engine().exec(wd, razie.base.scripting.ScriptFactory.mkContext())
}
