package razie.wf

import razie.AA
import razie.base.{ActionContext => AC}


/** 
 * just playing with the syntax. this implementation is simple mapping to scala. It is not good
 * because it's lacking the model building and visualization.
 * 
 * although visualization could be added, also, state persistancy and distribution are not easy, possibly
 * via continuations a la swarm
 * 
 * @author razvanc
 */
object Wf1 {
  //  type Cond : => Boolean
   
  class wabase[T<:WfActBase] {
    def log (f: => String) = new WfActBase { override def execu () = println(f) }
  }

 
  def wif (cond : => Boolean) (t:WfAct) = WfIf (()=>cond, t)
  implicit def wf  (f : => Unit) = WfScala (()=>f)

  trait WfAct extends WfActBase {
     def welse (e:WfAct) = WfElse (e)
     def wf  (f : => Unit) = WfScala (()=>f)
     
     def + (e:WfAct) = WfSeq (this,e)
     def | (e:WfAct) = WfPar (this,e)
  }

  case class WfProxy (proxied:WfAct) extends WfAct { 
     override def exec (in:AC) : AC = proxied.exec(in)
  }
  
  case class WfSeq (a:WfAct*) extends WfAct {
     override def exec (in:AC) : AC = a.foldLeft(in)((x,y)=>y.exec(x))
  }
  case class WfPar (a:WfAct*) extends WfAct {
     override def exec (in:AC) : AC = a.foldLeft(in)((x,y)=>y.exec(x))
  }

  case class WfScala (val f : () => Unit) extends WfAct { 
     override def exec (in:AC) : AC = { f; in }
  }   

  case class WfElse (a:WfAct)  extends WfProxy (a)
  
  case class WfIf (val cond : () => Boolean, t:WfAct, e:WfElse*) extends WfAct {
     override def exec (in:AC) : AC = {
        if (cond()) 
           t.exec(in)
        else 
           e.firstOption.map(_.exec(in)).getOrElse(in)
     }
  }
 
  // very simple engine, eh?
  class Engine {
     def exec (start:WfAct, ctx:AC) =  start.exec(ctx)
  }
  
  // test
  val wd = wif (1==2) {
     println ("then")
  } welse {
     println ("else")
  }
}

object Wf1Main extends Application {
  println ("pinkie")
  new Wf1.Engine().exec(Wf1.wd, razie.base.scripting.ScriptFactory.mkContext())
}
