package razie

import razie.AA

object WfTests {

}

trait WfActBase {
  def exec  (in:AA) : AA = {execu(in); in}
  def execu (in:AA) : Unit = execu()
  def execu ()      : Unit = {/*nada*/}
}

class wabase[T<:WfActBase] {
  def log (f: => String) = new WfActBase { override def execu () = println(f) }
}

object Wf1 {
//  type Cond : => Boolean
 
  def wif (cond : => Boolean) (t:WfAct) = WfIf (()=>cond, t)
  implicit def wf  (f : => Unit) = WfScala (()=>f)

  trait WfAct extends WfActBase {
     def welse (e:WfAct) = WfElse (e)
     def wf  (f : => Unit) = WfScala (()=>f)
     
     def + (e:WfAct) = WfSeq (this,e)
     def | (e:WfAct) = WfPar (this,e)
  }

  case class WfProxy (proxied:WfAct) extends WfAct { 
     override def exec (in:razie.AA) : razie.AA = proxied.exec(in)
  }
  
  case class WfSeq (a:WfAct, b:WfAct) extends WfAct {
     override def exec (in:razie.AA) : razie.AA = b.exec(a.exec(in))
  }
  case class WfPar (a:WfAct, b:WfAct) extends WfAct {
     override def exec (in:razie.AA) : razie.AA = b.exec(a.exec(in))
  }

  case class WfScala (val f : () => Unit) extends WfAct { 
     override def exec (in:razie.AA) : razie.AA = { f; in }
  }   

  case class WfElse (a:WfAct)  extends WfProxy (a)
  
  case class WfIf (val cond : () => Boolean, t:WfAct, e:WfElse*) extends WfAct {
     override def exec (in:razie.AA) : razie.AA = {
        if (cond()) 
           t.exec(in)
        else 
           e.firstOption.map(_.exec(in)).getOrElse(in)
     }
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
  Wf1.wd.exec(razie.AA())
}
