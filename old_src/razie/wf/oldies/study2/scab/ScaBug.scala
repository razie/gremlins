package razie.wf.study2.scab

object ScaBug1 extends App {
   var acc = ""
      
   case class C1 (val f:() =>Unit) {
      def doit () = f()
   }

  def wif (c: =>Boolean) (t:C1) = t
  implicit def wf  (f : => Unit) = C1 (()=>f)

  val d = wif (1==1) {
     acc += "1"
     acc += "2"
  }

  acc += "start"
  d.doit()
  acc
}

object ScaBug2 extends App {
  trait WfAct {
    def exec()
  }

  /** simple activities just do their thing */
  case class WfSimple extends WfAct { 
    override def exec() = {}
  }

  case class WfScala (val f : () => Unit) extends WfSimple { 
     override def exec () = { f(); }
  }   
  
  case class WfIf   (val cond : () => Boolean, t:WfAct, var e:WfElse*) extends WfAct {
   
    /** return either branch, depending on cond */
    def exec () =
      if (cond()) 
        t.exec()
      else
        e.firstOption.map(_.exec())
        
     def welse (a:WfAct) = {this.e = WfElse (a); this}
  }
  
  case class WfProxy (a:WfAct) extends WfAct { 
    override def exec () = a.exec()
  }

  case class WfElse (t:WfAct)  extends WfProxy (t) {
  }

  def wif (cond : => Boolean) (t:WfAct) = WfIf (()=>cond, t)
  implicit def wf  (f : => Unit) = WfScala (()=>f)

  val d = wif (1==2) {
     println("1")
     println("2")
  } welse {
     println("3")
     println("4")
  }

  d.exec()
}
