package razie.wf.study2

import razie.base.ActionContext

/** basic executable/actionable */
trait WFunc[T] { // extends PartialFunc ?
  def exec (in:ActionContext, prevValue:Any) : T
}

/** this is the actual work carried out by the respective action */
trait WfExec extends WFunc[Any] {
  override  def exec  (in:ActionContext, prevValue:Any) : Any = exec (prevValue)
  protected def exec  (prevValue:Any)                   : Any = { execu(); prevValue} /* default does nothing */
  protected def execu ()                                : Unit = {} /* default does nothing */
            def wname : String = "?"
}

class WfAssign (name:String) (v: Any => Any) extends WfExec {
  override def exec (in:ActionContext, prevValue:Any) = { 
    in.set (name, v(prevValue))
    prevValue
  }
  
  override def wname = "assign"
}

class WfLog (msg : (ActionContext, Any) => String) extends WfExec { 
  override def exec  (in:ActionContext, prevValue:Any) : Any = println (msg(in, prevValue))
  override def wname = "log"
}
