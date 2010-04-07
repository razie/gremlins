package razie.wf

import razie.base.ActionContext

/** this is the actual work carried out by the respective action */
trait WfActBase {
  
            def exec  (in:ActionContext, prevValue:Any) : Any = exec (prevValue)
  protected def exec  (prevValue:Any)                   : Any = { execu(); prevValue} /* default does nothing */
  protected def execu ()                                : Unit = {} /* default does nothing */
}
