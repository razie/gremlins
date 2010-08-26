package razie.wf.study2

import razie.base.ActionContext

/** this is the actual work carried out by the respective action */
trait WfActBase {
  def exec  (in:ActionContext) : ActionContext   = {execu(in); in}
  def execu (in:ActionContext) : Unit            = execu()
  def execu ()                 : Unit            = {/*nada*/}
}
