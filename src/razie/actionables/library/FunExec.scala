package razie.actionables.library

import razie.actionables._
import razie.actionables.THasActionables
import razie.base._

/** simple executable wrapping a simple scala functional - use it to overload stuff in code*/
class FunExec (val fun : (ActionContext)=>AnyRef) extends razie.actionables.IExecutable {
	override def exec(in:ActionContext, v:Any):Any = {
	  fun (in)
	}
}
