package razie.actionables.library;

import razie.actionables.ActionableSpec;
import razie.actionables.IExecutable;
import razie.base._
import com.razie.pub.base.log.Log;

/**
 * stub - will print stuff
 * 
 * @author razvanc
 */
class ExecLog extends IExecutable {

	def spec : ActionableSpec = new ActionableSpec(
			razie.AA("msg=missing"), razie.AA("result"));

   override def apply(in:ActionContext, v:Any):Any = {
      val msg = in sa "msg"
  
		Log logThis msg

		msg
	}

}
