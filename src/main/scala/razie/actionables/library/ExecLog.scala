/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables.library;

import razie.actionables.ActionableSpec;
import razie.base._
import com.razie.pub.base.log.Log;

/**
 * log a message
 * 
 * @author razvanc
 */
class ExecLog extends razie.gremlins.JWFunc {

	def spec : ActionableSpec = new ActionableSpec(
			razie.AA("msg=missing"), razie.AA("result"));

   override def apply(in:ActionContext, v:Any):Any = {
      val msg = in sa "msg"
  
		Log logThis msg

		msg
	}

}
