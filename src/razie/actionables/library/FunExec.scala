/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables.library

import razie.actionables._
import razie.actionables.THasActionables
import razie.base._

/** simple executable wrapping a simple scala functional - use it to overload stuff in code*/
class FunExec (val fun : (ActionContext)=>AnyRef) extends razie.wf.JWFunc{
	override def apply(in:ActionContext, v:Any):Any = {
	  fun (in)
	}
}
