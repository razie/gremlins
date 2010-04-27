/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.base.ActionContext

/** basic executable/actionable interface  */
trait WFunc[T] { // extends PartialFunc ?
  def exec (in:ActionContext, prevValue:Any) : T
}

trait JWFunc extends WFunc[Any] {
  override def exec (in:ActionContext, prevValue:Any) : Any
}
