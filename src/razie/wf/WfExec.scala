/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.base.{ActionContext => AC}
import razie.actionables._

/** this is the actual work carried out by the respective action */
trait WfExec extends WFunc[Any] {
//  override  def exec  (in:AC, prevValue:Any) : Any = exec (prevValue)
  def wname : String = "?"
}

trait WfLibrary[T] { 
  // wraps an WfExec into a WfAct...customize this per implementation
  def wrap (e:WfExec) : T
}
