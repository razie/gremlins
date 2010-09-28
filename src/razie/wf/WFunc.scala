/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.base.ActionContext
import razie.actionables._

/** 
 * Basic executable/actionable interface. These process a default input value and return a default output value.
 * 
 * They are also invoked in a context - a set of objects in a certain role.
 * 
 * There are two major branches: WFunc and WfActivity. An action is a workflow specific thing and is aware of next actions, state of execution whatnot. It also does something so it's derived from WFunc. 
 * 
 * WFunc by itself only does something and is not stateful. Most activities are like that.
 */
trait WFunc[T <: Any] { // extends PartialFunc ?
  def apply (in:ActionContext, prevValue:Any) : T
}

/** use for defaults */
object WFuncNil extends WFunc[Any] {
  override def apply (in:ActionContext, prevValue:Any) : Any = prevValue
  
  def == (a:Any, b:Any) : Boolean = a == b
}

/** use for defaults */
case class WFCMP[T <: Any] (to:T, how: (Any,Any) => Boolean = WFuncNil.==) extends WFunc[Boolean] {
  override def apply (in:ActionContext, prevValue:Any) : Boolean = how(prevValue, to)
}

/** java friendly WFunc */
trait JWFunc extends WFunc[Any] {
  override def apply (in:ActionContext, prevValue:Any) : Any
}

//-------------------- serialization: not the best idea...but

/** deserialization is assumed via DSL */
trait HasDsl /*extends GReferenceable*/ {
  def serialize : String = toDsl
  
  /* serialize the DEFINITION 
   * 
   * this must be the same format as the DSL one-liner or multiple lnes (for structured)
   * 
   * TODO could be better
   */
  def toDsl : String 
  def toIndentedDsl (indent:Int=>String, level:Int) = indent(level) + toDsl
}

/** 
 * if A extends B which HasDsl but not is serializable, tag it with this
 * 
 * TODO could be better
 */
trait notisser /*extends GReferenceable*/ extends HasDsl {
  override def toDsl : String  = throw new UnsupportedOperationException ("class notisser")
  override def toIndentedDsl (indent:Int=>String, level:Int) = throw new UnsupportedOperationException ("class notisser")
}

/** need to implement serialization
 * 
 * TODO could be better
 */
trait sertodo /*extends GReferenceable*/ extends HasDsl {
  override def toDsl : String  = throw new UnsupportedOperationException ("serialization is TODO")
}

//--------------------------- library stuff

/** bridge the WFunc to the WfActivity */
trait WfExec extends WFunc[Any] {
  def wname : String = "?"
}

/** libraries of executables should have this trait. You can have lib[WfActivity] or lib[WfExec] or whatever */
trait WfLibrary[T] { 
  /** wraps an WfExec into a WfActivity...customize this per implementation */
  def wrap (e:WfExec) : T
  
}

// TODO need interface for progress, to be implemented by activities that can do stuff that takes time
