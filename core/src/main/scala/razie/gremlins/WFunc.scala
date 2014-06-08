/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins

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

/** test the value against another and return the result */
case class WFCMP[T <: Any] (to:T, how: (Any,Any) => Boolean = {(a,b)=> a == b}) extends WFunc[Boolean] {
  override def apply (in:ActionContext, prevValue:Any) : Boolean = how(prevValue, to)
}

/** java friendly WFunc interface */
trait JWFunc extends WFunc[Any] {
  override def apply (in:ActionContext, prevValue:Any) : Any
}

//-------------------- serialization: not the best idea...but

/** deserialization is assumed via DSL 
 *  
 *  the idea is that all activities would have an external DSL form as well
 *  and can serialize themselves in that form
 *  
 *  serialize the DEFINITION only - not including states/values
 */
trait HasDsl /*extends GReferenceable*/ {
  def serialize : String = toDsl
  
  /** serialize the DEFINITION - not including 
   */
  def toDsl : String 
  def toIndentedDsl (indent:Int=>String, level:Int) = indent(level) + toDsl
}

/** 
 * if A extends B which HasDsl but not is serializable, tag it with this
 */
trait NoDsl extends HasDsl {
  override def toDsl : String  = throw new UnsupportedOperationException ("class notisser")
  override def toIndentedDsl (indent:Int=>String, level:Int) = throw new UnsupportedOperationException ("class notisser")
}

/** 
 *  need to implement serialization
 */
trait TodoDsl /*extends GReferenceable*/ extends HasDsl {
  override def toDsl : String  = throw new UnsupportedOperationException ("serialization is TODO")
}

//--------------------------- library stuff

/** bridge the WFunc to the WfActivity */
trait WfExec extends WFunc[Any] {
  def wname : String = this.getClass.getSimpleName
}

/** 
 *  libraries of executables should have this trait. 
 *  
 *  You can have lib[WfActivity] or lib[WfExec] or whatever 
 */
trait WfLibrary[T] { 
  /** wraps an WfExec into a WfActivity...customize this per implementation */
  def wrap (e:WfExec) : T
  
}

// TODO need interface for progress, to be implemented by activities that can do stuff that takes time
