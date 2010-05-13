/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.base.ActionContext

/** basic executable/actionable interface  */
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

/** deser is assumed via DSL */
trait HasDsl /*extends GReferenceable*/ {
  def serialize : String = toDsl
  
  /* serialize the DEFINITION 
   * 
   * this must be the same format as the DSL one-liner or multiple lnes (for structured)
   */
  def toDsl : String 
}

/** TODO could be better */
trait notisser /*extends GReferenceable*/ extends HasDsl {
  override def toDsl : String  = throw new UnsupportedOperationException ("class notisser")
}

/** TODO could be better */
trait sertodo /*extends GReferenceable*/ extends HasDsl {
  override def toDsl : String  = throw new UnsupportedOperationException ("serialization is TODO")
}

