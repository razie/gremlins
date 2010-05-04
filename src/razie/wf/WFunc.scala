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

