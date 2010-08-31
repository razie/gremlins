/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables;

import java.util.List
import razie.base.AttrAccess

/** represents a collection of actionables, i.e. the methods of an object
 *
 * @author razvanc
 */
trait HasActionables {
   import scala.collection.JavaConversions._
   
   def actionables : Map[String, Actionable]
   def jactionables() : java.util.Map[String, Actionable] = actionables
}

/** just to have HasActionable as a Trait - cannot use scala code from java code */
trait THasActionables extends HasActionables {
  type fun = (AttrAccess) => AttrAccess
}
