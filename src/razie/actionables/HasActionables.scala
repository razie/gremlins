package razie.actionables;

/** represents a collection of actionables, i.e. the methods of an object
 *
 * @author razvanc
 */
trait HasActionables {
   import scala.collection.JavaConversions._
   
   def actionables : Map[String, Actionable]
   def jactionables() : java.util.Map[String, Actionable] = actionables
}
