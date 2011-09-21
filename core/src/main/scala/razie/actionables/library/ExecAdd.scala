/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables.library

import razie.actionables._
import razie.actionables.THasActionables
import razie.base._

/** good for testing: concat all arguments SORTED by name and return result */
class ExecAdd () extends razie.gremlins.JWFunc {
  import scala.collection.JavaConversions._

  override def apply(in:ActionContext, v:Any):Any = {
     // java thinking
	 var sum:String = ""
  
    for (y <- in.getPopulatedAttr.toList.sort((a,b) => a.toString < b.toString)) 
      sum = sum + (in a y)
    sum
     
     // scala thinking
//    in.getPopulatedAttr.toList.sort((a,b) => a.toString < b.toString) foldRight ("") (_ + (in a __)
  }
}
