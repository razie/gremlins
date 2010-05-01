package razie.actionables.library

import razie.actionables._
import razie.actionables.THasActionables
import razie.base._

/** good for testing: concat all arguments SORTED by name and return result */
class ExecAdd () extends razie.actionables.IExecutable {
  import scala.collection.JavaConversions._

  override def exec(in:ActionContext, v:Any):Any = {
	 var sum:String = ""
  
    for (val y <- in.getPopulatedAttr.toList.sort((a,b) => a.toString < b.toString)) 
      sum = sum + (in a y)
    sum
  }
}
