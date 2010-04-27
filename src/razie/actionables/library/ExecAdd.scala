package razie.actionables.library

import razie.actionables._
import razie.actionables.THasActionables
import razie.base._

/** good for testing: concat all arguments SORTED by name and return result */
class ExecAdd () extends razie.actionables.IExecutable {

  implicit def javaIterableToScalaIterable[T](iterable:java.lang.Iterable[T]):Iterable[T] = {
    new Iterable[T] {
    	
    	//TODO elements is deprecated as of 2.8
      override def elements = {
        val jiterator = iterable.iterator();
        new Iterator[T] {
          override def next = jiterator.next()
          override def hasNext = jiterator.hasNext()
        }
      }
      
      override def iterator = elements
    }
  }

    override def exec(in:ActionContext, v:Any):Any = {
	  var sum:String = ""
  
      for (val y <- in.getPopulatedAttr.toList.sort((a,b) => a.toString < b.toString)) 
        sum = sum + (in a y)
      sum
	}
}
