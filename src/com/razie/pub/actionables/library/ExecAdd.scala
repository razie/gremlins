package com.razie.pub.actionables.library

import com.razie.pub.actionables._
import com.razie.pub.actionables.THasActionables
import razie.base.AttrAccess

/** good for testing: concat all arguments SORTED by name and return result */
class ExecAdd () extends com.razie.pub.actionables.IExecutable {

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

    override def execute(in:AttrAccess):AttrAccess = {
	  var sum:String = ""
  
      for (val y <- in.getPopulatedAttr.toList.sort((a,b) => a.toString < b.toString)) 
        sum = sum + (in a y)
      razie.AA("result", sum)
	}
}
