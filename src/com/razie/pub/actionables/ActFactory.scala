/*
 * Razvan's public code. Copyright 2008 based on Apache license (share alike) see LICENSE.txt for
 * details.
 */
package com.razie.pub.actionables

import com.razie.pub.actionables.library.ScalaExecFactory
import razie.base.AttrAccess
import razie.base.ActionItem
import org.w3c.dom.Element
//import com.razie.pub.base.data._

/** factory for the basic executables offered 
 */
object ActFactory extends ActionableFactory {
   val pat = """(\w+):(\w+)[ \?](.*)""".r
  
   ActionableFactory.init (this)
   
   override def make(name:String, unifiedstring:String) : Actionable = {
      val pat(domain, cmd, args) = unifiedstring

      val ex = ScalaExecFactory.make (unifiedstring)
      val in = razie.AA(args)
     
      make(name, new ActionItem(name), in, ex);
   }

	override def make(name:String, ai:ActionItem, in:AttrAccess,
                     ex:IExecutable) : Actionable = {
      new Actionable (name, ai, in, ex);
   }
 
}
