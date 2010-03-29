/*
 * Razvan's public code. Copyright 2008 based on Apache license (share alike) see LICENSE.txt for
 * details.
 */
package com.razie.pub.actionables.library

import com.razie.pub.actionables.ExecutableFactory
import com.razie.pub.actionables.IExecutable
import com.razie.pub.actionables.ActFactory

/** this is the fully functional factory. you can register categories in here */
object ScalaExecFactory extends ExecutableFactory {
   // TODO rename back from TEMP when compiler allows...had some issues with scala plugin
   
   var factories = scala.collection.mutable.Map[String, ExecutableFactory] ()

   // we can register this by default
   ScalaExecFactory.reg ("raziecmd", new RazbaseExecutables)
     
   /** this implementation will figure out the domain and delegate to that factory
    *
    * @param unifiedstring
    * @return
    */
   override def make(unifiedstring:String) : IExecutable = {
     val ActFactory.pat(domain, cmd, args) = unifiedstring

     factories.get(domain) match {
       case Some(fact) => fact.make(unifiedstring)
       case None => throw new IllegalArgumentException ("couldn't find domain factory for: " +unifiedstring)
       }
     }

   /** 
    * @return the domains currently registered
    */
   override def domains() : Array[String] = {
      // TODO implement this
     Array("1")
     }

   /** 
    * @param domain
    * @return the actions in this domain
    */
   override def commands(domain:String) : Array[String] = {
      // TODO implement this
     Array("1")
      }

   /** register a new factory for a category */
   def reg (domain:String, f:ExecutableFactory) = 
     factories.put (domain,f);
 }
