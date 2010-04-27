/*
 * Razvan's public code. Copyright 2008 based on Apache license (share alike) see LICENSE.txt for
 * details.
 */
package razie.actionables

import razie.actionables.library._

/** this is the fully functional factory. you can register categories in here */
object ScalaExecFactory extends ExecutableFactory {
   
   var factories = scala.collection.mutable.Map[String, ExecutableFactory] ()

   // we can register this by default
   ScalaExecFactory.reg ("simple", new RazbaseExecutables)
     
   /** this implementation will figure out the domain and delegate to that factory
    *
    * @param unifiedstring
    * @return
    */
   override def make(unifiedstring:String) : IExecutable = {
     val ActFactory.pat(domain, cmd, parens, args) = unifiedstring

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
