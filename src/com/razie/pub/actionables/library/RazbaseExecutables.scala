/*
 * Razvan's public code. Copyright 2008 based on Apache license (share alike) see LICENSE.txt for
 * details.
 */
package com.razie.pub.actionables.library

import com.razie.pub.actionables.ExecutableFactory
import com.razie.pub.actionables.IExecutable
import com.razie.pub.actionables.ActFactory

 /** factory for the basic executables offered 
*/ 
 class RazbaseExecutables extends ExecutableFactory {
   
   /** make an executable
    *
    * @param unifiedstring
    * @return
    */
   override def make(unifiedstring:String) : IExecutable = {
     val ActFactory.pat(domain, cmd, args) = unifiedstring
     
     require (domains contains domain, "need to support domain") 
     require (commands(domain) contains cmd, "need to support command: "+cmd) 
     
     cmd match {
       case "log" => new ExecLog
       case "telnet" => new ExecTelnet
       case "add" => new ExecAdd
       case "playkey" => new ExecPlaykey
       case "cmdline" => new ExecCmdline
       }
     }

   /** 
    * @return the domains currently registered
    */
   override def domains() : Array[String] =  Array("raziecmd")

   /** 
    * @param domain
    * @return the actions in this domain
    */
   override def commands(domain:String) : Array[String] = 
     Array("log", "telnet", "add", "playkey", "cmdline")

 }
