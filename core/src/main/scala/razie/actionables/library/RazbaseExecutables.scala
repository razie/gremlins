/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables.library

import razie.actionables._

/** factory for the basic executables offered  
 * 
 * @author razvanc
 */
object RazbaseExecutables extends ExecutableFactory {
   
   /** make an executable
    *
    * @param unifiedstring
    * @return
    */
   override def make(unifiedstring:String) : razie.gremlins.JWFunc = {
     val Executables.pat(domain, cmd, parens, args) = unifiedstring
     
     require (cmds contains cmd, "need to support command: "+cmd) 

     cmd match {
       case "log" => new ExecLog
       case "telnet" => new ExecTelnet
       case "add" => new ExecAdd
       case "playkey" => new ExecPlaykey
       case "cmdline" | "shell" => new ExecCmdline
       case "pipe" => new ExecCmdline (true)
       }
     }

   /** 
    * @param domain
    * @return the actions in this domain
    */
   override def commands() : Array[razie.AI] =  cmds map (new razie.AI(_))
     
   val cmds = Array("log", "telnet", "add", "playkey", "cmdline", "shell", "pipe") 

 }
