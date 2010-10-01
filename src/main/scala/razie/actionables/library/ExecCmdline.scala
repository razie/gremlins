/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables.library

import razie.actionables._
import razie.actionables.THasActionables
import razie.base._

import java.awt.AWTException;
import java.awt.Robot;
import java.awt.event.KeyEvent;
import razie.actionables.util.WinExec

/** run a command line - windows and unix */
class ExecCmdline (wait:Boolean = false) extends razie.gremlins.JWFunc {
	def spec = new ActionableSpec(razie.AA("cmd=uname -A"), razie.AA("result"));

    override def apply(in:ActionContext, v:Any):Any = {
      var res : Any = "?"

      val cmd = if (in isPopulated "cmd") in sa "cmd" else if (v == null) "" else v.toString
      
      // TODO figure out implementation per OS...i.e. unix vs windows
      if (wait)
         res = WinExec.execAndWait (cmd)
      else
         WinExec.execCmd (cmd)
      
      res.toString
	}
}
