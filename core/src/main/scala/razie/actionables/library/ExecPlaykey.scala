/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables.library

import razie.actionables._
import razie.actionables.THasActionables
import com.razie.pub.base._
import razie.base._

import java.awt.AWTException;
import java.awt.Robot;
import java.awt.event.KeyEvent;

/** play a key */
class ExecPlaykey () extends razie.gremlins.JWFunc {
	def spec = new ActionableSpec(razie.AA("key=A,ctrl=no,shift=no"), razie.AA("result"));

   override def apply(in:ActionContext, v:Any):Any = {
      var res = true
      val k=(in a "key").asInstanceOf[String]
     
      // how to turn into code?
      val key : Int = k(0)
    
      try {
         val r = new Robot
            
         if ((in a "ctrl") != null)
         r.keyPress(KeyEvent.VK_CONTROL);
      

         if ((in a "shift") != null)
         r.keyPress(KeyEvent.VK_SHIFT);
      
         r.keyPress(key);
         r.keyRelease(key);

         if ((in a "shift") != null)
         r.keyRelease(KeyEvent.VK_SHIFT);

         if ((in a "ctrl") != null)
         r.keyRelease(KeyEvent.VK_CONTROL);
      } catch {
         case e:AWTException => res=false
      }
        
      res.toString
	}
}
