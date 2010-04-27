/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.oldies.java.eng;

import razie.base.scripting.ScriptContext;
import razie.wf.oldies.java.inst.WfInst;


/**
 * workflow engine
 * 
 * TODO detailed docs
 * 
 * @author razvanc
 */
public class Engine {
   void exec(WfInst inst, ScriptContext ctx) {
      inst.exec(ctx);
   }

   public static Engine makeEngine() {
      return new Engine();
   }
}
