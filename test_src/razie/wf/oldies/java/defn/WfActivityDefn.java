/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.oldies.java.defn;

import razie.base.scripting.ScriptContext;
import razie.base.scripting.ScriptFactory;
import razie.wf.oldies.java.inst.WfActivity;


/**
 * instances of this are actual definitions
 * 
 */
public interface WfActivityDefn {
   ScriptContext getDefnParms();

   WfActivity makeInstance(ScriptContext ctx);

   void exec(WfActivity activity, ScriptContext ctx);

   public abstract class Impl implements WfActivityDefn {
      ScriptContext defnParms = ScriptFactory.mkContext();

      public Impl() {
      }

      public Impl(Object... pairs) {
         this.defnParms.setAttr(pairs);
      }

      public WfActivity makeInstance(ScriptContext ctx) {
         return new WfActivity(this);
      }

      public ScriptContext getDefnParms() {
         return this.defnParms;
      }
   }
}
