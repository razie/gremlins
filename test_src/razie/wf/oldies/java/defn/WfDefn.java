/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.oldies.java.defn;

import razie.base.scripting.ScriptContext;
import razie.wf.oldies.java.inst.WfActivity;
import razie.wf.oldies.java.inst.WfInst;


public interface WfDefn extends WfListDefn {

   public class Impl extends WfListDefn.Impl implements WfDefn {
      public Impl(Object... pairs) {
         super(pairs);
      }

      public WfActivity makeInstance(ScriptContext ctx) {
         WfInst inst = new WfInst(this, ctx);
         for (WfActivityDefn adefn : this.activities) {
            inst.activities.add(adefn.makeInstance(ctx));
         }
         return inst;
      }

      public void exec(WfActivity activity, ScriptContext ctx) {
         // TODO i'll make it a sequence for now
         for (WfActivity a : activity.activities) {
            a.exec(ctx);
         }
      }
   }
}
