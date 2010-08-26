/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.oldies.java.defn;

import razie.base.scripting.ScriptContext;
import razie.wf.oldies.java.inst.WfActivity;


/**
 * log an expression or a message
 * 
 * @author razvanc
 * 
 */
public class WfaGOTO extends WfActivityDefn.Impl implements WfActivityDefn {
   WfActivityDefn target;

   public WfaGOTO(WfActivityDefn target, Object... pairs) {
      super(pairs);
      this.target = target;
   }

   public void exec(WfActivity activity, ScriptContext ctx) {
      this.target.exec(activity, ctx);
      activity.status = WfActivity.Status.SUCC;
   }
}
