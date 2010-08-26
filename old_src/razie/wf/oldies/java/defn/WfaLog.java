/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.oldies.java.defn;

import razie.base.scripting.RazScript;
import razie.base.scripting.ScriptContext;
import razie.wf.oldies.java.inst.Script;
import razie.wf.oldies.java.inst.WfActivity;

import com.razie.pub.base.log.Log;

/**
 * log an expression or a message
 * 
 * @author razvanc
 * 
 */
public class WfaLog extends WfActivityDefn.Impl implements WfActivityDefn {
   RazScript script;
   String msg;

   public WfaLog(Object... pairs) {
      super(pairs);
      this.script = Script.toScript(this.defnParms.getAttr("script"));
      this.msg = (String) this.defnParms.getAttr("msg");
   }

   public void exec(WfActivity activity, ScriptContext ctx) {
      if (this.msg != null)
         Log.logThis(this.msg);
      else if (this.script != null)
         Log.logThis(this.script.eval(ctx).toString());
      else
         Log.logThis("DEFAULT LOG MESSAGE - Log activity missing both script and msg");

      activity.status = WfActivity.Status.SUCC;
   }
}
