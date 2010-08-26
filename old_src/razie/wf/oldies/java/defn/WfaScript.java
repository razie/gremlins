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


/**
 * 
 * just execute a script - the results are ignored. You can use Log just as well, i guess...
 * 
 * @author razvanc
 * 
 */
public class WfaScript extends WfActivityDefn.Impl {
   RazScript script;

   public WfaScript(Object... pairs) {
      super(pairs);
      this.script = Script.toScript(this.defnParms.getAttr("script"));
   }

   public void exec(WfActivity activity, ScriptContext ctx) {
      this.script.eval(ctx);
      activity.status = WfActivity.Status.SUCC;
   }
}
