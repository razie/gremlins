/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package com.razie.wf.defn;

import razie.base.scripting.RazScript;
import razie.base.scripting.ScriptContext;

import com.razie.wf.inst.Script;
import com.razie.wf.inst.WfActivity;

/**
 * conditional branch
 * 
 * @author razvanc
 * 
 */
public class WfaIF extends WfListDefn.Impl {

   RazScript cond;
   // just for reflection of properties
   WfActivityDefn thenBranch;
   WfActivityDefn elseBranch;

   public WfaIF(WfActivityDefn thenBranch, WfActivityDefn elseBranch, Object... pairs) {
      super(pairs);
      this.cond = Script.toScript(this.defnParms.getAttr("cond"));
      this.thenBranch = thenBranch;
      this.elseBranch = elseBranch;
      this.add(thenBranch);
      this.add(elseBranch);
   }

   public void exec(WfActivity activity, ScriptContext ctx) {
      Object result = this.cond.eval(ctx);
      if (result != "false") {
         activity.activities.get(0).exec(ctx);
      } else {
         activity.activities.get(1).exec(ctx);
      }
   }
}
