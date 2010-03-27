/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package com.razie.wf;

import razie.base.scripting.ScriptContext;
import razie.base.scripting.ScriptFactory;

import com.razie.wf.defn.WfDefn;
import com.razie.wf.defn.WfaIF;
import com.razie.wf.defn.WfaLog;
import com.razie.wf.inst.WfInst;

public class MainWf {

   public static void main(String[] argv) {
      run(makeSimpleProcess());
      run(makeIf());
   }

   public static void run(WfDefn defn) {
      ScriptContext ctx = ScriptFactory.mkContext();

      WfInst inst = (WfInst) defn.makeInstance(ctx);
      inst.exec(ctx);
   }

   private static WfDefn makeSimpleProcess() {
      WfDefn defn = new WfDefn.Impl();
      defn.add(new WfaLog("msg", "a message to log"));

      return defn;
   }

   private static WfDefn makeIf() {
      WfDefn defn = new WfDefn.Impl("gigi", "a bastard...");
      defn
            .add(new WfaIF(new WfaLog("msg", "true branch"), new WfaLog("msg", "false branch"), "cond",
                  "gigi"));

      return defn;
   }
}
