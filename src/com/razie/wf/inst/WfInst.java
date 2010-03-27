/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package com.razie.wf.inst;

import razie.base.scripting.ScriptContext;

import com.razie.wf.defn.WfActivityDefn;
import com.razie.wf.defn.WfDefn;

public class WfInst extends WfActivity {

   public WfInst(WfActivityDefn defn, ScriptContext ctx) {
      super(defn);
      if (!(defn instanceof WfDefn)) {
         throw new IllegalArgumentException("my defn must be a WfDefn - right now it's a "
               + defn.getClass().getName());
      }
      this.myContext = ctx;
      this.defn = (WfDefn) defn;
   }

   /**
    * @link aggregationByValue
    */

   private razie.base.scripting.ScriptContext myContext;
   private WfDefn defn;
}
