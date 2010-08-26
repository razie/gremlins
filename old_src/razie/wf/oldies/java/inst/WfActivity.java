/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.oldies.java.inst;

import java.util.ArrayList;
import java.util.List;

import razie.base.scripting.ScriptContext;
import razie.wf.oldies.java.defn.WfActivityDefn;


public class WfActivity {

   public State state = State.NOTSTARTED;
   public Status status = Status.UNKNOWN;

   private WfActivityDefn defn;
   /**
    * @associates com.razie.wf.inst.WfActivity
    * @directed directed
    * @link aggregationByValue
    * @supplierCardinality 0..*
    */
   public List<WfActivity> activities = new ArrayList<WfActivity>();

   public WfActivity(WfActivityDefn defn) {
      this.defn = defn;
   }

   public void exec(ScriptContext ctx) {
      this.state = State.INPROGRESS;
      this.defn.exec(this, ctx);
      this.state = State.DONE;
   }

   public static enum State {
      NOTSTARTED, INPROGRESS, DONE
   }

   public static enum Status {
      UNKNOWN, SUCC, FAILED, INDEFINITE
   }
}
