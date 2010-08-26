/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.oldies.java.defn;

import java.util.ArrayList;
import java.util.List;

import razie.base.scripting.ScriptContext;
import razie.wf.oldies.java.inst.WfActivity;


public interface WfListDefn extends WfActivityDefn {

   /**
    * @associates com.razie.wf.defn.WfLink
    * @directed directed
    * @link aggregationByValue
    * @supplierCardinality 0..*
    */
   List<WfLink> getInboundLinks();

   /**
    * @associates com.razie.wf.defn.WfLink
    * @directed directed
    * @link aggregationByValue
    * @supplierCardinality 0..*
    */
   List<WfLink> getOutboundLinks();

   /**
    * @associates com.razie.wf.defn.WfActivity
    * @directed directed
    * @link aggregationByValue
    * @supplierCardinality 0..*
    */
   public List<WfActivityDefn> getActivities();

   public void add(WfActivityDefn defn);

   public class Impl extends WfActivityDefn.Impl implements WfListDefn {
      public List<WfActivityDefn> activities = new ArrayList<WfActivityDefn>();
      public List<WfLink> inboundLinks = new ArrayList<WfLink>();
      public List<WfLink> outboundLinks = new ArrayList<WfLink>();

      public Impl(Object... pairs) {
         super(pairs);
      }

      public List<WfActivityDefn> getActivities() {
         return this.activities;
      }

      /** gotta be like this, i guess all derived would do the same, if not they can overwrite */
      public WfActivity makeInstance(ScriptContext ctx) {
         WfActivity inst = new WfActivity(this);
         for (WfActivityDefn adefn : this.activities) {
            inst.activities.add(adefn.makeInstance(ctx));
         }
         return inst;
      }

      public void add(WfActivityDefn defn) {
         this.activities.add(defn);
      }

      public void exec(WfActivity activity, ScriptContext ctx) {
         // TODO i'll make it a sequence for now
         for (WfActivity a : activity.activities) {
            a.exec(ctx);
         }
      }

      public List<WfLink> getInboundLinks() {
         return this.inboundLinks;
      }

      public List<WfLink> getOutboundLinks() {
         return this.outboundLinks;
      }
   }
}