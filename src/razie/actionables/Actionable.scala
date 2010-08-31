/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables;

import razie.base.ActionContext;
import razie.base.ActionItem;
import razie.base.AttrAccess;

/**
 * needed a base class for the actionable concept
 * 
 * An Actionable is a ready-to-invoke executable item. Whenever you feel like it, just trigger it.
 * 
 * Note that the AttrAccess input parms also define the context in which this is called. 
 * 
 * TODO maybe expose the context explicitely?
 * 
 * @author razvanc
 */
class Actionable (
      val name:String, 
      val item:ActionItem, 
      val inArgs:AttrAccess, 
      val executable:razie.wf.JWFunc) {
   
	def execute () : Any = {
		this.executable.apply(inArgs, null);
	}
	
	def jexecutable() = executable
	def jinArgs() = inArgs
}

/** just to have Actionable as a Trait - cannot use scala code from java code */
trait TActionable extends Actionable { }
