package razie.actionables;

import razie.base.ActionContext;
import razie.base.ActionItem;
import razie.base.AttrAccess;

/**
 * needed a base class for the actionable concept
 * 
 * TODO 1-1 detailed docs
 * 
 * @author razvanc
 */
class Actionable (
      val name:String, 
      val item:ActionItem, 
      val inArgs:AttrAccess, 
      val executable:IExecutable) {
   
	def execute () : Any = {
		this.executable.apply(inArgs, null);
	}
	
	def jexecutable() = executable
	def jinArgs() = inArgs
}
