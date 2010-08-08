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
public class Actionable {

	public String name;
	public ActionItem item;
	public AttrAccess inArgs;
	public IExecutable executable;

	public Actionable(String name, ActionItem ai, AttrAccess in,
			IExecutable ex) {
		this.name = name;
		this.item = ai;
		this.inArgs = in;
		this.executable = ex;
	}

	public Object execute() {
		return this.executable.apply(inArgs, null);
	}
}
