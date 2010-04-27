package razie.actionables;

import razie.base.AttrAccess;

/**
 * the specification for an actionable...
 * 
 * TODO find proper use
 * 
 * @author razvanc
 */
public class ActionableSpec {

	public AttrAccess inArgs;
	public AttrAccess outArgs;

	public ActionableSpec(AttrAccess in, AttrAccess out) {
		this.inArgs = in;
		this.outArgs = in;
	}
}
