package com.razie.pub.actionables;

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

	public AttrAccess execute() {
		return this.executable.execute(inArgs);
	}
}
