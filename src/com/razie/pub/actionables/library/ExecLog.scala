package com.razie.pub.actionables.library;

import com.razie.pub.actionables.ActionableSpec;
import com.razie.pub.actionables.IExecutable;
import razie.base.AttrAccess;
import com.razie.pub.base.log.Log;

/**
 * stub - will print stuff
 * 
 * @author razvanc
 */
class ExecLog extends IExecutable {

	def spec : ActionableSpec = new ActionableSpec(
			razie.AA("msg=missing"), razie.AA("result"));

   override def execute(in:AttrAccess):AttrAccess = {
      val msg = in sa "msg"
  
		Log logThis msg

		razie.AA("result", msg);
	}

}
