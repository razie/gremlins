package com.razie.pub.actionables.library

import com.razie.pub.actionables._
import com.razie.pub.actionables.THasActionables
import razie.base.AttrAccess

/** simple executable wrapping a simple scala functional - use it to overload stuff in code*/
class FunExec (val fun : (AttrAccess)=>AttrAccess) extends com.razie.pub.actionables.IExecutable {
	override def execute(in:AttrAccess):AttrAccess = {
	  fun (in)
	}
}
