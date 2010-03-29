package com.razie.pub.actionables.library

import com.razie.pub.actionables._
import com.razie.pub.actionables.THasActionables
import razie.base.AttrAccess

import java.awt.AWTException;
import java.awt.Robot;
import java.awt.event.KeyEvent;
import com.razie.pub.actionables.util.WinExec

/** play a key */
class ExecCmdline () extends com.razie.pub.actionables.IExecutable {
	def spec = new ActionableSpec(razie.AA("cmd=uname -A"), razie.AA("result"));

    override def execute(in:AttrAccess):AttrAccess = {
      val res = "?"

      // TODO figure out implementation per OS...i.e. unix vs windows
      WinExec.execCmd (in sa "cmd")
      
      razie.AA ("result", res.toString)
	}
}
