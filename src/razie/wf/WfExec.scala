/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.base.ActionContext

/** basic executable/actionable */
trait WFunc[T] { // extends PartialFunc ?
  def exec (in:ActionContext, prevValue:Any) : T
}

/** this is the actual work carried out by the respective action */
trait WfExec extends WFunc[Any] {
  override  def exec  (in:ActionContext, prevValue:Any) : Any = exec (prevValue)
  protected def exec  (prevValue:Any)                   : Any = { execu(); prevValue} /* default does nothing */
  protected def execu ()                                : Unit = {} /* default does nothing */
            def wname : String = "?"
}

trait WfLibrary[T] { 
  // wraps an WfExec into a WfAct...customize this per implementation
  def wrap (e:WfExec) : T
}

/** basic library of executables */
trait WfLib[T] extends WfLibrary[T] { 
  me =>

  // nop if you need an empty activity - maybe required by the syntax
  def nop  = me wrap new WfExec { override def execu() = {}; override def wname = "nop" }
  def todo = me wrap new WfExec { override def execu() = {}; override def wname = "TODO" }
  
  // logging
  def log (m: => String) = me wrap new WfLog ((x,y)=>m)
  def log (m: (ActionContext, Any)=> String) = me wrap new WfLog (m)
  
  // assign
  def assign (name:String) (value: =>Any) = me wrap new WfAssign (name)((x)=>value)

  // TODO scripted activities can migrate easily anywhere, eh?
  def script (name:String, lang:String, s:String) = todo
  
  // TODO
  def act (u:String) = todo
  
  // TODO
  def action (gref:String, action:String) = todo
  
}

class WfAssign (name:String) (v: Any => Any) extends WfExec {
  override def exec (in:ActionContext, prevValue:Any) = { 
    in.set (name, v(prevValue))
    prevValue
  }
  
  override def wname = "assign"
}

class WfLog (msg : (ActionContext, Any) => String) extends WfExec { 
  override def exec  (in:ActionContext, prevValue:Any) : Any = println (msg(in, prevValue))
  override def wname = "log"
}


