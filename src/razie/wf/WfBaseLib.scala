/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.base.{ActionContext => AC}
import razie.actionables._

/** basic library of executables */
trait WfLib[T] extends WfLibrary[T] { 
  me =>

  // nop if you need an empty activity - maybe required by the syntax
  def nop  = me wrap new WfExec { override def execu() = {}; override def wname = "nop" }
  def todo = me wrap new WfExec { override def execu() = {}; override def wname = "TODO" }
  
  def inc (i:Int=1) = me wrap new WfeInc (i)
  def inc : T = inc(1)
  def dec (i:Int=1) = me wrap new WfeInc (-1 * i)
  def dec : T = dec(1)
  def set (i:Any) = me wrap new WfeSet (i)
  
  def $0 = (in:AC, vv:Any) => vv
  def $ (who:Int) = (in:AC, vv:Any) => who match {
     case 0 => vv
     case _ => in a who.toString
  }
  def $ (name:String) = (in:AC, vv:Any) => name match {
     case "0" => vv
     case _ => in a name
  }
  
  def s      (sc:String) = script ("?", "scala", sc)
  
  // logging
  def log (m: => Any) = me wrap new WfeLog ((x,y)=>m)
  def log (m: (AC, Any)=> Any) = me wrap new WfeLog (m)
  
  // assign
  def assign (name:String) (value: =>Any) = me wrap new WfeAssign (name)((x)=>value)

  // TODO scripted activities can migrate easily anywhere, eh?
  def script (name:String, lang:String, s:String) = todo
  
  // TODO
  def act (ustr:String) = me wrap new WfeAction (ustr)
  // TODO don't rebuild only to parse again
  def act (t:(String, String, String)) : T = act(t._1+":"+t._2+"?"+t._3)
  
  // TODO
  def action (gref:String, action:String) = todo
  
}

class WfeAction (ustr:String) extends WfExec {
  val ac = ExecutableFactory.instance.make(ustr)
  
  override def exec (in:AC, prevValue:Any) = { 
    ac.exec (in, prevValue)
  }
  
  override def wname = "act - " + ustr
}

class WfeAssign (name:String) (v: Any => Any) extends WfExec {
  override def exec (in:AC, prevValue:Any) = { 
    in.set (name, v(prevValue))
    prevValue
  }
  
  override def wname = "assign"
}

class WfeLog (msg : (AC, Any) => Any) extends WfExec { 
  override def exec  (in:AC, prevValue:Any) : Any = {
    val v = msg(in, prevValue)
    println (v.toString)
    v
  }
  override def wname = "log"
}

class WfeInc (i:Int) extends WfExec { 
  override def exec  (in:AC, prevValue:Any) : Any = prevValue.asInstanceOf[Int] + i
  override def wname = "inc" 
}
  
class WfeSet (newV:Any) extends WfExec { 
  override def exec  (in:AC, prevValue:Any) : Any = newV
  override def wname = "set" 
}
  
