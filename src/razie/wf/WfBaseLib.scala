/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.base.{ActionContext => AC}
import razie.actionables._

/** no arguments or expressions of any kind */
class Wfe0 (override val wname:String) extends WfExec with isser {
  override def exec(in:AC, v:Any) = v
  override def toDsl = wname
}

/** do(expr) - uses an expression - either constant or a scripted expression */
abstract class Wfe1 (override val wname:String, val expr:XExpr) extends WfExec with isser {
  override def toDsl = wname+" ("+expr.toString+")"
}

/** do(a=b,c=d...) - has arguments */
abstract class Wfe2 (override val wname:String, val aa:razie.AA) extends WfExec with isser {
  override def toDsl = wname+" ("+aa.toString+")"
}

/** basic library of executables */
trait WfLib[T] extends WfLibrary[T] { 
  me =>
  def todo = me wrap new Wfe0 ("todo")
}

/** basic library of executables */
trait WfBaseLib[T] extends WfLib[T] { 
  me =>

  //------------------- expressions 
  def $0 = new $Expr ("0")
  def $ (name:String) = new $Expr (name)
  
  implicit def xe  (sc:String) = new XExpr (sc)
  implicit def xei (sc:Int) = new XExpr (sc.toString)

  
  // nop if you need an empty activity - maybe required by the syntax
  def nop  = me wrap new Wfe0 ("nop")
  
  def inc (i:String) = me wrap new WfeInc (WCFExpr parseXExpr i)
  def inc (i:Int=1) = me wrap new WfeInc (WCFExpr parseXExpr i.toString)
  def inc : T = inc(1)
  def dec (i:Int=1) = inc (-1 * i)
  def dec : T = dec(1)
  def set (i:Any) = me wrap new WfeSet (WCFExpr parseXExpr i.toString)
  
  // logging
  def log (m: XExpr) = me wrap new WfeLog (m)
  
  // assign
  def assign (name:String) (value: =>Any) = me wrap new WfeAssign (name)((x)=>value)

  // TODO scripted activities can migrate easily anywhere, eh?
  implicit def s      (sc:String) = script ("?", "scala", sc)
  def scala  (sc:String) = script ("?", "scala", sc)
  def js     (sc:String) = script ("?", "js", sc)
  def script (name:String, lang:String, s:String) = WfeScript (lang, s)
  
  // Razie's actionables
  def act (ustr:String) = me wrap new WfeAction (ustr)
  // TODO don't rebuild only to parse again
  def act (t:(String, String, String)) : T = act(t._1+":"+t._2+"("+t._3+")")
  
  // TODO
  def action (gref:String, action:String) = todo
  
}

/** simple library */
trait WCFBaseLib extends WCFExpr {
  def wcfbaselib_lib: Parser[WfAct] = wlog | wnop | winc | wdec | razact
  
  def wlog: Parser[WfAct] = "log"~"("~expr~")" ^^ {case "log"~"("~e~")" => wf.log (e)}
  def wnop: Parser[WfAct] = "nop" ^^ (x => wf.nop)
  def winc: Parser[WfAct] = "inc"~opt("("~expr~")") ^^ {
     case "inc"~Some(e) => wf.inc()
     case "inc"~None => wf.inc()
  }
  def wdec: Parser[WfAct] = "dec"~opt("("~expr~")") ^^ {
     case "dec"~Some(e) => wf.inc()
     case "dec"~None => wf.inc(-1)
  }
  
  def razact: Parser[WfAct] = "act:"~ac ^^ {case "act:"~a => wf.act (a)}
  def ac : Parser[TUP] = ident~":"~ident~opt(acoa) ^^ { case d~":"~f~sa => (d,f,sa.getOrElse("")) }
} 

class WfeAction (ustr:String) extends Wfe0 ("act:"+ustr) {
  val ac = ActFactory.make("?", ustr)
  override def exec (in:AC, prevValue:Any) = ac.execute() 
}

case class WfeScript (lang:String, scr:String) extends WfExec {
  val sc = razie.base.scripting.ScriptFactory.make (lang, scr)
  
  override def exec (in:AC, prevValue:Any) = { 
    sc.eval(new razie.base.scripting.ScalaScriptContext (in, "$0", prevValue))
  }
  
  override def wname = "act - " + scr
}

class WfeAssign (name:String) (v: Any => Any) extends WfExec {
  override def exec (in:AC, prevValue:Any) = { 
    in.set (name, v(prevValue))
    prevValue
  }
  
  override def wname = "assign"
}

class WfeLog (e:XExpr) extends Wfe1 ("log", e) { 
  override def exec  (in:AC, prevValue:Any) : Any = {
    val v = e.eval(in, prevValue)
    println (v.toString)
    v
  }
}

class WfeInc (e:XExpr) extends Wfe1 ("inc", e) { 
  override def exec  (in:AC, prevValue:Any) : Any = 
    prevValue.asInstanceOf[Int] + e.eval(in, prevValue).toString.toInt
}
  
class WfeSet (newV:XExpr) extends Wfe1 ("set", newV) { 
  override def exec  (in:AC, prevValue:Any) : Any = newV.eval(in, prevValue)
}
  
