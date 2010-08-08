/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.base.{ActionContext => AC}
import razie.AA
import razie.actionables._
import razie.g._

/** no arguments or expressions of any kind */
class Wfe0 (override val wname:String) extends WfExec with HasDsl {
  override def apply(in:AC, v:Any) = v
  override def toDsl = wname
}

/** do(expr) - uses an expression - either constant or a scripted expression */
abstract class Wfe1 (override val wname:String, val expr:XExpr) extends WfExec with HasDsl {
  override def toDsl = wname+" ("+expr.toDsl+")"
}

/** do(a=b,c=d...) - has arguments */
abstract class Wfe2 (override val wname:String, val aa:razie.AA) extends WfExec with HasDsl {
  override def toDsl = wname+" ("+aa.toString+")"
}

/** basic library of executables */
trait WfLib[T] extends WfLibrary[T] { 
  me =>
  def todo = me wrap new Wfe0 ("todo")
  
  implicit val linkFactory = (x,y) => WL(x,y)
}

/** basic library of executables */
trait WfBaseLib[T] extends WfLib[T] { 
  me =>

  //------------------- expressions 
  /** the implicit local value */
  def $0 = new $Expr ("0")                 
  def $x = new $Expr ("x")                 
  def $y = new $Expr ("y")                 
  /** a value by name from context */
  def $ (name:String) = new $Expr (name)
  /** a constant value */
  def $C(const:Any) = new CExpr (const)
  
  implicit def xe  (sc:String) = new XExpr (sc)
  implicit def xei (sc:Int) = new CExpr (sc)
  
  // nop if you need an empty activity - maybe required by the syntax
  def nop  = me wrap new Wfe0 ("nop")
  
  def sleep (i:XExpr) = me wrap new WfeSleep (i)
  def sleep (i:String) = me wrap new WfeSleep (WCFExpr parseXExpr i)
  def sleep (i:Int=1) = me wrap new WfeSleep (WCFExpr parseXExpr i.toString)
  
  def inc (i:XExpr) = me wrap new WfeInc (i)
  def inc (i:String) = me wrap new WfeInc (WCFExpr parseXExpr i)
  def inc (i:Int=1) = me wrap new WfeInc (WCFExpr parseXExpr i.toString)
  def inc : T = inc(1)
  def dec (i:XExpr) = me wrap new WfeDec (i)
  def dec (i:Int=1) = me wrap new WfeDec (WCFExpr parseXExpr i.toString)
  def dec : T = dec(1)
  def set (i:Any) = me wrap new WfeSet (WCFExpr parseXExpr i.toString)
  
  // logging
  def log (m: XExpr) = me wrap new WfeLog (m)
  
  // assign
//  def assign (name:String) (value: =>Any) = me wrap new WfeAssign (name)((x)=>value)
  def assign (name:String, e:XExpr) = me wrap new WfeAssign (name, e)

  // TODO scripted activities can migrate easily anywhere, eh?
  implicit def s      (sc:String) = script ("?", "scala", sc)
  def sscala  (sc:String) = script ("?", "scala", sc)
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
trait WCFBaseLib extends WCFBase {
//  override def activities () : Parser[WfAct] = wcfbaselib
  def wcfbaselib : Parser[WfAct] = (
        wlog | wnop | winc | wdec | wass | razact | resReq | resReply | resReplyIgnore | assetcmd
        )
  
  def wlog: Parser[WfAct] = "log"~"("~expr~")" ^^ {case "log"~"("~e~")" => wf.log (e)}
  def wnop: Parser[WfAct] = "nop" ^^ (x => wf.nop)
  def winc: Parser[WfAct] = "inc"~opt("("~expr~")") ^^ {
     case "inc"~Some("("~e~")") => wf.inc(e)
     case "inc"~None => wf.inc()
  }
  def wdec: Parser[WfAct] = "dec"~opt("("~expr~")") ^^ {
     case "dec"~Some("("~e~")") => wf.dec(e)
     case "dec"~None => wf.inc(-1)
  }
  
  def wass: Parser[WfAct] = "assign"~$expr~"="~expr ^^ {case "assign"~i~"="~e => wf.assign (i.name, e)}
  
  def razact: Parser[WfAct] = "act:"~ac ^^ {case "act:"~a => wf.act (a)}
  def ac : Parser[TUP] = ident~":"~ident~opt(acoa) ^^ { case d~":"~f~sa => (d,f,sa.getOrElse("")) }
  
  def resReply : Parser[WfAct] = "ResReply" ^^ { case _ => new razie.wf.WfResReply() }
  def resReplyIgnore : Parser[WfAct] = "ResReplyIgnore" ^^ { case _ => new razie.wf.WfResReplyIgnore() }
  
  def resReq : Parser[WfAct] = "ResReq"~"("~nocomma~","~nocomma~","~expr~")" ^^ { 
     case "ResReq"~"("~g~","~w~","~e~")" => new WfResReq(razie.g.GRef.parse(g), w, AA(), e) 
  }
  def nocomma : Parser[String] = """[^,]+""".r // ^^ { e => e }
  
  def assetcmd : Parser[WfAct] = "snak"~"("~nocomma~","~nocomma~","~exprmap~")" ^^ { 
     case "snak"~"("~a~","~g~","~em~")" => new WfAssetCmd(a, razie.g.GRef.parse(g), AA(), em) 
  }
  def exprmap : Parser[Map[String,XExpr]] = "("~>repsep (exprmember, ",")<~")" ^^ (Map() ++ _)
  def exprmember : Parser[(String,XExpr)] = ident~"="~expr ^^ { case i~"="~e => (i,e) }
} 

class WfeAction (ustr:String) extends Wfe0 ("act:"+ustr) {
  val ac = ActFactory.make("?", ustr)
  override def apply (in:AC, prevValue:Any) = ac.execute() 
}

case class WfeScript (lang:String, scr:String) extends WfExec {
  val sc = razie.base.scripting.ScriptFactory.make (lang, scr)
  
  override def apply (in:AC, prevValue:Any) = { 
    sc.eval(new razie.base.scripting.ScalaScriptContext (in, "$0", prevValue))
  }
  
  override def wname = "act - " + scr
}

class WfeAssign (name:String, e:XExpr) extends WfExec with HasDsl {
  override def apply (in:AC, prevValue:Any) = name match {
     case "0" => e(in, prevValue)
     case _ => {
        in.set (name, e(in, prevValue).asInstanceOf[AnyRef])
        prevValue
     }
  }
  
  override def wname = "assign"
  override def toDsl = "assign $"+name+"="+e.toDsl
}

class WfeLog (e:XExpr) extends Wfe1 ("log", e) { 
  override def apply  (in:AC, prevValue:Any) : Any = {
    val v = e.apply(in, prevValue)
    println (if (v == null) "NULL" else v.toString)
    v
  }
}

class WfeInc (e:XExpr) extends Wfe1 ("inc", e) { 
  override def apply  (in:AC, prevValue:Any) : Any = 
    (if (prevValue.isInstanceOf[Int]) prevValue.asInstanceOf[Int] else prevValue.toString.toInt) + 
    e.apply(in, prevValue).toString.toInt
}
  
class WfeDec (e:XExpr) extends Wfe1 ("dec", e) { 
  override def apply  (in:AC, prevValue:Any) : Any = 
    prevValue.asInstanceOf[Int] - e.apply(in, prevValue).toString.toInt
}
  
class WfeSet (newV:XExpr) extends Wfe1 ("set", newV) { 
  override def apply  (in:AC, prevValue:Any) : Any = 
    newV.apply(in, prevValue)
}
  
class WfeSleep (e:XExpr) extends Wfe1 ("sleep", e) { 
  override def apply  (in:AC, prevValue:Any) : Any = Thread sleep e.apply(in, prevValue).toString.toInt
}
  
