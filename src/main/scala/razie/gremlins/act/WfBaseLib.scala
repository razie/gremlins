/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins.act

import razie.base.{ ActionContext => AC }
import razie.AA
import razie.actionables._
import razie.g._
import razie.gremlins._
import razie.wf

/** trait shared by all libraries of executables */
trait WfLib[T] extends WfLibrary[T] { me =>
  def todo = me wrap new Wfe0("todo")
  def todo(msg: String = "") = me wrap new Wfe0("todo" + msg)

  implicit val linkFactory = (x, y) => new WfLink(x, y)
}

/** basic library of executables */
trait WfBaseLib[T] extends WfLib[T] { me =>

  //------------------- expressions 
  /** the implicit local value */
  def it = new $Expr("0") // haskell interpreter uses 'it' for the last value
  def $0 = new $Expr("0") // classic for current value in a range of scripting langs
  def $x = new $Expr("x") 
  def $y = new $Expr("y")
  /** a value by name from context */
  def $(name: String) = new $Expr(name)
  /** a constant value */
  def $C(const: Any) = new CExpr(const)

  implicit def xe(sc: String) = new AExpr(sc)
  implicit def xei(sc: Int) = new CExpr(sc)

  // nop if you need an empty activity - maybe required by the syntax
  def nop = me wrap new Wfe0("nop")

  def sleep(i: AExpr) = me wrap new WfeSleep(i)
  def sleep(i: String) = me wrap new WfeSleep(WCFExpr parseAExpr i)
  def sleep(i: Int = 1) = me wrap new WfeSleep(WCFExpr parseAExpr i.toString)

  def inc(i: AExpr) = me wrap new WfeInc(i)
  def inc(i: String) = me wrap new WfeInc(WCFExpr parseAExpr i)
  def inc(i: Int = 1) = me wrap new WfeInc(WCFExpr parseAExpr i.toString)
  def inc: T = inc(1)
  def dec(i: AExpr) = me wrap new WfeDec(i)
  def dec(i: Int = 1) = me wrap new WfeDec(WCFExpr parseAExpr i.toString)
  def dec: T = dec(1)
  def set(i: Any) = me wrap new WfeSet(WCFExpr parseAExpr i.toString)

  /** log result of expression */
  def log(m: AExpr) = me wrap new WfeLog(m)

  /** assign result of expression to name in context */
  def assign(name: String, e: AExpr) = me wrap new WfeAssign(name, e)

  // TODO scripted activities can migrate easily anywhere, eh?
  implicit def s(sc: String) = script("?", "scala", sc)
  def sscala(sc: String) = script("?", "scala", sc)
  def js(sc: String) = script("?", "js", sc)
  def script(name: String, lang: String, s: String) = WfeScript(lang, s)

  // Razie's actionables
  def act(ustr: String) = me wrap new WfeAction(ustr)
  // TODO don't rebuild only to parse again
  def act(t: (String, String, String)): T = act(t._1 + ":" + t._2 + "(" + t._3 + ")")

  def stop (pass:Int = 1) = new razie.gremlins.eng.WfStop (pass)
  
  // TODO
  //  def action (gref:String, action:String) = new WfAssetCmd (action, GRef.fromString(gref), AC(), Map())
}

/** no arguments or expressions of any kind */
class Wfe0(override val wname: String) extends WfExec with HasDsl {
  override def apply(in: AC, v: Any) = v
  override def toDsl = wname
}

/** do(expr) - uses an expression - either constant or a scripted expression */
abstract class Wfe1(override val wname: String, val expr: AExpr) extends WfExec with HasDsl {
  override def toDsl = wname + " (" + expr.toDsl + ")"
}

/** do(a=b,c=d...) - has arguments */
abstract class Wfe2(override val wname: String, val aa: razie.AA) extends WfExec with HasDsl {
  override def toDsl = wname + " (" + aa.toString + ")"
}

/** external DSL parsing for the simple library */
trait WCFBaseLib extends WCFBase {
  //  override def activities () : Parser[WfActivity] = wcfbaselib
  def wcfbaselib: Parser[WfActivity] = (
      wlog | wnop | winc | wdec | wass | razact |  
      resReq | resReply | resReplyIgnore | assetcmd 
      )

  // TODO
//  def gref: Parser[WfActivity] = "gref" ~ "(" ~ expr ~ ")" ^^ { case "gref" ~ "(" ~ e ~ ")" => wf.log(e) }
//  def wpath: Parser[WfActivity] = "wpath" ~ "(" ~ expr ~ ")" ^^ { case "wpath" ~ "(" ~ e ~ ")" => wf.log(e) }
  
  def wlog: Parser[WfActivity] = "log" ~ "(" ~ expr ~ ")" ^^ { case "log" ~ "(" ~ e ~ ")" => wf.log(e) }
  def wnop: Parser[WfActivity] = "nop" ^^ (x => wf.nop)
  def winc: Parser[WfActivity] = "inc" ~ opt("(" ~ expr ~ ")") ^^ {
    case "inc" ~ Some("(" ~ e ~ ")") => wf.inc(e)
    case "inc" ~ None => wf.inc()
  }
  def wdec: Parser[WfActivity] = "dec" ~ opt("(" ~ expr ~ ")") ^^ {
    case "dec" ~ Some("(" ~ e ~ ")") => wf.dec(e)
    case "dec" ~ None => wf.inc(-1)
  }

  def wass: Parser[WfActivity] = "assign" ~ $expr ~ "=" ~ expr ^^ { case "assign" ~ i ~ "=" ~ e => wf.assign(i.name, e) }
  
//  def cancel: Parser[WfActivity] = "skip" ~ gref ^^ { case "skip" ~ n => wf.skip(n) }

  def razact: Parser[WfActivity] = "act:" ~ ac ^^ { case "act:" ~ a => wf.act(a) }
  def ac: Parser[TUP] = ident ~ ":" ~ ident ~ opt(acoa) ^^ { case d ~ ":" ~ f ~ sa => (d, f, sa.getOrElse("")) }

  def resReply: Parser[WfActivity] = "ResReply" ^^ { case _ => new razie.gremlins.WfResReply() }
  def resReplyIgnore: Parser[WfActivity] = "ResReplyIgnore" ^^ { case _ => new razie.gremlins.WfResReplyIgnore() }

  def resReq: Parser[WfActivity] = "ResReq" ~ "(" ~ nocomma ~ "," ~ nocomma ~ "," ~ nocomma ~ "," ~ expr ~ ")" ^^ {
    case "ResReq" ~ "(" ~ g ~ "," ~ t ~ "," ~ w ~ "," ~ e ~ ")" => new WfResReq(razie.g.GRef.parse(g), w, AA(), e, t)
  }
  def nocomma: Parser[String] = """[^,]+""".r // ^^ { e => e }

  def assetcmd: Parser[WfActivity] = "snak" ~ "(" ~ nocomma ~ "," ~ nocomma ~ "," ~ exprmap ~ ")" ^^ {
    case "snak" ~ "(" ~ a ~ "," ~ g ~ "," ~ em ~ ")" => new WfAssetCmd(a, razie.g.GRef.parse(g), AA(), em)
  }
  def exprmap: Parser[Map[String, AExpr]] = "(" ~> repsep(exprmember, ",") <~ ")" ^^ (Map() ++ _)
  def exprmember: Parser[(String, AExpr)] = ident ~ "=" ~ expr ^^ { case i ~ "=" ~ e => (i, e) }
}

class WfeAction(ustr: String) extends Wfe0("act:" + ustr) {
  val ac = Actionables.make("?", ustr)
  override def apply(in: AC, prevValue: Any) = ac.execute()
}

case class WfeScript(lang: String, scr: String) extends WfExec {
  val sc = razie.base.scripting.ScriptFactory.make(lang, scr)

  override def apply(in: AC, prevValue: Any) = {
    val ctx = razie.base.scripting.ScriptFactory.mkContext("scala", in)
    ctx.set("$0", prevValue.asInstanceOf[AnyRef])
    sc.eval(ctx)
  }

  override def wname = "act - " + scr
}

/** assigne variable */
class WfeAssign(name: String, e: AExpr) extends WfExec with HasDsl {
  override def apply(in: AC, prevValue: Any) = name match {
    case "0" => e(in, prevValue)
    case _ => {
      in.set(name, e(in, prevValue).asInstanceOf[AnyRef])
      prevValue
    }
  }

  override def wname = "assign"
  override def toDsl = "assign $" + name + "=" + e.toDsl
}

/** log the expression */
class WfeLog(e: AExpr) extends Wfe1("log", e) {
  override def apply(in: AC, prevValue: Any): Any = {
    val v = e.apply(in, prevValue)
    println(if (v == null) "NULL" else v.toString)
    v
  }
}

/** increment an integer */
class WfeInc(e: AExpr) extends Wfe1("inc", e) {
  override def apply(in: AC, prevValue: Any): Any =
    (if (prevValue.isInstanceOf[Int]) prevValue.asInstanceOf[Int] else prevValue.toString.toInt) +
      e.apply(in, prevValue).toString.toInt
}

/** decrement an integer */
class WfeDec(e: AExpr) extends Wfe1("dec", e) {
  override def apply(in: AC, prevValue: Any): Any =
    prevValue.asInstanceOf[Int] - e.apply(in, prevValue).toString.toInt
}

/** assigne variable */
class WfeSet(newV: AExpr) extends Wfe1("set", newV) {
  override def apply(in: AC, prevValue: Any): Any =
    newV.apply(in, prevValue)
}

/** sleep a number of millisecons in the current thread */
class WfeSleep(e: AExpr) extends Wfe1("sleep", e) {
  override def apply(in: AC, prevValue: Any): Any = Thread sleep e.apply(in, prevValue).toString.toInt
}

