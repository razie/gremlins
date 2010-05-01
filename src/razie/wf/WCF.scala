/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import scala.util.parsing.combinator._
import razie.AA
import razie.base.{ActionContext => AC}
import razie.base.scripting._
import razie.wf.lib._

abstract class Expr[T] (val expr : String) extends WFunc[T] {
  override def exec (in:AC, prevValue:Any) : T = eval (in, prevValue)
  // basically rename exec to eval
  def eval (in:AC, prevValue:Any) : T 
  override def toString = expr
}
  
class XExpr (e : String) extends Expr[Any] (e) {
  override def eval (in:AC, prevValue:Any) : Any = e
}

class $Expr (name : String) extends XExpr ("$"+name) {
  override def eval (in:AC, prevValue:Any) : Any = name match {
     case "0" => prevValue
     case _ => in a name
  }
}
  
abstract class BExpr (e:String) extends Expr[Boolean] (e)

object WCFExpr extends WCFExpr {
  def parseXExpr (s:String) = parseAll(expr, s) get
  def parseBExpr (s:String) = parseAll(cond, s) get
}

trait WCFExpr extends JavaTokenParsers {
   
   //------------- arguments
   
  type TUP = (String, String, String)

  // TODO need to accept ) as well
  val acargs: Parser[String] = """[^)]*""".r
  def acoa : Parser[String] = "("~acargs~")" ^^ { case "("~a~")" => a }

  //--------------- expressions
   
//  def const : Parser[Any] = ffp | fstr

  // TODO proper expressions - this just makes everything a string
  def expr : Parser[XExpr] = dolar0 | dolarexpr | numexpr | moreexpr
  def dolar0 : Parser[XExpr] = """$0""" ^^ { x => new $Expr("0") }
  def dolarexpr : Parser[XExpr] = """$"""~ident ^^ { case """\$"""~i => new $Expr(i) }
  def numexpr : Parser[XExpr] = wholeNumber ^^ { i => new XExpr(i) }
  def moreexpr : Parser[XExpr] = """[^()=<>|&]+""".r ^^ { e => new XExpr (e) }
//        val kk = new ScriptContextImpl(in)
//        kk.set ("value", v.asInstanceOf[AnyRef])
//        new ScriptScala(e).eval(kk).getOrThrow
  
//  def expr: Parser[XExpr] = term~rep("+"~term | "-"~term)
//  def term: Parser[XExpr] = factor~rep("*"~factor | "/"~factor) ^^ { case f~l => 
//  def expr: Parser[XExpr] = term~rep("+"~term | "-"~term) ^^ { case t~l => t+l.first }// TODO fix this
//  def term: Parser[XExpr] = factor~rep("*"~factor | "/"~factor) ^^ { case f~l => t*l.first } //TODO fix this
//  def factor: Parser[XExpr] = ffp | fstr | "("~expr~")" ^^ { case "("~e~")" => e }
//  
//  def fident: Parser[XExpr] = ident ^^ {s => new XExpr { override def eval (in:AC) = in a s } }
//  def ffp: Parser[XExpr] = floatingPointNumber ^^ {s => new XExpr { override def eval (in:AC) = s.toFloat } }
//  def fstr: Parser[XExpr] = stringLiteral ^^ {s => new XExpr { override def eval (in:AC) = s } }
  
 
  
  //------------ conditions
  
  def cond : Parser[BExpr] = boolexpr
  
  def boolexpr: Parser[BExpr] = bterm1|bterm1~"||"~bterm1 ^^ { case a~s~b => bcmp(a,s,b) }
  def bterm1: Parser[BExpr] = bfactor1|bfactor1~"&&"~bfactor1 ^^ { case a~s~b => bcmp(a,s,b) }
  def bfactor1: Parser[BExpr] = eq | neq | lte | gte | lt | gt
  def eq : Parser[BExpr] = expr~"=="~expr ^^ { case a~s~b => cmp(a,s,b) }
  def neq: Parser[BExpr] = expr~"!="~expr ^^ { case a~s~b => cmp(a,s,b) }
  def lte: Parser[BExpr] = expr~"<="~expr ^^ { case a~s~b => cmp(a,s,b) }
  def gte: Parser[BExpr] = expr~">="~expr ^^ { case a~s~b => cmp(a,s,b) }
  def lt : Parser[BExpr] = expr~"<"~expr ^^ { case a~s~b => cmp(a,s,b) }
  def gt : Parser[BExpr] = expr~">"~expr ^^ { case a~s~b => cmp(a,s,b) }
  
  def bcmp (a:BExpr, s:String, b:BExpr) = new BExpr (a+" "+s+" "+b) {
     override def eval (in:AC, v:Any) = s match {
        case "||" => a.eval(in, v) || b.eval(in, v)
        case "&&" => a.eval(in, v) && b.eval(in, v)
        case _ => error ("Operator " + s + " UNKNOWN!!!")
        } 
     }
  
  def cmp (a:XExpr, s:String, b:XExpr) = new BExpr (a+" "+s+" "+b) {
     override def eval (in:AC, v:Any) = s match {
        case "==" => a.eval(in, v) == b.eval(in, v)
        case "!=" => a.eval(in, v) != b.eval(in, v)
//        case "<=" => a.eval(in, v) <= b.eval(in, v)
//        case ">=" => a.eval(in, v) >= b.eval(in, v)
//        case "<" => a.eval(in, v) < b.eval(in, v)
//        case ">" => a.eval(in, v) > b.eval(in, v)
        case _ => error ("Operator " + s + " UNKNOWN!!!")
        } 
     }

}

/** version with combinator parsers */
trait WCFBase extends WCFExpr {
  // by default it's a sequence
  
  def wtypes : Parser[WfAct] = wctrl | label
  
  def wset : Parser[Seq[WfAct]] = "{"~rep(one)~"}" ^^ { case "{"~l~"}" => l }
  
  def wfa : Parser[WfAct] = oneormore
  def one : Parser[WfAct] = wtypes~opt(";") ^^ {case x~o => x}
  
  def seq : Parser[WfAct] = "seq"~wset ^^ { case "seq"~l => wf.seq(l:_*) }
  def par : Parser[WfAct] = "par"~wset ^^ { case "par"~l => wf.par(l:_*) }
  def label : Parser[WfAct] = "label"~ident~one ^^ { case "label"~i~a => wf.label(i, a) }
  
  def oneormore : Parser[WfAct] = one | wset ^^ { l => wf.seq(l:_*) }

//  def wmatch : Parser[Any] = "match"~"("~expr~")"~"{"~rep(wcase)~"}"
//  def wcase : Parser[Any] = "case"~const~"=>"~wfa

  def wfdefn : Parser[WfAct] = rep(wfa) ^^ { case l => wf.seq(l:_*) }
  
  def wctrl : Parser[WfAct] = wif | seq | par
 
  def wif : Parser[WfAct] = "if"~"("~cond~")"~"then"~wfa~opt(welse) ^^ {
     case "if"~"("~cond~")"~"then"~wfa~we => new WfIf (cond, wfa, we)
  }
  def welse : Parser[WfElse] = "else"~wfa ^^ { case "else"~wfa => new WfElse (wfa) }
  
  def bcmp (a:BExpr, s:String, b:BExpr) = new BExpr {
     override def eval (in:AC, v:Any) = s match {
        case "||" => a.eval(in, v) || b.eval(in, v)
        case "&&" => a.eval(in, v) && b.eval(in, v)
        case _ => error ("Operator " + s + " UNKNOWN!!!")
        } 
     }
  
  def cmp (a:XExpr, s:String, b:XExpr) = new BExpr {
     override def eval (in:AC, v:Any) = s match {
        case "==" => a.eval(in, v) == b.eval(in, v)
        case "!=" => a.eval(in, v) != b.eval(in, v)
//        case "<=" => a.eval(in) <= b.eval(in)
//        case ">=" => a.eval(in) >= b.eval(in)
//        case "<" => a.eval(in) < b.eval(in)
//        case ">" => a.eval(in) > b.eval(in)
        case _ => error ("Operator " + s + " UNKNOWN!!!")
        } 
     }
  
//  def wmatch : Parser[Any] = "match"~"("~expr~")"~"{"~rep(wcase)~"}"
//  def wcase : Parser[Any] = "case"~const~"=>"~wfa

//  def expr: Parser[XExpr] = term~rep("+"~term | "-"~term)
//  def term: Parser[XExpr] = factor~rep("*"~factor | "/"~factor) ^^ { case f~l => 
//  def expr: Parser[XExpr] = term~rep("+"~term | "-"~term) ^^ { case t~l => t+l.first }// TODO fix this
//  def term: Parser[XExpr] = factor~rep("*"~factor | "/"~factor) ^^ { case f~l => t*l.first } //TODO fix this
//  def factor: Parser[XExpr] = ffp | fstr | "("~expr~")" ^^ { case "("~e~")" => e }
//  
//  def fident: Parser[XExpr] = ident ^^ {s => new XExpr { override def eval (in:AC) = in a s } }
//  def ffp: Parser[XExpr] = floatingPointNumber ^^ {s => new XExpr { override def eval (in:AC) = s.toFloat } }
//  def fstr: Parser[XExpr] = stringLiteral ^^ {s => new XExpr { override def eval (in:AC) = s } }
  
 
  trait XExpr extends WFunc[Any] {
    override def exec (in:AC, prevValue:Any) : Any = eval (in, prevValue)
    // basically rename exec to eval
             def eval (in:AC, prevValue:Any) : Any 
  }
  
  trait BExpr extends WFunc[Boolean] {
    override def exec (in:AC, prevValue:Any) : Boolean = eval (in, prevValue)
    // basically rename exec to eval
             def eval (in:AC, prevValue:Any) : Boolean 
  }
  
  def parseitman (s:String) = parseAll(wfdefn, s)
} 

/** simple extension example */
object WCF extends WCFBase with WCFBaseLib {
  override def wtypes : Parser[WfAct] = super.wtypes | wcfbaselib_lib 
}

object WFCMain extends Application {
  val s1 =
"""nop""" 

  val s2 =
"""
nop
""" 
     
  val s3 =
"""
nop
log (1)
""" 

  val if1 =
"""
if (1==1)
then nop
""" 

  val if2 =
"""
if (1==1) then log("1") else log("2")
""" 

  val if3 =
"""
if (1==1) 
then log("1") 
else log("2")
""" 

  val if4 =
"""
if (1==1) 
then act:simple:pipe(cmd="pwd") 
else act:simple:telnet(host="pwd",port="",cmd="") 
""" 

  val if5 =
"""
par {
  label a1 
  if (1==1) 
    then act:simple:pipe(cmd="pwd") 
    else act:simple:telnet(host="pwd",port="",cmd="") 
  whenComplete a1 
    log ("hurrah, it's done...")
""" 

//  p (s1) 
//  p (s2) 
  p (s3) 
//  p (if1) 
//  p (if2) 
//  p (if3) 
//  p (if4) 
//  p (if5) 
  
  def p (s:String) {
    println ("expr to parse:")
    println (s)
    println ("==========================")
    val res = WCF.parseitman(s)
    println("result parsed")
    println (res)
    res.map(x => {
       println("Workflow is: " + x.mkString)
       println ("==========================")
       println (">>>>>>>> RESULT is " + Engines().exec(x, razie.base.scripting.ScriptFactory.mkContext(), ""))
    })
    println ("=========================================================")
  }
}
