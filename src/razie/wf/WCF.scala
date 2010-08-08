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

/** an expression */
abstract class Expr[T <: Any] (val expr : String) extends WFunc[T] with HasDsl {
  override def toString = toDsl
  override def toDsl = expr
}
  
case class XExpr (e : String) extends Expr[Any] (e) {
  override def apply (in:AC, v:Any) : Any = e
  
  def + [T <:Any] (t:T) = t match {
     case b:XExpr => new XExprPLUS (this,b)
     case x@_=> new XExprPLUS (this,new CExpr(x))
  }
  
  def == (b:XExpr) = new BCMP2 (this,"==",b)
  def != (b:XExpr) = new BCMP2 (this,"!=",b)
  def <  (b:XExpr) = new BCMP2 (this,"<",b)
  def >  (b:XExpr) = new BCMP2 (this,">",b)
  def >= (b:XExpr) = new BCMP2 (this,">=",b)
  def <= (b:XExpr) = new BCMP2 (this,"<=",b)
}

class XExprPLUS (a:XExpr, b:XExpr) extends XExpr ("+") {
  override def apply (in:AC, v:Any) : Any = a(in, v).toString + b(in,v).toString
  override def toDsl = a.toDsl + " + " + b.toDsl
}

// TODO i'm looksing the type definition
case class CExpr[T <: Any] (ee : T) extends XExpr (ee.toString) {
  override def apply (in:AC, v:Any) : Any = ee
  override def toDsl = "\"" + expr + "\""
}

case class $Expr (name : String) extends XExpr ("$"+name) {
  override def apply (in:AC, v:Any) : Any = name match {
     case "0" => v
     case _ => in a name
  }
}
  
abstract class BExpr (e:String) extends Expr[Boolean] (e) {
  def || (b:BExpr) = new BCMP1 (this,"||",b)
  def && (b:BExpr) = new BCMP1 (this,"&&",b)
}  

case class BCMP1 (a:BExpr, s:String, b:BExpr) extends BExpr (a.toDsl+" "+s+" "+b.toDsl) {
     override def apply (in:AC, v:Any) = s match {
        case "||" => a.apply(in, v) || b.apply(in, v)
        case "&&" => a.apply(in, v) && b.apply(in, v)
        case _ => error ("Operator " + s + " UNKNOWN!!!")
        } 
     }
  
case class BCMP2  (a:XExpr, s:String, b:XExpr) extends BExpr (a.toDsl+" "+s+" "+b.toDsl) {
     override def apply (in:AC, v:Any) = s match {
        case "==" => a(in, v) == b(in, v)
        case "!=" => a(in, v) != b(in, v)
        case "<=" => a(in, v).toString <= b(in, v).toString
        case ">=" => a(in, v).toString >= b(in, v).toString
        case "<" => a(in, v).toString < b(in, v).toString
        case ">" => a(in, v).toString > b(in, v).toString
        case _ => error ("Operator " + s + " UNKNOWN!!!")
        } 
     }

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
   
  def expr : Parser[XExpr] = ppexpr | pterm1
  def ppexpr : Parser[XExpr] = pterm1~"+"~pterm1 ^^ { case a~s~b => a + b }
  def pterm1 : Parser[XExpr] = numexpr | dolar0 | dolarexpr | cexpr //| moreexpr
  
  def dolar0 : Parser[$Expr] = """$0""" ^^ { case x => new $Expr("0") }
  def dolarexpr : Parser[$Expr] = """$"""~ident ^^ { case """$"""~i => new $Expr(i) }
  def $expr : Parser[$Expr] = dolar0 | dolarexpr 
  def numexpr : Parser[XExpr] = wholeNumber ^^ { case i => new XExpr(i) }
  def cexpr : Parser[XExpr] = "\""~"""[^"]*""".r~"\"" ^^ { case "\""~e~"\"" => new CExpr (e) }
//  def moreexpr : Parser[XExpr] = """[^+"()=<>|&]+""".r ^^ { e => new XExpr (e) }
  
//  def pexpr: Parser[XExpr] = term~rep("+"~term | "-"~term) ^^ { case t~l => t+l.first }// TODO fix this
//  def term: Parser[XExpr] = factor~rep("*"~factor | "/"~factor) ^^ { case f~l => t*l.first } //TODO fix this
//  def factor: Parser[XExpr] = ffp | fstr | "("~expr~")" ^^ { case "("~e~")" => e }
  
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
  
  def bcmp (a:BExpr, s:String, b:BExpr) = new BCMP1 (a,s,b)
  def cmp  (a:XExpr, s:String, b:XExpr) = new BCMP2 (a,s,b)

  def parseExpr (s:String) = parseAll(expr, s)
}

/** version with combinator parsers */
trait WCFBase extends WCFExpr {
  // by default it's a sequence
  
  // all libraries must implement this
  def activities () : Parser[WfAct] = wcfbase
  def wcfbase : Parser[WfAct] = wctrl | label
  def wtypes : Parser[WfAct] = activities()
  
  def wset : Parser[Seq[WfAct]] = "{"~rep(one)~"}" ^^ { case "{"~l~"}" => l }
  
  def wfa : Parser[WfAct] = oneormore
  def one : Parser[WfAct] = wtypes~opt(";") ^^ {case x~o => x}
  
  def seq : Parser[WfAct] = "seq"~wset ^^ { case "seq"~l => wf.seq(l:_*) }
  def par : Parser[WfAct] = "par"~wset ^^ { case "par"~l => wf.par(l:_*) }
  def scope : Parser[WfAct] = "scope"~one ^^ { case "scope"~a => wf.scope(a) }
  def label : Parser[WfAct] = "label"~ident~one ^^ { case "label"~i~a => wf.label(i, a) }
  
  def oneormore : Parser[WfAct] = one | wset ^^ { l => wf.seq(l:_*) }

//  def wmatch : Parser[Any] = "match"~"("~expr~")"~"{"~rep(wcase)~"}"
//  def wcase : Parser[Any] = "case"~const~"=>"~wfa

  def wfdefn : Parser[WfAct] = rep(wfa) ^^ { case l => wf.seq(l:_*) }
  
  def wctrl : Parser[WfAct] = wif | seq | par | scope
 
  def wif : Parser[WfAct] = "if"~"("~cond~")"~"then"~wfa~opt(welse) ^^ {
     case "if"~"("~cond~")"~"then"~wfa~we => new WfIf (cond, wfa, we)
  }
  def welse : Parser[WfElse] = "else"~wfa ^^ { case "else"~wfa => new WfElse (wfa) }
  
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
  
  def parseitman (s:String) = parseAll(wfdefn, s)
  def comp(x:Parser[WfAct],y:Parser[WfAct]) : Parser[WfAct] = x | y
//  def comp(x:WCFBase#Parser[WfAct],y:WCFBase#Parser[WfAct]) : WCFBase#Parser[WfAct] = x | y
} 

/** simple extension example */
object WCF extends WCFBase with WCFBaseLib with CspWcf {
  override def activities() : Parser[WfAct] = wcfbase | wcfbaselib | cspwcflib 
}

///** simple extension example */
//object WCF extends WCFBase {
//  type Gen = () => WCFBase#Parser[WfAct]
//  type P = WCFBase#Parser[WfAct]
//  
//  val libs = new collection.mutable.ListBuffer[Gen]()
// 
//  this += (() => new WCFBaseLib().activities())
//  this += (() => new CspWcf().activities())
//  
//  def += (lib:Gen) = { libs append lib; this }
//  
//  override def activities() : WCFBase#Parser[WfAct] = super.activities() | new WCFBaseLib().activities() | new CspWcf().activities()
////  override def activities() : WCFBase#Parser[WfAct] = libs map (_()) foldLeft (super.activities()) (comp(_,_).asInstanceOf[WCFBase#Parser[WfAct]])
////  override def activities() : WCFBase#Parser[WfAct] = super.activities() | libs map (_()) reduceLeft (_ | _)
////  override def wtypes : Parser[WfAct] = super.wtypes | WCFBaseLib.activities | csp_lib
//}

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
//  p (s3) 
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
  
  val e1 = """1"""
  val e2 = """"a""""
  val e3 = """$0"""
  val e4 = """$name"""
  val e5 = """1+1"""
  val e6 = """$0 + $0"""
  val e7 = """$0 + "-P""""
    
  e(e1)
  e(e2)
  e(e3)
  e(e4)
  e(e5)
  e(e6)
  e(e7)
  
  def e (s:String) {
    println ("expr to parse:")
    println (s)
    println ("==========================")
    val res = WCF.parseExpr(s)
    println("result parsed")
    println (res)
    res.map(x => {
       println(x.getClass.getName + " = " + x.toString)
    })
    println ("=========================================================")
  }
}



