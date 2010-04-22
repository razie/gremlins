/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import scala.util.parsing.combinator._
import razie.AA
import razie.base.{ActionContext => AC}

/** version with combinator parsers */
class WCF extends JavaTokenParsers {
  // by default it's a sequence
  def wfdefn : Parser[WfAct] = rep(wfa) ^^ { case l => wf.seq(l:_*) }
  
  def wfa : Parser[WfAct] = wctrl | wlib

  def wctrl : Parser[WfAct] = wif 
 
  def wif : Parser[WfAct] = "if"~"("~cond~")"~"then"~wfa~opt(welse) ^^ {
     case "if"~"("~cond~")"~"then"~wfa~we => new WfIf ((x)=>cond.eval(null, x), wfa, we.toList:_*)
  }
  def welse : Parser[WfElse] = "else"~wfa ^^ { case "else"~wfa => new WfElse (wfa) }
  
//  def wif : Parser[WfAct] = ("if"~"("~cond~")"~"then"~wfa ^^ )~opt("else"~wfa)
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

  def wlib: Parser[WfAct] = wlog | wnop
  def wlog: Parser[WfAct] = "log"~"("~expr~")" ^^ {case "log"~"("~e~")" => wf.log ((in,v)=>e.eval(in, v).toString)}
  def wnop: Parser[WfAct] = "nop" ^^ (x => wf.nop)

//  def const : Parser[Any] = ffp | fstr

  def expr : Parser[XExpr] = """[^()=<>|&]+""".r ^^ { e => new XExpr () { override def eval(in:AC, v:Any) = e } }
  
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

  p (s1) 
  p (s2) 
  p (s3) 
  p (if1) 
  p (if2) 
  
  def p (s:String) {
    println ("expr to parse:")
    println (s)
    println ("==========================")
    val res = new WCF().parseitman(s)
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
