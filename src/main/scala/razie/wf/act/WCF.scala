/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.act

import scala.util.parsing.combinator._
import razie.AA
import razie.base.{ActionContext => AC}
import razie.base.scripting._
import razie.wf.lib._
import razie.wf._

// parsers

/** workflow spec parsers */
trait WCFBase extends WCFExpr {
  // by default it's a sequence
  
  // all libraries must implement this
  def activities () : Parser[WfActivity] = wcfbase
  def wcfbase : Parser[WfActivity] = wctrl | label
  def wtypes : Parser[WfActivity] = activities()
  
  def wset : Parser[Seq[WfActivity]] = "{"~rep(one)~"}" ^^ { case "{"~l~"}" => l }
  
  def wfa : Parser[WfActivity] = oneormore
  def one : Parser[WfActivity] = wtypes~opt(";") ^^ {case x~o => x}
  
  def seq : Parser[WfActivity] = "seq"~wset ^^ { case "seq"~l => wf.seq(l) }
  def par : Parser[WfActivity] = "par"~wset ^^ { case "par"~l => wf.par(l) }
  def scope : Parser[WfActivity] = "scope"~one ^^ { case "scope"~a => wf.scope(a) }
  def label : Parser[WfActivity] = "label"~ident~one ^^ { case "label"~i~a => wf.label(i, a) }
  
  def oneormore : Parser[WfActivity] = one | wset ^^ { l => wf.seq(l) }

//  def wmatch : Parser[Any] = "match"~"("~expr~")"~"{"~rep(wcase)~"}"
//  def wcase : Parser[Any] = "case"~const~"=>"~wfa

  def wfdefn : Parser[WfActivity] = rep(wfa) ^^ { case l => wf.seq(l) }
  
  def wctrl : Parser[WfActivity] = wif | seq | par | scope
 
  def wif : Parser[WfActivity] = "if"~"("~cond~")"~"then"~wfa~opt(welse) ^^ {
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
  def comp(x:Parser[WfActivity],y:Parser[WfActivity]) : Parser[WfActivity] = x | y
//  def comp(x:WCFBase#Parser[WfActivity],y:WCFBase#Parser[WfActivity]) : WCFBase#Parser[WfActivity] = x | y
} 

/** simple extension example */
object WCF extends WCFBase with WCFBaseLib with CspWcf {
  override def activities() : Parser[WfActivity] = wcfbase | wcfbaselib | cspwcflib 
}

///** simple extension example */
//object WCF extends WCFBase {
//  type Gen = () => WCFBase#Parser[WfActivity]
//  type P = WCFBase#Parser[WfActivity]
//  
//  val libs = new collection.mutable.ListBuffer[Gen]()
// 
//  this += (() => new WCFBaseLib().activities())
//  this += (() => new CspWcf().activities())
//  
//  def += (lib:Gen) = { libs append lib; this }
//  
//  override def activities() : WCFBase#Parser[WfActivity] = super.activities() | new WCFBaseLib().activities() | new CspWcf().activities()
////  override def activities() : WCFBase#Parser[WfActivity] = libs map (_()) foldLeft (super.activities()) (comp(_,_).asInstanceOf[WCFBase#Parser[WfActivity]])
////  override def activities() : WCFBase#Parser[WfActivity] = super.activities() | libs map (_()) reduceLeft (_ | _)
////  override def wtypes : Parser[WfActivity] = super.wtypes | WCFBaseLib.activities | csp_lib
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
       println (">>>>>>>> RESULT is " + x.run (""))
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



