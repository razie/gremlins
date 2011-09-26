/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins

import scala.util.parsing.combinator._
import razie.AA
import razie.base.{ActionContext => AC}
import razie.base.scripting._
import razie.gremlins.lib._

/** an expression */
abstract class Expr[T <: Any] (val expr : String) extends WFunc[T] with HasDsl {
  override def toString = toDsl
  override def toDsl = expr
}

/** arithmetic expressions */
class AExpr (val e : String) extends Expr[Any] (e) {
  override def apply (in:AC, v:Any) : Any = e
  
  def + [T <:Any] (t:T) = t match {
     case b:AExpr => new AExprPLUS (this,b)
     case x@_=> new AExprPLUS (this,new CExpr(x))
  }
  
  def == (b:AExpr) = new BCMP2 (this,"==",b)
  def != (b:AExpr) = new BCMP2 (this,"!=",b)
  def <  (b:AExpr) = new BCMP2 (this,"<",b)
  def >  (b:AExpr) = new BCMP2 (this,">",b)
  def >= (b:AExpr) = new BCMP2 (this,">=",b)
  def <= (b:AExpr) = new BCMP2 (this,"<=",b)
}

/** PLUS arithmetic expressions 
 * 
 * TODO it's only string
 */
class AExprPLUS (a:AExpr, b:AExpr) extends AExpr ("+") {
  override def apply (in:AC, v:Any) : Any = a(in, v).toString + b(in,v).toString
  override def toDsl = a.toDsl + " + " + b.toDsl
}

/** constant expression
 * 
 *  TODO i'm loosing the type definition
 */
case class CExpr[T <: Any] (ee : T) extends AExpr (ee.toString) {
  override def apply (in:AC, v:Any) : Any = ee
  override def toDsl = "\"" + expr + "\""
}

/** $varname expressions - like unix scripting variables */
case class $Expr (name : String) extends AExpr ("$"+name) {
  override def apply (in:AC, v:Any) : Any = name match {
     case "0" => v
     case _ => in a name
  }
}

/** boolean expressions */
abstract class BExpr (e:String) extends Expr[Boolean] (e) {
  def || (b:BExpr) = new BCMP1 (this,"||",b)
  def && (b:BExpr) = new BCMP1 (this,"&&",b)
  def not = new BCMPNot (this)
}

/** negated boolean expression */
case class BCMPNot(a: BExpr) extends BExpr("") {
  override def apply(in: AC, v: Any) = ! a.apply(in, v)
}

/** composed boolean expression */
case class BCMP1 (a:BExpr, s:String, b:BExpr) extends BExpr (a.toDsl+" "+s+" "+b.toDsl) {
     override def apply (in:AC, v:Any) = s match {
        case "||" => a.apply(in, v) || b.apply(in, v)
        case "&&" => a.apply(in, v) && b.apply(in, v)
        case _ => sys.error ("Operator " + s + " UNKNOWN!!!")
        } 
     }

/** simple boolean expression */
case class BCMP2  (a:AExpr, s:String, b:Expr[Any]) extends BExpr (a.toDsl+" "+s+" "+b.toDsl) {
     override def apply (in:AC, v:Any) = s match {
        case "==" => a(in, v) == b(in, v)
        case "!=" => a(in, v) != b(in, v)
        case "<=" => a(in, v).toString <= b(in, v).toString
        case ">=" => a(in, v).toString >= b(in, v).toString
        case "<" => a(in, v).toString < b(in, v).toString
        case ">" => a(in, v).toString > b(in, v).toString
        case _ => sys.error ("Operator " + s + " UNKNOWN!!!")
        } 
     }

object WCFExpr extends WCFExpr {
  def parseAExpr (s:String) = parseAll(expr, s) get
  def parseBExpr (s:String) = parseAll(cond, s) get
}


/** expression parsers */
trait WCFExpr extends JavaTokenParsers {
   
   //------------- arguments
   
  type TUP = (String, String, String)

  // TODO need to accept ) as well
  val acargs: Parser[String] = """[^)]*""".r
  def acoa : Parser[String] = "("~acargs~")" ^^ { case "("~a~")" => a }

  //--------------- expressions
   
  def expr : Parser[AExpr] = ppexpr | pterm1
  def ppexpr : Parser[AExpr] = pterm1~"+"~pterm1 ^^ { case a~s~b => a + b }
  def pterm1 : Parser[AExpr] = numexpr | dolar0 | dolarexpr | cexpr //| moreexpr
  
  def dolar0 : Parser[$Expr] = """$0""" ^^ { case x => new $Expr("0") }
  def dolarexpr : Parser[$Expr] = """$"""~ident ^^ { case """$"""~i => new $Expr(i) }
  def $expr : Parser[$Expr] = dolar0 | dolarexpr 
  def numexpr : Parser[AExpr] = wholeNumber ^^ { case i => new AExpr(i) }
  def cexpr : Parser[AExpr] = "\""~"""[^"]*""".r~"\"" ^^ { case "\""~e~"\"" => new CExpr (e) }
//  def moreexpr : Parser[AExpr] = """[^+"()=<>|&]+""".r ^^ { e => new AExpr (e) }
  
//  def pexpr: Parser[AExpr] = term~rep("+"~term | "-"~term) ^^ { case t~l => t+l.first }// TODO fix this
//  def term: Parser[AExpr] = factor~rep("*"~factor | "/"~factor) ^^ { case f~l => t*l.first } //TODO fix this
//  def factor: Parser[AExpr] = ffp | fstr | "("~expr~")" ^^ { case "("~e~")" => e }
  
//        val kk = new ScriptContextImpl(in)
//        kk.set ("value", v.asInstanceOf[AnyRef])
//        new ScriptScala(e).eval(kk).getOrThrow
  
//  def expr: Parser[AExpr] = term~rep("+"~term | "-"~term)
//  def term: Parser[AExpr] = factor~rep("*"~factor | "/"~factor) ^^ { case f~l => 
//  def expr: Parser[AExpr] = term~rep("+"~term | "-"~term) ^^ { case t~l => t+l.first }// TODO fix this
//  def term: Parser[AExpr] = factor~rep("*"~factor | "/"~factor) ^^ { case f~l => t*l.first } //TODO fix this
//  def factor: Parser[AExpr] = ffp | fstr | "("~expr~")" ^^ { case "("~e~")" => e }
//  
//  def fident: Parser[AExpr] = ident ^^ {s => new AExpr { override def eval (in:AC) = in a s } }
//  def ffp: Parser[AExpr] = floatingPointNumber ^^ {s => new AExpr { override def eval (in:AC) = s.toFloat } }
//  def fstr: Parser[AExpr] = stringLiteral ^^ {s => new AExpr { override def eval (in:AC) = s } }
  
 
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
  def cmp  (a:AExpr, s:String, b:AExpr) = new BCMP2 (a,s,b)

  def parseExpr (s:String) = parseAll(expr, s)
}

