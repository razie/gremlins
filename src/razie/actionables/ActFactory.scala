/*
 * Razvan's public code. Copyright 2008 based on Apache license (share alike) see LICENSE.txt for
 * details.
 */
package razie.actionables

import razie.base.AttrAccess
import razie.base.ActionItem
import org.w3c.dom.Element
import scala.util.parsing.combinator._
//import com.razie.pub.base.data._

///** version with combinator parsers */
//class ACTF extends JavaTokenParsers {
//   type TUP = (String, String, String, String)
//  def ac : Parser[TUP] = ident~":"~ident~"("~cond~")"~"then"~wfa~opt(welse) ^^ {
//     case "if"~"("~cond~")"~"then"~wfa~we => new WfIf ((x)=>cond.eval(null, x), wfa, we.toList:_*)
//  }
//
////  def ffp: Parser[XExpr] = floatingPointNumber ^^ {s => new XExpr { override def eval (in:AC) = s.toFloat } }
////  def fstr: Parser[XExpr] = stringLiteral ^^ {s => new XExpr { override def eval (in:AC) = s } }
//  
//  def parseitman (s:String) = parseAll(wfdefn, s)
//} 

/** factory for the basic executables offered 
 */
object ActFactory extends ActionableFactory {
   // domain:func args             // normal
   // domain:func?args             // url style
   val pat = """(\w+):(\w+)([ \?(])(.*)""".r
  
   ActionableFactory.init (this)
   
   override def make(name:String, unifiedstring:String) : Actionable = {
      val pat(domain, cmd, parens, args) = unifiedstring

      val ex = ScalaExecFactory.make (unifiedstring)
      val in = razie.AA(if (args.endsWith(")")) args.substring(0, args.length-1) else args)
     
      make(name, new ActionItem(name), in, ex);
   }

	override def make(name:String, ai:ActionItem, in:AttrAccess,
                     ex:IExecutable) : Actionable = {
      new Actionable (name, ai, in, ex);
   }
 
}
