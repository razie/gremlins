/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables;

import razie.base.{ AttrAccess, ActionItem };

/**
 * simple actionable factory
 * 
 * TODO 1-1 detailed docs
 * 
 * @author razvanc
 */
object Actionables {

   /** make an executable - besides the executable, the unifiedstring must contain the arguments... */
   def make(name:String, unifiedstring:String) : Actionable = {
      val Executables.pat(domain, cmd, parens, args) = unifiedstring

      val ex = Executables.make (unifiedstring)
      val in = razie.AA(if (args.endsWith(")")) args.substring(0, args.length-1) else args)
     
      make(name, new ActionItem(name), in, ex);
   }

   /** make an executable */
   def make(name:String, ai:ActionItem, in:AttrAccess, ex:razie.wf.JWFunc) : Actionable = {
      new Actionable (name, ai, in, ex);
   }
 
  /** Foo = (Foo) makeProxy (foo, overloadingActionables) */
  def makeProxy(o: Object, hasa: HasActionables): AnyRef = DebugProxy.newInstance(o, hasa);

}

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
