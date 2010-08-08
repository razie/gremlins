/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.lib

import razie.AA
import razie.base.{ActionContext => AC}
import razie.wf._
import razie.wf.res._
import razie.g._

/** communication channel / pipe: put/get values in sequence */
class CommChannel (val _name:String, val _size:Int) extends WQueue (_name, _size) {
  AllResources add this
}

/** 
 * CSP - channel based communication between processes - implies multi-way synchronization on channels
 */
object CSP extends CSP

trait CSP extends WfLib[WfAct] with WfBaseLib[WfAct] {
  me =>
   
  override def wrap (e:WfExec) : WfAct = razie.wf.WfWrapper (e)

  /** channel naming convention: it has to be unique in the same agent instance */
  object Channel {
     /** creates a channle with a unique ID recommended approach */
     def apply () = WfChannel(GRef.uid, true)
     /** creates a channle with a given name - NOTE that the name is shared across this AgentJVM */
     def apply (name:String, size:Int = 0) = WfChannel(name, true, size)
     def resolve (name:String, size:Int) = AllResources.resolveOrCreate(GRef.id("WQueue", name)) { new CommChannel (name, size) }
     def destroy (name:String) = AllResources remove GRef.id("WQueue", name)
  }

  /** template WA to clear a channel */
  def clear (cname:String) = 
    WfChannel (cname) + 
    WfResReq (channelRef(cname), "clear", AA(), $0) +
    new WfResReplyIgnore()
  
  /** template WA to put into a channel */
  def put (cname:String, ve:XExpr) = 
    WfChannel (cname) + 
    WfResReq (channelRef(cname), "put", AA(), ve) +
    new WfResReply ()
  
  /** template WA to get from a channel a value and assign to given variable name */
  def get (cname:String, ve:$Expr) = 
    WfChannel (cname) + 
    WfResReq (channelRef(cname), "get", AA(), $(ve.name)) +
    new WfResReply () +
    wrap(new WfeAssign(ve.name, $0))
  
  def channelRef (name:String) = GRef.id ("WQueue", name)
}

/** special workflow activity to define/use a channel. It's used to inject channel syntax into workflow, like "c ! P" */
case class WfChannel (cname:String, clear:Boolean = false, size:Int = 1) extends razie.wf.WfWrapper (
      
  new Wfe1 ("channel", CExpr (cname)) {
    override def apply(in:AC, v:Any) = {
      import CSP.Channel
      val x = Channel resolve (cname, size)
      if (clear) x.get.asInstanceOf[CommChannel].clear
      v
    }
  override def toDsl = "channel ("+clear+","+size+","+expr.toDsl+")"
  }) {
  import CSP.$0
 
  /** channels have a different meaining for this */
  override def | (x:WfAct) : WfAct = this + x
//  override def | (x:WfAct*) : WfAct = x.foldRight (this.asInstanceOf[WfAct]) ((a,b)=>a + b)
  
  def ? (x:WfAct) = (this get $0) + x
  def ? (x:$Expr) = this get x
  def ! (x:$Expr) = this put x
  def ! (x:String) = this put x
  def ! (x:WfAct) = x + (this put $0)
     
  /** out from process into channel */
  def put (ve:XExpr) = { CSP.put (cname, ve) }
  def put (ve:String) = { CSP.put (cname, new CExpr(ve)) }
  def -<- (x:$Expr) = this put x  // allow v(x) + P
  
  /** in process out from channel */
  def get (ve:$Expr) = { CSP.get (cname, ve) }
  def ->- (x:$Expr) = this get x  // allow v(x) + P
  def apply (x:$Expr) = this get x  // allow c(x) + P
  def apply () = this get $0  // allow c(x) + P
  
  def * (x:WfAct) = this + x      // allow v("c") * P === v(c).P
  def apply (x:WfAct) = this + x  // allow v(x) (P Q)
  def apply (x:WfChannel => WfAct) = this + x(this)  // allow v(x) (P Q)
}

/**
 *  PI calculus http://en.wikipedia.org/wiki/Pi-calculus
 * 
 *  PI syntax   our syntax                         description                   
 *  P | Q                                          P and Q in paqrallel
 *              P | Q                              P and Q in paqrallel
 *  (v c) P                                        define channel c
 *              v("c")  
 *              val c = v("c")    
 *  c(x).P                                         read x do P
 *              c ? P                              - implies $0 the default value
 *              c($("x")) + P   
 *              c($0) + P          
 *              val x = $("x"); c(x) + P 
 *              c() + P          
 *              c.get($0) + P          
 *  c<x>.P                                         do P then write x in c
 *              c ! P                              - implies $0 the default value
 *              P + c.put($0) 
 *              P --> c.put($0) 
 *              c.put($0) <-- P
 *              P + (c -<- $0)
 *              val x = $("x"); P + (c -<- x )
 *              
 *  NOTE - $0     is the "default value" and it doesn't need a neame
 *  NOTE - $("x") is the variable named x
 */
object PiCalc extends CSP {

  /** create or clear a channel */
  def v (name:String) = WfChannel(name, true)
  /** create or clear a channel */
  def v (c:WfChannel) = WfChannel (c.cname, true, c.size)

  /** stupid O activity */
  def O = log("O")
  
  def P = log($0 + "-P")
  def Q = log($0 + "-Q")
  def T = log($0) // transparent
}

/** simple library */
trait CspWcf extends WCFBase {
//  override def activities () : Parser[WfAct] = cspwcflib
  def cspwcflib : Parser[WfAct] = channel 

  def boo : Parser[Boolean] = "true" ^^ {s:String => true} | "false" ^^ {s:String => false}
  def channel : Parser[WfAct] = "channel"~"("~boo~","~wholeNumber~","~"\""~ident~"\""~")" ^^ { case "channel"~"("~c~","~s~","~"\""~i~"\""~")" => new WfChannel(i, c, Integer.parseInt(s)) }
} 

//=================================================== samples

object PiSamples extends Application {
  import PiCalc._
 
  def c = Channel("c") // channel c
  def x = $("x") // variable x
  
  def myp01 = v(c) (P | Q)
  println (wf toDsl myp01)
  require ((wf(wf toDsl myp01).print run "1").asInstanceOf[List[_]] contains "1-P")
  
  def myp02 = v(c) (c ? P | c ! Q)  // correct v(c) ( c(0) P | c<0> Q )
  println (wf toDsl myp02)
  require ((wf(wf toDsl myp02).print run "1").asInstanceOf[List[_]] contains "1-Q-P")
  
  def myp11 = v(c) + P  // correct (v c) P
  require ((myp11.print run "1") == "1-P")
  
  def myp21 = v(c) ( c.put($0) --> P )
  def myp22 = v(c) ( c -<- $0 + P )
  def myp23 = v(c)
  require ((myp21 run "1") == "1-P")
  require ((myp22 run "1") == "1-P")
  require ((myp23 run "1") == "1")

  // channel exists, read+P : P waits to read a value and then continues
  def myp31  = c($0) + P        // correct x(0) P
  def myp32 = c ->- $0 + P  // correct x(0) P
  require (((c.put(4) --> myp31).print run "1") == "4-P")
  require (((c.put(4) --> myp32).print run "1") == "4-P")
 
  def myp41 = v(c) (c.get($0) --> P | c.put($0) <-- Q)  // correct v(c) ( c(0) P | c<0> Q )
  require ((myp41.print run "1").asInstanceOf[List[_]] contains "1-Q-P")

  Engines().stop
}

