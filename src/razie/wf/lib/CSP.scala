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

/** bpel activities implemented as wf templates 
 * 
 * http://www.oasis-open.org/committees/download.php/23964/wsbpel-v2.0-primer.htm
 */
object CSP extends CSP

 class CommChannel (override val name:String) extends WQueue (name) {
    AllResources add this
 }

trait CSP extends WfLib[WfAct] with WfBaseLib[WfAct] {
  me =>
   
  override def wrap (e:WfExec) : WfAct = razie.wf.WfWrapper (e)
  
  object Channel {
     def apply (name:String) = WfChannel(name)
     def resolve (name:String) = AllResources.resolveOrCreate(GRef.id("WQueue", name)) { new CommChannel (name) }
  }
  
//  class ChannelWrapper (val a:CommChannel) {
//     def | (x:WfAct) = {}
//  }
//  
//  class WfActWrapper (val a:WfAct) {
//     def | (x:CommChannel) = {}
//  }
//  
  def clear (cname:String) = 
    WfChannel (cname) + 
    WfResReq (channelRef(cname), "clear", AA(), $0) +
    new WfResReply {
     override def reply (r:WResReqReply) {}
  }
  
  def put (cname:String, ve:XExpr) = 
    WfChannel (cname) + 
    WfResReq (channelRef(cname), "put", AA(), ve) +
    new WfResReply {
     override def reply (r:WResReqReply) {}
  }
  
  def get (cname:String, ve:$Expr) = 
    WfChannel (cname) + 
    WfResReq (channelRef(cname), "get", AA(), $(ve.name)) +
    new WfResReply {
     var rreply:WResRROK = null

     override def reply (r:WResReqReply) = rreply = r.asInstanceOf[WResRROK]
     
     override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = {
        val x = new WfeAssign(ve.name, $0) apply (in, rreply.result)
        (x,glinks.headOption.toList)
     }
  }
  
  def channelRef (name:String) = GRef.id ("WQueue", name)
}

case class WfChannel (cname:String, clear:Boolean = false) extends razie.wf.WfWrapper (
  new Wfe1 ("channel", CExpr (cname)) {
    override def apply(in:AC, v:Any) = {
      import CSP.Channel
      val x = Channel resolve cname
      if (clear) x map (_.asInstanceOf[CommChannel].clear)
      v
    }
  }) {
  import CSP.$0
 
  /** channels have a different meaining for this */
  override def | (x:WfAct) : WfAct = this + x
  
//  override def ? (x:String) = this in x
//  override def ? (x:$Expr) = this in x.name
//  override def ! (x:String) = this out x
//  override def ! (x:$Expr) = this out x.name
     
  /** out from process into channel */
  def put (ve:XExpr) = { CSP.put (cname, ve) }
  def put (ve:String) = { CSP.put (cname, new CExpr(ve)) }
  
  /** in process out from channel */
  def get (ve:$Expr) = { CSP.get (cname, ve) }
  
  def * (x:WfAct) = this + x      // allow v("c") * P === v(c).P
  def apply (x:WfAct) = this + x  // allow v(x) (P Q)
  def apply (x:WfChannel => WfAct) = this + x(this)  // allow v(x) (P Q)
  def apply (x:$Expr) = this + get (x)  // allow c(x) + P
  def apply () = this + get ($0)  // allow c(x) + P
  def ->- (x:$Expr) = this + get (x)  // allow v(x) + P
  def -<- (x:$Expr) = this + put (x)  // allow v(x) + P
}

/**
 *  PI calculus http://en.wikipedia.org/wiki/Pi-calculus
 * 
 *  PI syntax   our syntax       description                   
 *  P | Q       P | Q              P and Q in paqrallel
 *  (v c) P     v("c")            define channel c
 *              val c = v("c")    
 *  c(x).P      c($("x")) + P          read x do P
 *              c($0) + P          
 *              val x = $("x");   c(x) + P 
 *              c() + P          
 *              c.get($0) + P          
 *  c<x>.P      c.put($0) + P        write x in c do P
 *              c -<- $0 + P       
 *              val x = $("x");   c -<- x + P 
 *  NOTE - $0     is the "default value" and it doesn't need a neame
 *  NOTE - $("x") is the variable named x
*/
object PiCalc extends CSP {

//  def v (name:String) = CSP.clear(name)
  def v (name:String) = WfChannel(name, true)
  def v (c:WfChannel) = WfChannel (c.cname, true)

  def O = log("O")
}

object CSPSamples extends Application {
  import CSP._
 
  def P = log($0 + "-P")
  def Q = log($0 + "-Q")
  def T = log($0) // transparent

  def myp = Channel("c") | P
  
//  def hoare = P?x
  Engines().stop
}

object PiSamples extends Application {
  import PiCalc._
 
  def P = log($0 + "-P")
  def Q = log($0 + "-Q")
  def T = log($0) // transparent

  def c = Channel("c") // channel c
  def x = $("x") // variable x
  
  // declare channel - no effect really
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
//  def myp42 = v("d") (d => d($0) + P | d.put($0) <-- Q)  // correct v(d) ( d(0) P | d<0> Q )
  require ((myp41.print run "1").asInstanceOf[List[_]] contains "1-Q-P")
//  require ((myp42.print run "1").asInstanceOf[List[_]] contains "1-Q-P")
  

  Engines().stop
}
