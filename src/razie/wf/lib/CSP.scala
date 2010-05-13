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

trait CSP extends WfLib[WfAct] with WfBaseLib[WfAct] {
  me =>
   
  override def wrap (e:WfExec) : WfAct = razie.wf.WfWrapper (e)
  
  val channels = new scala.collection.mutable.HashMap[String, CommChannel]()

  class CommChannel (override val name:String) extends WQueue (name) {
     channels += (name -> this)
  }

  object Channel {
     def apply (name:String) = WfChannel (name)
  }
  
  class ChannelWrapper (val a:CommChannel) {
     def | (x:WfAct) = {}
  }
  
  class WfActWrapper (val a:WfAct) {
     def | (x:CommChannel) = {}
  }
  
  def P = log($0 + "-P")
  def Q = log($0 + "-Q")
  def T = log($0) // transparent

  def put (cname:String, ve:$Expr) = new WfResAct (channelRef(cname), "put", AA(), $(ve.name))
  def get (cname:String, ve:$Expr) = 
     new WfResAct (channelRef(cname), "get", AA(), $(ve.name)) + 
     wf.assign (ve.name, $0)
  
  def channelRef (name:String) = GRef.id ("Channel", name)
}

case class WfChannel (cname:String) extends razie.wf.WfWrapper (
  new Wfe1 ("channel", CExpr (cname)) {
    override def apply(in:AC, v:Any) = {
      import CSP.Channel
      Channel (cname)
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
  def put (ve:$Expr) = { CSP.put (cname, ve) }
  
  /** in process out from channel */
  def get (ve:$Expr) = { CSP.get (cname, ve) }
  
  def apply (x:WfChannel) = x + get ($0)  // allow v(x) + P
  def apply (x:$Expr) = this + get (x)  // allow v(x) + P
  def <> (x:$Expr) = this + put (x)  // allow v(x) + P
}


object PiCalc extends CSP {

  def v (name:String) = WfChannel(name)
  def v (c:WfChannel) = c

  def O = log("O")
}

object CSPSamples extends Application {
  import CSP._
 
  def myp = Channel("x") | P
  
//  def hoare = P?x
}

object PiSamples extends Application {
  import PiCalc._
 
  def x = v("x")  // correct syntax: (v x) P
  
//  def myp11 = v(x) ( x.put($0) -> P )
//  require ((myp11 run "1") == "1-P")
  
  def myp12 = v(x) + P  // correct (v x) P
  require ((myp12 run "1") == "1-P")
 
  def myp13 = x($0) + P  // correct x(0) P
  require ((myp12 run "1") == "1-P")
 
//  def myp14 = x<>($0) + P  // correct x<0> P
//  require ((myp12 run "1") == "1-P")
 
 
     
//  def myp = v(x) ( x.out("z") -> O )
//  def myp = v(x) ( x.out("z") -> O )
  
}
