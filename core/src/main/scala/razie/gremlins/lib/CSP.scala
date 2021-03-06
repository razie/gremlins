/**
 * ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins.lib

import razie.AA
import razie.base.{ ActionContext => AC }
import razie.gremlins._
import razie.gremlins.res._
import razie.g._
import razie.gremlins.act._
import razie.wf
import razie.wfs
import CSP.Channel

/** communication channel / pipe: put/get values in sequence */
class CommChannel(val _name: String, val _size: Int) extends WQueue(_name, _size) {
  AllResources add this
}

/**
 * CSP - channel based communication between processes - implies multi-way synchronization on channels
 *
 *  Note that I don't really find a good distinction between CSP and PI - see it all under PiCalc below
 */
object CSP extends CSP

trait CSP extends WfLib[WfActivity] with WfBaseLib[WfActivity] {
  me =>

  override def wrap(e: WfExec): WfActivity = new WfWrapper(e)

  /** channel naming convention: it has to be unique in the same agent instance */
  object Channel {
    /** creates a channle with a unique ID recommended approach */
    def apply() = WfChannel(GRef.uid, true)
    /**
     * creates a channle with a given name - NOTE that the name is shared across this AgentJVM
     *  @param size how many values to buffer - 0 means blocking
     */
    def apply(name: String, size: Int) = WfChannel(name, true, size)
    def resolve(name: String, size: Int) = AllResources.resolveOrCreate(GRef.id("WQueue", name)) { new CommChannel(name, size) }
    def destroy(name: String) = AllResources remove GRef.id("WQueue", name)
  }

  /** template WA to clear a channel */
  def clear(channelName: String) = wfs.collectOne {
    WfChannel (channelName) +
      WfResReq (channelRef(channelName), "clear", AA(), $0) +
      new WfResReplyIgnore()
  }

  /** template WA to put into a channel */
  def put(channelName: String, ve: AExpr) = wfs.collectOne {
    WfChannel (channelName) +
      WfResReq (channelRef(channelName), "put", AA(), ve) +
      new WfResReply()
  }

  /** template WA to get from a channel a value and assign to given variable name */
  def get(channelName: String, ve: $Expr) = wfs.collectOne {
    WfChannel (channelName) +
      WfResReq (channelRef(channelName), "get", AA(), $(ve.name)) +
      new WfResReply() +
      wrap(new WfeAssign(ve.name, $0))
  }

  def channelRef(name: String) = GRef.id ("WQueue", name)

  class TempWfa(a: => WfActivity) {
    def >>(x: WfChannel) = x << a
    def <<(x: WfChannel) = x >> a
  }
  implicit def tottwa(a: => WfActivity) = new TempWfa(a)
}

/**
 * special workflow activity to define/use a channel.
 *  It's used to inject channel syntax into workflow, like "c ! P"
 */
case class WfChannel(cname: String, shouldClear: Boolean = false, size: Int = 1)
  extends razie.gremlins.act.WfWrapper(

    new Wfe1("channel", CExpr (cname)) {
      override def apply(in: AC, v: Any) = {
        val x = Channel resolve (cname, size)
        if (shouldClear) x.get.asInstanceOf[WQueue].clear
        v
      }
      override def toDsl = "channel (" + shouldClear + "," + size + "," + expr.toDsl + ")"
    }) {
  import CSP.$0

  /** channels have a different meaining for this */
  override def |(x: WfActivity): WfActivity = apply(x)
  //  override def | (x:WfActivity*) : WfActivity = x.foldRight (this.asInstanceOf[WfActivity]) ((a,b)=>a + b)

  def ?(x: WfActivity) = wfs.collectOne { (this get $0) + x }
  def ?(x: $Expr) = this get x
  def !(x: $Expr) = this put x
  def !(x: String) = this put x
  def !(x: WfActivity) = wfs.collectOne { x + (this put $0) }
  def <<(x: WfActivity) = wfs.collectOne { x + (this put $0) }
  def >>(x: WfActivity) = wfs.collectOne { (this get $0) + x }

  /** out from process into channel */
  def put(ve: AExpr) = { CSP.put (cname, ve) }
  def put(ve: String) = { CSP.put (cname, new CExpr(ve)) }
  def -<-(x: $Expr) = this put x

  /** in process out from channel */
  def get(ve: $Expr) = { CSP.get (cname, ve) }
  def ->-(x: $Expr) = this get x
  def apply(x: $Expr) = this get x // allow c(x) + P
  def apply() = this get $0 // allow c(x) + P

  def *(x: WfActivity) = apply(x) // allow v("c") * P === v(c).P
  def apply(x: WfActivity) = wfs.collectOne { this + x } // allow v(x) (P Q)
  def apply(x: WfChannel => WfActivity): WfActivity = apply (x(this)) // allow v(x) (P Q)

  def clear() = {
    val x = Channel resolve (cname, size)
    x.get.asInstanceOf[WQueue].clear
    this
  }
}

/**
 * PI calculus http://en.wikipedia.org/wiki/Pi-calculus
 *
 *  PI calculus is in essence "parallel processes, communicating via named channels".
 *
 *  PI syntax   our syntax                         description
 *  P | Q                                          P and Q in paqrallel
 *              P | Q                              P and Q in paqrallel
 *
 *  (v c) P                                        define channel c
 *              v("c")
 *              val c = v("c")
 *
 *  c(x).P                                         read x do P
 *              c ? P                              - implies $0 the default value
 *              c($("x")) + P
 *              c($0) + P
 *              val x = $("x"); c(x) + P
 *              c() + P
 *              c.get($0) + P
 *
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
  def v(name: String) = WfChannel(name, true)
  /** create or clear a channel */
  def v(c: WfChannel) = WfChannel (c.cname, true, c.size)

  /** stupid O activity */
  def O = log("O")

  def P = log($0 + "-P")
  def Q = log($0 + "-Q")
  def T = log($0) // transparent
}

/** simple library */
trait CspWcf extends WCFBase {
  //  override def activities () : Parser[WfActivity] = cspwcflib
  def cspwcflib: Parser[WfActivity] = channel

  def boo: Parser[Boolean] = "true" ^^ { s: String => true } | "false" ^^ { s: String => false }
  def channel: Parser[WfActivity] = "channel" ~ "(" ~ boo ~ "," ~ wholeNumber ~ "," ~ "\"" ~ ident ~ "\"" ~ ")" ^^ {
    case "channel" ~ "(" ~ c ~ "," ~ s ~ "," ~ "\"" ~ i ~ "\"" ~ ")" => new WfChannel(i, c, Integer.parseInt(s))
  }
}

/* ambient calculus // http://en.wikipedia.org/wiki/Ambient_calculus
 *
 * TODO
 */
object ambients {
}

