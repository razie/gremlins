/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins

import razie.AA
import razie.base.{ ActionContext => AC }
import razie.g._
import razie.{wf, gremlins}
import razie.Gremlins

case class WfaCollector(who:Any, collect: WfActivity => Unit)

/** if you need to collect children, push yourself here and pop after xscope defined */
object WfaCollector {
  val curr = new java.lang.ThreadLocal[List[WfaCollector]] {
    override def initialValue() = List[WfaCollector]()
  }
  val flags = new java.lang.ThreadLocal[collection.mutable.Map[String,Boolean]] {
    override def initialValue() = collection.mutable.HashMap[String,Boolean]()
  }

  private[this] def push(who:Any, a: WfActivity => Unit) { curr set WfaCollector(who, a) :: currents }
  private[this] def pop() { curr.set(currents.drop(1)) }
  def currents = curr.get.asInstanceOf[List[WfaCollector]]
  def current = currents.headOption
  def who = current map (_.who)
  def debug(msg: String) = razie.Debug(">>>>>>>>>" + msg + curr.get)

  def collect[T](col: WfActivity => Unit)(who:Any)(f: => T): T = {
    push (who, col )
    val ret = try {
      f
    } finally {
      pop()
    }
    ret
  }

  def noCollect[T](f: => T): T = collect ({ x => {} }) (this) (f)
  def cantCollect[T](block:String) (f: => T): T = collect ({ x => {
    throw new IllegalStateException ("CAN'T define new activities in this block: " + block + " !!!")
    } }) (this) (f)
  def flagged[T] (name:String) (f: => T): T = collect ({ x => {} }) (name) (f)
  def isFlag (name:String) : Option[Boolean] = currents.find(_.who match { 
    case s:String if (s==name) => true
    case _ => false
    }).map (_ => true)
}
