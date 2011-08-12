/**
 * ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.dsl;

/** a collector has a 'name' and how to collect something */
case class DslCollector[A](who: Any, collect: A => Unit)

/**
 * static collectors, organized as a stack per each ThreadLocal
 *
 * Typefoolery:
 * <li>A stands for the generally collected type or Any
 * <li>T below is the type of the following block, simply returned after collection - often the same as A
 *
 * if you need to collect children, push yourself here and pop after xscope defined
 */
class DslCollectors[A] {
  val curr = new java.lang.ThreadLocal[List[DslCollector[A]]] {
    override def initialValue() = List[DslCollector[A]]()
  }

  val flags = new java.lang.ThreadLocal[collection.mutable.Map[String, Boolean]] {
    override def initialValue() = collection.mutable.HashMap[String, Boolean]()
  }

  private[this] def push(who: Any, collector: A => Unit) { curr set DslCollector(who, collector) :: currents }
  private[this] def pop() { curr.set(currents.drop(1)) }

  def currents = curr.get.asInstanceOf[List[DslCollector[A]]]
  def current = currents.headOption
  def who = current map (_.who)
  def debug(msg: String) = razie.Debug(">>>>>>>>>" + msg + curr.get)

  /** collect the following block */
  def collect[T](collector: A => Unit)(who: Any)(f: => T): T = {
    push(who, collector)
    val ret = try {
      f
    } finally {
      pop()
    }
    ret
  }

  /** don't collect this - you can nest this inside a collector block */
  def noCollect[T](f: => T): T = collect({ x: A => {} })(this)(f)

  /** get pissed if anyone wants collected in the following block */
  def cantCollect[T](block: String)(f: => T): T = collect({ x: A =>
    {
      throw new IllegalStateException("CAN'T define new activities in this block: " + block + " !!!")
    }
  })(this)(f)

  def flagged[T](name: String)(f: => T): T = collect({ x: A => {} })(name)(f)

  def isFlag(name: String): Option[Boolean] = currents.find(_.who match {
    case s: String if (s == name) => true
    case _ => false
  }).map(_ => true)
}

/**
 * generic purpose collector
 */
object DslCollector extends DslCollectors[Any] {}
