/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables

import razie.actionables.library._

/**
 * this is important: all possible libraries of actions will register here to
 * create their own actions
 * 
 * The executables are identified by their domain and a name, unique inside the
 * domain.
 * 
 * @author razvanc
 */
object Executables {
   // domain:func args             // normal
   // domain:func?args             // url style
   val pat = """(\w+):(\w+)([ \?(])(.*)""".r

  var factories = scala.collection.mutable.Map[String, (razie.AI, ExecutableFactory)]()

  // we can register this by default
  reg(razie.AI("simple"), RazbaseExecutables)
  reg("razie", RazbaseExecutables)

  /** this implementation will figure out the domain and delegate to that factory
   *
   * @param unifiedstring
   * @return
   */
  def make(unifiedstring: String): razie.wf.JWFunc = {
    val pat(domain, cmd, parens, args) = unifiedstring

    factories.get(domain) match {
      case Some(fact) => fact._2.make(unifiedstring)
      case None => throw new IllegalArgumentException("couldn't find domain factory for: " + unifiedstring)
    }
  }

  /** 
   * @return the domains currently registered
   */
  def domains(): Array[razie.AI] = factories.values.map(_._1).toArray

  /** 
   * @param domain
   * @return the actions in this domain
   */
  def commands(domain: String): Option[Array[razie.AI]] = factories get domain map (_._2.commands)

  /** register a new factory for a category */
  def reg(domain: razie.AI, f: ExecutableFactory) = factories.put(domain.name, (domain, f))
}

trait ExecutableFactory {
  /**
   * make an executable. Example unifiedstring is: [domain:command args] i.e.
   * "simple:telnet host=localhost,port=4449,cmd=pause"
   * 
   * @param unifiedstring
   * @return
   */
  def make(unifiedstring: String): razie.wf.JWFunc

  /** return the list of commands available in this factory */
  def commands(): Array[razie.AI]
}
