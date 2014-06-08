/**
 * ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._
import razie.actionables._
import razie.actionables.ActionableSpec
import razie.base.ActionContext
import razie.AI.stoai

/**
 * sample activity - just add X to the argument
 */
class ExecX extends razie.gremlins.JWFunc {

  def spec: ActionableSpec = new ActionableSpec(razie.AA("someArg=default"), razie.AA("result"));

  override def apply(in: ActionContext, v: Any): Any = {
    val msg = in sa "someArg"

    razie Log ("adding X to " + msg)

    msg + "X"
  }
}

/**
 * factory for the basic executables offered
 */
object SampleExecutables extends ExecutableFactory {

  /**
   * make an executable
   *
   * @param unifiedstring
   * @return
   */
  override def make(unifiedstring: String): razie.gremlins.JWFunc = {
    val Executables.pat(domain, cmd, parens, args) = unifiedstring

    require(cmds contains cmd, "need to support command: " + cmd)

    cmd match {
      case "x" => new ExecX
    }
  }

  /**
   * @param domain
   * @return the actions in this domain
   */
  override def commands(): Array[razie.AI] = cmds map (new razie.AI(_))

  val cmds = Array("x")
}

class SampleExecFactoryTest extends JUnit3Suite {
  import Actionables._

  Executables.reg("sample", SampleExecutables)

  def testX: Unit = expect("ExecX") {
    val o = Executables.make("sample:x someArh=hihi")
    o.getClass().getSimpleName
  }
}
