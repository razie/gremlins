/**
 * ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins.eng

import razie.AA
import razie.g.GRef
import razie.gremlins.WfActivity

object Audit {
  private def audit(aa: AA) = razie.Audit(aa)

  val EV = "Event"
  val PR = "process"
  val AC = "activity"
  val VAL = "value"

  def recCreate(a: Process) = audit(razie.AA(EV, "CREATE", PR, a))
  def recStart(a: Process, v: Any) = audit(razie.AA(EV, "START", PR, a, VAL, v.asInstanceOf[AnyRef]))
  def recDone(a: Process, v: Any) = audit(razie.AA(EV, "DONE", PR, a, VAL, v.asInstanceOf[AnyRef]))
  def recExecBeg(a: WfActivity, in: Any) =
    audit(razie.AA(EV, "EXEC.beg", AC, a, "in", in.asInstanceOf[AnyRef]))
  def recExecEnd(a: WfActivity, in: Any, out: Any, paths: Int) =
    audit(razie.AA(EV, "EXEC.end", AC, a, "in", in.asInstanceOf[AnyRef], "out", out.asInstanceOf[AnyRef], "paths", paths))
  def recResNotFound(a: WfActivity, res: GRef) =
    audit(razie.AA(EV, "ERROR.resNotFound", AC, a, "res", res))
}
