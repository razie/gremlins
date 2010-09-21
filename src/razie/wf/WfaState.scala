/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.AA
import razie.g.GLoc

object LinkState extends Enumeration {
  val CREATED, SELECTED, DONE = Value
}

/** processing states indicate what stage of execution the action's at */
object ProcState extends Enumeration {
  val CREATED, INPROGRESS, DONE = Value
}

/* processing statuses indicate how the action is progressing */
object ProcStatus extends Enumeration {
  val WHATEVER, OK, NOTOK, LATE, STOPPED, SKIPPED = Value
}

case class AuditEvent (val code : String, info:Any*) extends AA (info) 

/** collect any audit information here, such as result of intermediary expressions what not */
trait IsAudited {
  lazy val auditEvents = new scala.collection.mutable.ListBuffer[AuditEvent]()
  
  def audit (e:AuditEvent) = { this.auditEvents += e; this }
}

/**
 * this encapsulates an action's state. it needs to be persisted to recove the action.
 * 
 * since it's a trait, it may be mixed into an action or kept in a parallel structure...your choice
 */
trait WfaState extends IsAudited {
  var procState : ProcState.Value   = ProcState.CREATED
  var procStatus : ProcStatus.Value = ProcStatus.WHATEVER 

  def wfaPersistString : String = "WfaState["+procState+","+procStatus+"]"
  def wfaLoadFromPersistString (s:String) {
    val pat = """WfaState\[\([^,]*\),\([^,]*\)]""".r
    val pat(e, u) = s
    procState = ProcState.withName(e)
    procStatus = ProcStatus.withName(u)
  }
  
  val key = razie.g.GRef.uid()
}

/** I figure even links need state, so here it is... */
trait WflState {
  var linkState : LinkState.Value   = LinkState.CREATED

  /** collect any audit information here, such as result of intermediary expressions what not */
  lazy val audit : AA = AA()
  
  def wfaPersistString : String = "WflState["+linkState+"]"
  def wfaLoadFromPersistString (s:String) {
    val pat = """WflState\[\([^,]*\)]""".r
    val pat(e) = s
    linkState = LinkState.withName(e)
  }
}

// TODO implement the audit
