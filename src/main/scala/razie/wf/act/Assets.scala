/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.act

import razie.wf._
import razie.base.{ ActionContext => AC }
import razie.g.GRef

/** gremlin that invokes a command on an asset - see the snakked framework for details on assets */
case class WfAssetCmd(val action: String, val asset: GRef, val args: AC, val expr: Map[String, AExpr]) extends WfSimple with HasDsl {
  override def traverse(in: AC, v: Any): (Any, Seq[WfLink]) = {
    val toUse = razie.AA()
    toUse.setAttr(in)
    toUse.setAttr(args)
    expr.foreach(m => toUse.set(m._1, m._2.apply(in, v)))

    val outv = razie.g.GAMAct.act(asset, action, toUse)

    (outv, glinks)
  }

  private[this] def mtos = (expr.map(m => m._1 + "=" + m._2)) mkString ","
  override def toDsl = "snak (" + action + "," + asset.toString + "," + "(" + mtos + "))"
}

/** 
 * let's get snakked, yeah!
 */
object snakked {}
