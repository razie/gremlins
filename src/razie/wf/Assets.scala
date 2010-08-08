package razie.wf

import razie.base.{ActionContext => AC}
import razie.g.GRef


object WfAssetCmd {
  def apply (action:String, asset:GRef, args:AC, expr:Map[String,XExpr]) : WfAct = 
     new WfAssetCmd (action, asset, args, expr)
}

class WfAssetCmd (val action:String, val asset:GRef, val args:AC, val expr:Map[String,XExpr]) extends WfSimple with HasDsl {
  override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = {
    val toUse = razie.AA()
    toUse.setAttr(in) 
    toUse.setAttr(args) 
    expr.foreach(m => toUse.set(m._1, m._2 .apply(in, v)))
   
    val outv = razie.g.GAMAct.act (asset, action, toUse)
    
    (outv,glinks)
  }
  
  private[this] def mtos = (expr.map (m=> m._1+"="+m._2)) mkString ","
  override def toDsl = "snak ("+action+","+asset.toString+","+"("+mtos+"))"
}

/** 
 * let's get snakked, yeah!
 */
object snakked {
}
