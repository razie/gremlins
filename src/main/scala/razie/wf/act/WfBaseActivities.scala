/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.act

import razie.AA
import razie.base.{ ActionContext => AC }
import razie.wf._

/**
 * basic workflow patterns
 * 
 * http://www.yawlfoundation.org/resources/patterns,
 * http://www.workflowpatterns.com/documentation/
 *
 * basic patterns: seq, split-and, split-xor, and-join, xor-join, cancel
 */

//-------------------------------- basic activities

/** simple activities just do their thing */
class WfSimple extends WfActivity {
  override def traverse(in: AC, v: Any): (Any, Seq[WfLink]) = this match {
    case a: WfExec => (a.apply(in, v), glinks)
    case _ => (v, glinks)
  }
}

/** note that this proxy is stupid... see WfElse to understand why... */
class WfProxy(a: WfActivity, var l: WfLink*) extends WfSimple {
  // if the depy was from a to someone, update it to be this to someone...?
  glinks = l.map(x => { if (x.a == a) new WfLink(this, x.z) else x })

  override def traverse(in: AC, v: Any): (Any, Seq[WfLink]) = (a.traverse(in, v)._1, glinks)

  //  override def toString: String =
  //    this.getClass().getSimpleName + "()"
}

/** simple activities just do their thing */
class WfWrapper(wrapped: WfExec) extends WfSimple with WfExec with HasDsl {
  override def apply(in: AC, prevValue: Any) = wrapped.apply(in, prevValue)

  override def toString: String = wrapped match {
    case d: notisser => "wf." + wrapped.wname
    case d: HasDsl => d.toDsl + "(" + key + ")"
    case _ => "wf." + wrapped.wname
  }

  override def wname = wrapped.wname

  override def toDsl = wf toDsl wrapped
}

//------------------- begin / end subgraph

/** sub-graph root, control node: create new control node linking to all others */
class WfStart(a: WfActivity*) extends WfSimple { a map (this --> _) }

/** sub-graph end, control node: find all ends of subgraph and point to this end */
class WfEnd(a: WfActivity*) extends WfSimple {
  // find all distinct leafs and connect them to me distint because 1->2->4 and 1->3->4
  (a flatMap (x => razie.g.Graphs.filterNodes[WfActivity, WfLink](x) { z => z.glinks.isEmpty })).distinct foreach (i => i +-> this)
}

/** 
 * the new proxy: contains a sub-graph.
 * will point to the entry point of its sub-graph and connect the end of it to itself.
 */
class WfScope(aa: WfActivity, var l: WfLink*) extends WfStart(aa) with HasDsl {
  new WfScopeEnd(aa, l: _*)
  override def toDsl = "scope " + (wf toDsl aa)
}

/** 
 * special activity - ends a scope and points to where the scope was meant to point to
 */
class WfScopeEnd(s: WfActivity, var l: WfLink*) extends WfEnd(s) {
  glinks = l.map(x => { if (x.a == s) new WfLink(this, x.z) else x })
}

//------------------------- selector

/** selector activity selects one of the branches, based on its value - the basis for many other */
class WfSelOne(expr: WFunc[_] = WFuncNil) extends WfSimple {

  override def traverse(in: AC, v: Any): (Any, Seq[WfLink]) = {
    val sel = expr.apply(in, v)
    (v, glinks.filter(l => l.isInstanceOf[WfLinkV] && l.asInstanceOf[WfLinkV].selector == sel).headOption.toList)
  }

  /** set new branches to follow with value: this -> (b,c) */
  def -->[T <: Any](z: => Map[T, WfActivity]) = {
    glinks = z.map(p => new WfLinkV(this, p._2, p._1)).toList
    this
  }

  /** add new branch to follow with value: this -> (b,c) */
  def +->[T <: Any](z: => Map[T, WfActivity]) = {
    glinks = glinks.toList ::: z.map(p => new WfLinkV(this, p._2, p._1)).toList
    this
  }

  /** add new branch to follow with value: this -> (b,c) */
  def +->[T <: Any](z: (T, WfActivity)*) = {
    glinks = glinks.toList ::: z.map(p => new WfLinkV(this, p._2, p._1)).toList
    this
  }
}

/** selector activity selecting more than one option - the basis for many other */
class WfSelMany(expr: WFunc[_]) extends WfSimple {

  override def traverse(in: AC, v: Any): (Any, Seq[WfLink]) = {
    val sel = expr.apply(in, v)
    (v, glinks.filter(l => l.isInstanceOf[WfLinkV] && l.asInstanceOf[WfLinkV].selector == sel))
  }
}

//-------------------------funky

class WfLabel(name: String, aa: WfActivity) extends WfProxy(aa)

class WfWhen(name: String, aa: WfActivity) extends WfProxy(aa) { // TODO needs to find the labeled one
  // TODO needs to wait 
}

//------------------------------- seq / par

/** bounday of a subgraph: it acts like the head but all NEXT operations act on the end */
abstract class WfBound extends WfSimple {
  def lastAct: WfActivity

  /** reroute */
  override def -->[T <: WfActivity](z: T)(implicit linkFactory: LFactory): WfActivity = {
    lastAct.glinks = linkFactory(lastAct, z) :: Nil
    this
  }
  /** add a new dependency */
  override def +->[T <: WfActivity](z: T)(implicit linkFactory: LFactory): WfActivity = {
    lastAct.glinks = lastAct.glinks.toList.asInstanceOf[List[WfLink]] ::: List(linkFactory(lastAct, z))
    this
  }
  /** par depy a -> (b,c) */
  override def -->[T <: WfActivity](z: Seq[T])(implicit linkFactory: LFactory): WfActivity = {
    lastAct.glinks = z.map(linkFactory(lastAct, _)).toList
    this
  }
  /** par depy a -> (b,c) */
  override def +->[T <: WfActivity](z: Seq[T])(implicit linkFactory: LFactory): WfActivity = {
    lastAct.glinks = lastAct.glinks.toList.asInstanceOf[List[WfLink]] ::: z.map(linkFactory(lastAct, _)).toList
    this
  }
}

/** a sequence contains a list of proxies 
 * 
 * NOTE this is a scoped activity - it will scope the enclosed sub-graphs
 */
class WfSeq(a: WfActivity*) extends WfBound with HasDsl {
  override def lastAct: WfActivity = {
    // while building dynamic worklfows, there could be a seq no nodes but 1 link...
    glinks.lastOption.map(x => gnodes.lastOption.getOrElse(this)).getOrElse(this)
  }

  // wrap each in a proxy and link them in sequence
  // TODO deprecate gnodes
  var gnodes: Seq[WfActivity] = Nil // next activities - note that this is not containment, is it?
  gnodes =
    a.foldRight(Nil: List[WfActivity])((x, l) => wf.scope(x, l.headOption.map(new WfLink(x, _)).toList: _*) :: l)
  glinks = gnodes.headOption.map(new WfLink(this, _)).toList

  // to avoid seq(seq(t)) we redefine to just link to what i already have
  override def +(e: WfActivity) = {
    val p = wf.scope(e)
    if (glinks.lastOption.isDefined)
      gnodes.last --| p
    else
      glinks = List(new WfLink(this, p))
    gnodes = gnodes.toList ::: List(p)
    this
  }

  override def toDsl = "seq {\n" + (wf indent gnodes.map(wf toDsl _).mkString("\n")) + "\n}"
} // to avoid seq(seq(t)) we redefine to just link to what i already have

/** fork-join. The end will wait for all processing threads to be done 
 * 
 * NOTE this is a scoped activity - it will scope the enclosed sub-graphs
 */
class WfPar(a: WfActivity*) extends WfBound with HasDsl {
  lazy val aj = WfaCollector.noCollect { new eng.AndJoin() }

  var gnodes = a map wf.scope
  glinks = gnodes map (x => new WfLink(this, x))
  this --| aj

  override def lastAct: WfActivity = glinks.lastOption.map(x => gnodes.last).getOrElse(this)

  // to avoid par(par(t)) we redefine to just link to what i already have
  override def |(e: WfActivity) = {
    val p = wf.scope(e)
    glinks = List(new WfLink(this, p)) ::: glinks.toList
    gnodes = gnodes.toList ::: List(p)
    p --| aj
    this
  }

  //    // I don't understand this doesn't work
  //    override def | (e:WfActivity*) = {
  //      e.foreach {f=>
  //        val p = wf.scope(f)
  //        glinks = List(WfLink(this,p)) ::: glinks.toList
  //        gnodes = gnodes.toList ::: List(p) 
  //        p --| aj
  //        }
  //      this
  //    }

  override def toDsl = "par {\n" + (wf indent gnodes.map(wf toDsl _).mkString("\n")) + "\n}"
}

//---------------- scala code - not serializable though

/** scala code with no input nor return values */
class WfScala(f: () => Unit) extends WfSimple with WfExec {
  override def apply(in: AC, v: Any): Any = { f(); v }
}

class WfeScala(f: => Unit) extends WfExec {
  override def apply(in: AC, v: Any): Any = { f; v }
}

/** scala code with input and return values */
class WfeScalaV1(val f: (Any) => Any) extends WfExec {
  override def apply(in: AC, v: Any): Any = f(v)
}

/** scala code with a return value but no input */
class WfScalaV0(val f: () => Any) extends WfSimple with WfExec {
  override def apply(in: AC, v: Any): Any = f()
}

/** scala code with input and return values */
class WfScalaV1(val f: (Any) => Any) extends WfSimple with WfExec {
  override def apply(in: AC, v: Any): Any = f(v)
}

/** scala code with input and return values */
class WfScalaV1u(val f: (Any) => Unit) extends WfSimple with WfExec {
  override def apply(in: AC, v: Any): Any = { f(v); v }
}

//------------------------------------ if

case class WfElse(t: WfActivity) extends WfScope(t)

/** 
 * if-then-else
 * 
 * NOTE this is a scoped activity - it will scope the enclosed sub-graphs when serialized
 * TODO NOTE this is NOT a scoped activity - it will NOT scope the enclosed sub-graphs if not serialized
 */
case class WfIf(val cond: WFunc[Boolean], t: WfActivity, var e: Option[WfElse] = None)
  extends WfSelOne(cond) with HasDsl {

  this +-> (true -> t)
  e map (x => this +-> (false -> x))

  // syntax helper
  def welse(a: WfActivity): WfIf = {
    if (e.isEmpty) {
      this.e = Some(WfElse(a)) // to make sure that for next welse is not empty
      this +-> Map(false -> this.e.get)
    } else // oops, is this an welse wif welse?
      this.e.head.t match {
        case i: WfIf => i.welse(a)
        case _ => razie.Error("a second welse clause") // TODO cause syntax error
      }
    this
  }

  override def toDsl =
    "if (" + (wf toDsl cond) + ") then " + (wf toDsl t) +
      (e map (" else " + _.toDsl)).mkString
}

//------------------- dynamic seq/par for scala qorkflows

/** dynamic sequence collects its body at runtime - not nice
 */
class WfDynSeq(a: WfExec) extends WfSeq() {
  
  override def traverse(in: AC, v: Any): (Any, Seq[WfLink]) = {
    val lb = new collection.mutable.ListBuffer[WfActivity]
    val out = WfaCollector.collect (lb append _) {
      a.apply (in, v)
    }

    // construct with the colected
    //lb map (this + _)
    razie.Debug ("Collected: " + lb)
    
    if (lb.size > 0) {
      razie.Debug ("Old dynamic sub-graph... follows:"); this.print

      val newseq = WfaCollector.noCollect { 
        new WfSeq(lb: _*) 
        }
      
      // better be bounded
      glinks.headOption.map (newseq --| _.z)
      val newglinks = List(new WfLink(lastAct, newseq))
      lastAct.glinks = newglinks
      razie.Debug ("New dynamic sub-graph... follows:"); this.print
      
      (out, newglinks)
    } else
      (out, glinks)
  }

  // to avoid seq(seq(t)) we redefine to just link to what i already have
  override def +(e: WfActivity) = {
    val p = wf.scope(e)
    if (glinks.lastOption.isDefined)
      gnodes.last --| p
    else
      glinks = List(new WfLink(this, p))
    gnodes = gnodes.toList ::: List(p)
    this
  }

}

/** dynamic par collects its body at runtime - not nice
 */
class WfDynPar(a: WfExec) extends WfPar() {
  override def traverse(in: AC, v: Any): (Any, Seq[WfLink]) = {
    val lb = new collection.mutable.ListBuffer[WfActivity]
    val out = WfaCollector.collect (lb append _) {
      a.apply (in, v)
    }

    // construct with the colected
    //lb map (this + _)
    razie.Debug ("Collected: " + lb)
    if (lb.size > 0) {
      razie.Debug ("Old dynamic sub-graph... follows:"); this.print
      //decouple AJ from me
      aj.incoming = Nil
      glinks = Nil

      lb map (this | _)

      // this is runtime so aj needs rebuild cache
      (razie.g.Graphs.filterNodes[WfActivity, WfLink](this) { a =>
        a.glinks.headOption.map(_.z.key == aj.key).getOrElse(false)
      }) foreach (n => aj addIncoming n.glinks.head)

      razie.Debug ("New dynamic sub-graph... follows:"); this.print
      (out, glinks)
    } else
      (out, glinks)
  }

}

/** scala code with input and return values */
class WfFoldLeft[T](zero: T)(plus: (T, T) => T) extends WfSimple with WfExec {
  override def apply(in: AC, v: Any): Any = {
    v.asInstanceOf[List[T]].foldLeft (zero) (plus)
  }
}

/** scala code with input and return values */
class WfSort[T](lt: (T, T) => Boolean) extends WfSimple with WfExec {
  println ("-------------- sort")
  override def apply(in: AC, v: Any): Any = {
    v.asInstanceOf[List[T]].sortWith(lt)
  }
}

