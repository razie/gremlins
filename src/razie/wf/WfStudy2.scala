package razie.wf

import razie.AA
import razie.base.{ActionContext => AC}


/** 
 * in this version, I'm adding the underlying structure for an engine to run through. this gives
 * state persistency, distribution and visualization 
 * 
 * @author razvanc
 */
object Wf2 {
  //  type Cond : => Boolean
   
//  class wabase[T<:WfActBase] {
//    def log (f: => String) = new WfActBase { override def execu () = println(f) }
//  }

  //-------------------- engine/graph

  /** 
   * the workflow is modelled as a graph of activities connected by links/dependencies.
   * 
   * Mixing in the state alos allows its removal, should I decide to store it outside, later...cool, huh?
   */
  abstract case class WA () extends WfActBase with razie.g.GNode[WA, WL] with WfaState {
    override def gnodes = activities
    override def glinks = links
    
    def activities : Seq[WA] // next activities - note that this is not containment, is it?
    def links : Seq[WL] // links 
   
    /** executing these means maybe doing something (in=>out) AND figuring out who's next */
    def execute (in:AC) : (AC,Seq[WL])
  }
 
  /** may want to store some color here...or have the link do something */
  case class WL (val a:WA, val z:WA) extends razie.g.GLink[WA] { }
  
  /** 
   * a process thread encapsulates the state of a running workflow branch . You can see this as an 
   * actor as well
   */
  class ProcessThread (val start:WA, val ctx:AC) { 
    // should i keep state inside the activities or outside in a parallel graph, built as it's traversed?
     
    // the activities in progress these are all in parallel
    var currAct : WA = null
    
    // the activities to start on the next step
    var nextAct : WA = start 
   
    // advance to next action and return if there's more threads to spawn
    protected def pre = {
      currAct = nextAct
      currAct.procState = ProcState.INPROGRESS 
    }
    // execute action and return continuations: either myself or a bunch of spawns
    protected def exec : Seq[ProcessThread] = {
       val (out, next) = currAct.execute(ctx)
       if (next.size == 1) {
         nextAct = next.first.z
         this :: Nil // myself continues
       } else {
         nextAct = null 
         if (next.size > 1) 
            next.map (l=>new ProcessThread(l.z,ctx))
           // note that I'm done in this case and replace myself with these spawns
         else 
           Nil // done - no continuations
       }
    }
    // advance to next action and return if there's more threads to spawn
    protected def post = {
      currAct.procState = ProcState.DONE 
    }
    
    // advance to next action and return if there's more threads to spawn
    def move () : Seq[ProcessThread] = {
      pre
      val res = exec
      post
      res
    }
    
    def done () : Boolean = nextAct == null
  }

  /** a process instance encapsulates the state of the running owrkflow */
  class Process (val start:WA, val ctx:AC) { 
    // the threads in progress these are all in parallel
    var currThreads : Seq[ProcessThread] = new ProcessThread (start, ctx) :: Nil
    var oldThreads : Seq[ProcessThread] = Nil // don't know why i keep these

    // progress all threads - this is temp until I get the threading done
    def tick { currThreads = currThreads.flatMap(tick(_)) }
   
    // progress one thread - this would normally happen on its own thread
    def tick (p:ProcessThread) : Seq[ProcessThread] =  p.move
    
    def done () : Boolean = currThreads.isEmpty
  }

  /** the engine is more complicated in this case, basically graph traversal */
  class Engine { 
    def exec (start:WA, ctx:AC) = {
      val p = new Process (start, ctx)       
      
      while (!p.done) p.tick
    }
     
    def checkpoint () {}
  }

  //----------------- activitities
  
  def wif (cond : => Boolean) (t:WfAct) = WfIf (()=>cond, t)
  implicit def wf  (f : => Unit) = WfScala (()=>f)

  trait WfAct extends WA {
     def welse (e:WfAct) = WfElse (e)
     def wf  (f : => Unit) = WfScala (()=>f)
     
     def + (e:WfAct) = WfSeq (this,e)
     def | (e:WfAct) = WfPar (this,e)
  }

  /** simple activities just do their thing */
  case class WfSimple extends WfAct { 
    override def activities : Seq[WA] = Nil
    override def links : Seq[WL] = Nil
   
    def execute (in:AC) : (AC,Seq[WL]) =  (exec(in), links)
  }

  case class WfProxy (a:WfAct, l:WL*) extends WfAct { 
    override def exec (in:AC) : AC = a.exec(in)
    
    override def activities : Seq[WA] = Nil
    override def links : Seq[WL] = l 
   
    /** executing these means maybe doing something (in=>out) AND figuring out who's next */
    def execute (in:AC) : (AC,Seq[WL]) =  (exec(in), l)
  }

  /** a sequence contains a list of proxies */
  case class WfSeq (a:WfAct*) extends WfAct {
    override val activities : Seq[WA] = build
    override val links : Seq[WL] = a.firstOption.map(WL(this,_)).toList

    def build : Seq[WA] = {
      var acc = scala.collection.mutable.ListBuffer[WA]()
      var nxt:WfAct = null
      for (x <- a.reverse) {
        val l : Seq[WL] = if (nxt == null) Nil else WL(x, nxt) :: Nil
        acc prepend WfProxy(x, l:_*)
        nxt=x
      }
      acc
    }
   
    /** does nothing just returns the first in list */
    def execute (in:AC) : (AC,Seq[WL]) = (in, links)
  }
  
  case class WfPar (a:WfAct*) extends WfAct {
    override def activities : Seq[WA] = a
    override val links : Seq[WL] = a.map (x => WL(this,x))
   
    /** does nothing just returns the spawns */
    def execute (in:AC) : (AC,Seq[WL]) =  (in, links)
  }

  case class WfScala (val f : () => Unit) extends WfSimple { 
     override def exec (in:AC) : AC = { f; in }
  }   

  case class WfElse (t:WfAct)  extends WfProxy (t)
  
  case class WfIf   (val cond : () => Boolean, t:WfAct, e:WfElse*) extends WfAct {
    override def activities : Seq[WA] = List(t) ::: e.toList
    override val links : Seq[WL] = List (WL(this,t)) ::: e.map(WL(this,_)).toList
   
    /** return either branch, depending on cond */
    def execute (in:AC) : (AC,Seq[WL]) = 
      if (cond()) 
        (in, links.first :: Nil)
      else
        (in, e.firstOption.map(WL(this,_)).toList)
  }
  
  // test
  val wd = wif (1==2) {
     println ("then")
  } welse {
     println ("else")
  }
}

object Wf2Main extends Application {
  println ("pinkie")
  new Wf2.Engine().exec(Wf2.wd, razie.base.scripting.ScriptFactory.mkContext())
}
