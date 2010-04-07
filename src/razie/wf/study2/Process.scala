package razie.wf.study2

import razie.AA
import razie.base.{ActionContext => AC}
import razie.wf.{WfaState, ProcStatus, ProcState}

  //-------------------- engine/graph

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

