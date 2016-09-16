
v(c) (c ? P | c ! Q)
--------------------

An asynchronous workflow engine & language exploration (scala internal and external DSL). The engine has two implementations, one with plain threads and one with actors, both sharing the same core.

Read more about the [vision](Gremlins.markdown), [how to build](Building.markdown) or the [design](core/src/main/scala/razie/gremlins/Design.markdown).

Probably more interesting is how to write [scala workflows](ScalaWorkflows.markdown)

How to use
---------------------

The sbt/maven artifact is:

    def gremlins = "com.razie" %% "gremlins"         % "0.6.4-SNAPSHOT"

Make sure that, if you use a SNAPSHOT version, the snapshots repository is added to sbt, as in https://github.com/razie/
scripster/blob/master/project/Build.scala :

    resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                      "releases"  at "http://oss.sonatype.org/content/repositories/releases")


Versions

- 0.6.4-SNAPSHOT is the 2.10.0 build
- 0.6.3-SNAPSHOT is the last 2.9.1 build


Why?
----
Threads, shared state, actors, processes...they're all stepping stones. A framework that allows full expressivity for creating 
asynchronous/parallel and distributed work units are what we need.

The three ingredients are: flexibility, expressivity and low cost.

Cost to create a workflow must be low. That implies graphical representations, DSL etc. Cost to run this framework must be low. Cost to re-configure, debug and operate must be low.

The framework must allow users to express any construct they can think of (i.e. custom templates).

Flexibility is a trait embedded in the scala language. The framework must be flexible so new patterns, constructs and whatnot be allowed.


How?
----

There's more details in the vision page, but these are the basic principles behind this project:

1. A workflow is just a graph of activities through which an engine carves one or more concurrent paths
2. The workflow has many views: text DSL, scala DSL, graphical etc
3. There's only a small set of base/core activities.
4. Complex activities are built as patterns or templates from lower-level activities
5. There's a gremlin distribution API, uniformly implemented by all engines and components in a cloud
6. Branches (sections of the graph) of a bigger workflow could run on multiple devices/agents
7. Distributed branches, wherever they run, can be related back and managed as a unit
8. Since these are all plain graphs, certain graph transformations can be used to turn a state machine into a workflow or a PI into a BPEL or whatever you want into whatever you'd like...


Examples
========


v(c) (c ? P | c ! Q)
--------------------

PI/CSP examples in [CspDemo.scala](core/src/main/scala/razie/gremlins/lib/CspDemo.scala) and [CspTest.scala](core/src/test/scala/razie/gremlins/lib/CspTest.scala)

    def P = wf.log($0 + "-P")
    def Q = wf.log($0 + "-Q")
    def c = Channel("c", 0) // channel c, blocking
                                                                                                                                        
    def myp02 = v(c) (c ? P | c ! Q)  // correct v(c) ( c(0) P | c<0> Q )
    myp02 run "1" == List("1-Q", "1-Q-P")

If you don't know CSP, the process above will launch the two sub-processes P and Q in parallel. P will wait and be invoked for something coming out of the channel c while Q will run and put its result "1-Q" in that channel.

Internal Scala DSL Structure
----------------------------

Basic DSL contrast (scala vs text) in [WfBaseTest.scala](core/src/test/scala/razie/gremlins/WfBaseTest.scala)

    //this is scala code (internal DSL with content assist and all that)
    def wif1 = 
    wif (_ == 1) {  // no good, really - condition not serializable
       wf.inc + wf.log ($0)
     } welse { 
       wf.inc + wf.inc + wf.log ($0)
     }
   
    //this is text, parsed later (external DSL)
    def wif2s = """
    if ($0 == 1) then {
       inc; log ($0)
     } else {
       inc; log ($0)
     }"""
  
    def testwif11 = expect (2) { wif1.print run 1 }
    def testwif23 = expect (3) { wf(wif2s) run 2 }
  


External DSL Structure
-----------------------------

Basic DSL examples in [WfBaseTest.scala](core/src/test/scala/razie/gremlins/WfBaseTest.scala)

    //external dsl format again (parsed by engine)
    def wpar5 = """

    par {
      seq {
        inc
        log($0)
        }
      seq {
        inc
        log($0)
        }
      }"""
      
      def testwpar5 = expect (2::2::Nil) { (wf(wpar5).print run 1) }                                                                        

And, why this workflows library is interesting
==============================================

Different views backed by an internal graph presentation
--------------------------------------------------------

This code 

    def myp02 = v(c) (c ? P | c ! Q) 
    
    println ("dsl:")
    println (wf toDsl myp02)
    println ("graph:")
    myp02.print 
    println ("RRRRRRRRRRRRRESULT is: " + (myp02 run "1"))
  
Will produce this result:

    dsl:
    seq {
      channel (true,0,"c")
      scope par {
        scope seq {
          channel (false,1,"c")
          ResReq(razie.gidref:WQueue:c, Uid-2-1284989059919, get, $0)
          ResReply
          assign $0=$0
          log ($0 + "-P")
        }
        scope seq {
          log ($0 + "-Q")
          scope seq {
            channel (false,1,"c")
            ResReq(razie.gidref:WQueue:c, Uid-3-1284989060116, put, $0)
            ResReply
          }
        }
      }
    }
    
    graph:
    Graph: 
    WfSeq()
    ->channel (true,0,"c")
     ->WfScope()
      ->WfPar()
       ->WfScope()
        ->WfSeq()
         ->channel (false,1,"c")
          ->ResReq(razie.gidref:WQueue:c, Uid-4-1284989060211, get, $0)
           ->ResReply
            ->assign $0=$0
             ->log ($0 + "-P")
              ->WfScopeEnd()
               ->AndJoin 0
                ->WfScopeEnd()
       ->WfScope()
        ->WfSeq()
         ->log ($0 + "-Q")
          ->WfScope()
           ->WfSeq()
            ->channel (false,1,"c")
             ->ResReq(razie.gidref:WQueue:c, Uid-5-1284989060213, put, $0)
              ->ResReply
               ->WfScopeEnd()
                ->WfScopeEnd()
                 ->AndJoin 0
                  ->WfScopeEnd()
    
    RRRRRRRRRRRRRESULT is: List(1-Q-P, 1-Q)

The point is that a simple workflow can be created via internal or external DSL, it is turned
into a graph, which then executes. This graph can be mapped to a visual representation 
(say using BPEL)


Local multithreading
--------------------

Parallel branches run in a separate threads, so it's true multithreading. The engine has a pool of threads.


Timeout
-------   

Nasty timeout - will interrupt the target thread if it takes too long and skip the respective activity:

    def wt1 = timeout (1000) { 
      sleep(5000) 
      }
      
    def testwt1 = expect (true) { razie.Timer { wt1.print run 1 } ._1 < 2000 }


Scala workflows
===============

Due to popular demand, I created a "wfs" version to just run scala code in parallel. Read the detailed user guide [here ](ScalaWorkflows.markdown).

Lazy Example: 
-------------

    import razie.wfs._
    val workflow = seq {    
      par {      
        seq {      
          println ("he he - definition time")
          later { _ + "runtime-a" }
          }
        later { _ + "runtime-b" }
        }
        sort[String] (_ < _)
        matchLater { case x : List[String] => x mkString "," }  
      }

The body of the different nodes are executed as the nodes are run!

Strict Example: 
---------------

    import razie.wfs._
    val workflow = wfs strict seq {    
      par {      
        seq {      
          println ("he he - definition time")
          later { _ + "runtime-a" }
          }
        later { _ + "runtime-b" }
        }
        sort[String] (_ < _)
        matchLater { case x : List[String] => x mkString "," }  
      }

Simulating the let! (let bang) from F#
--------------------------------------

F# has introduced the so-called "asynchronous workflows". What that really is
is a simple lazy invocation of parts of bodies of functions. Here's a simulation
of the let! syntax and the overall behavior, as a scala DSL:

    def wfa1 = seq {
      val a = let! async { _ + "-a" }
      matchLater { case _ => a.get + "-b" }
    }

Follow this approach to assign named variables to intermediary results.

Next stop: the asynchronous monad...for another day!

