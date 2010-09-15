/**  ____    __    ____  ____  ____/___      ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___) __)    (  _ \(  )(  )(  _ \
 *   )   / /(__)\  / /_  _)(_  )__)\__ \     )___/ )(__)(  ) _ <
 *  (_)\_)(__)(__)(____)(____)(____)___/    (__)  (______)(____/
 *                      
 *  Copyright (c) Razvan Cojocaru, 2007+, Creative Commons Attribution 3.0
 */


Razie's Pub Workflow Engine
===========================

I've been meaning to write a workflow engine/language for sometime and recently, some scala DSL 
discussions gave me the motive. I know there's tons of workflow products but none complete and to my liking.

Anyways, evolution requires diversity, so enough rationalization (there's more at the bottom), let's 
dive into the fun stuff!

Read more about the [vision](blob/master/Gremlins.markdown), [how to build](blob/master/Building.markdown) or the [design](blob/master/src/razie/wf/Design.markdown).


Why?
----
Threads, shared state, actors, processes...they're all stepping stones. A framework that allows full expressivity for creating 
asynchronous/parallel and distributed work units are what we need.

The three ingredients are: flexibility, expressivity and low cost.

Cost to create a workflow must be low. That implies graphical representations, DSL etc. Cost to run this framework must be low. Cost to re-configure, debug and operate must be low.

The framework must allow users to express any construct they can think of and support it.

Flexibility is a trait embedded in the scala language. The framework must be flexible so new patterns, constructs and whatnot be allowed.


How?
----

There's more details in the vision page, but these are the basic principles behind this project:

1. A workflow is just a graph of activities through which an engine carves one or more concurrent paths
2. The workflow has many views: text DSL, scala DSL, graphical etc
3. There's only a small set of base/core activities.
4. Complex activities are built as patterns or templates from lower-level activities
5. There's a gremlin distribution API, uniformly implemented by all engines and components in a cloud
6. All branches of a bigger workflow, wherever they run, can be related back and managed as a unit


Examples
========


v(c) (c ? P | c ! Q)
--------------------

PI/CSP examples in [CspDemo.scala](blob/master/src/razie/wf/lib/CspDemo.scala)

    def P = wf.log($0 + "-P")                                                                                                             
    def Q = wf.log($0 + "-Q")                                                                                                             
    def c = Channel("c", 0) // channel c, blocking
                                                                                                                                        
    def myp02 = v(c) (c ? P | c ! Q)  // correct v(c) ( c(0) P | c<0> Q )                                                                 
    myp02 run "1" == List("1-Q", "1-Q-P")


Scala DSL Structure
-------------------

Basic DSL contrast (scala vs text) in [WfBaseTest.scala](blob/master/test_src/razie/wf/test/WfBaseTest.scala)

    def wif1 = 
    wif (_ == 1) {  // no good, really - condition not serializable
       wf.inc + wf.log ($0)
     } welse { 
       wf.inc + wf.inc + wf.log ($0)
     }
   
    def wif2s = """
    if ($0 == 1) then {
       inc; log ($0)
     } else {
       inc; log ($0)
     }"""
  
    def testwif11 = expect (2) { wif1.print run 1 }
    def testwif23 = expect (3) { wf(wif2s) run 2 }
  


Text DSL Structure
------------------

Basic DSL examples in [WfBaseTest.scala](blob/master/test_src/razie/wf/test/WfBaseTest.scala)

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

