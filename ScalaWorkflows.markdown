Scala workflows
==================

A scala workflow is one where the activities are scala code. 

The activity builders
==============

Will walk through the main activity builders, so you can understand how they work. There's just two categories right now with about 2 each, so 4 in total.


seq, par
-----------

    def seq(f: => Unit): WfActivity
    def par(f: => Unit): WfActivity

The signature tells you that it takes an anonymous piece of code as a definition body, will run it at definition time and collect any activities defined by embedded builders.

The difference between seq/par is not at definition time but at run-time. That's when par will launch its collected activities in parallel rather than sequence. Also, the resulting value of a seq activity is the value of its last child activity while par, it acts like a fork-join and will collect the values returned by each of its parallel branches into a list.


later, matchLater
-----------------

While seq/par deal with the structure of the workflow, later/matchLater build leaf activities which cannot have children. If you use any other builder inside one of these, you should get an error at definition time.

    def later         (f: Any => Any) : WfActivity 
    def valueOf       (f: => Any) : WfActivity 
    def matchLater[B] (f: PartialFunction[Any, B]) : WfActivity

The signature shows another difference: these contain the actual body of scala code the activity will execute at run-time. It also makes them applicable to different constructs:

    later   { woohoo("b") _ }

    valueOf { woohoo("b") (2) }

    matchLater {
      case l: List[String] => l mkString ","
    }


Strict versus non-strict
========================

The same workflow can be built in "lazy" mode or "strict" mode. This shows in multi-level workflows:

    // multilevel - simple scala code
    def wsp4 =
      seq {
        println ("------------------woohoo start")
        par {
          seq {
            println ("------------------woohoo build a")
            later { woohoo("a") _ }
          }
          seq {
            println ("------------------woohoo build b")
            later { woohoo("b") _ }
          }
        }
      }
        
This is a lazy workflow. What hapens is that the second level of seq is only executed at runtime. In this default mode, the workflow structure is built dynamically and the difference between the definition pass and run pass is blurred.

This mode is very useful if all you want to do is run scala code with the seq/par constructs (i.e. nice and easy fork/join DSL).

    def wsp5 = wfs strict wsp4

We can ensure the thing is ran strict. What this means is that ALL definition bodies are executed up-front, activities collected. After this pass, the workflow is built and its structure does not change anymore. It will simply be executed in the run pass. 

This strict mode may make it easier to visualize the workflows and possibly write them.


Running scala code in parallel
------------------------------

If all you want is to use the simple seq/par constructs to define a 'workflow' of asynchronous scala code, you can simply do this:

    def wsp4 =
      seq {
        par {
          seq { countWords (url1) }
          seq { countWords (url2) }
        }
      }
        
This will simply count the words at two different URLs in parallel and join them when they finish. Note that because seq takes a Unit, you cannot pass the word counts back to par... to do that, just use the valueOf builder. The following assumes two functions countWords and sum:

    def wsp4 =
      seq {

        par {
          valueOf { countWords (url1) }
          valueOf { countWords (url2) }
        }

        seq {
          println ( "total words: " + sum($0.asInstanceOf[List[Int]]) )
        }
      }
        
Note that valueOf (x) is a leaf builder and the same as later (a=>x) - it's just a syntax beauty.


DSL techniques
==================

The collection of activities built while thei builders are run, in the definition pass, is done via a DSL technique of a thread-static capturing the embedded activities - not the nicest to implement but produces natural-looking DSL statements.

You can look at WfaCollector, the collector implementation and WfDynSeq an example usage.


Two passes: define and run
========================

Since these are defined as a scala DSL, there are two passes: the first pass that will build the workflow structure and the second pass that actually runs the workflow:

    import razie.wfs._
      def woohoo(app: String)(in: Any): Any = {
        val x = in.toString + "-" + app;
        println ("------------- woohoo " + x);
        x
      }

      def wss7 = seq {
        println ("------------------woohoo start")
        later {
          println ("------------------woohoo build a")
          woohoo ("a") _
        }
        println ("------------------woohoo between")
        later {
          println ("------------------woohoo build b")
          woohoo ("b") _
        }
        println ("------------------woohoo end")
      }

      wss7 run 1   // starts the workflow with 1 as the input value

 
The sequence of the messages clearly shows the two passes:

    ------------------woohoo start
    ------------------woohoo build a
    ------------------woohoo between
    ------------------woohoo build b
    ------------------woohoo end
    ------------- woohoo 1-a
    ------------- woohoo 1-a-b

The other thing to note here is that, at run time, for the collected activities, there is an "invisible" value being passed around. Each activity is a Any => Any.

The way this works is: there are a few workflow acitivy builders: seq, par, wfs.later (or wfs.apply) and wfs.matchLater - these wrap code BUT the wrapped code will not neccessarily become an activity.

We will call the two passes: definition time and run time. The two different bodies of the different activity builders will be:
* the definition body is what you see and it may or may not build activities
* the workflow body is the structure of activities built during the definition pass

