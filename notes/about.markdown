[Razie's Gremlins](http://github.com/razie/gremlins) is a scala DSL and Workflow engine for asynchronous and parallel programming.

Why? Threads, shared state, actors, processes...they're all stepping stones. A framework that allows full expressivity for creating asynchronous/parallel and distributed work units are what we need.

Examples:
---------

Here's a sample CSP-style parallel process

    v(c) (c ? P | c ! Q) 

Sample parallel/sequence

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

