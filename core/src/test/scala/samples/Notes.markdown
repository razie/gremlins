Debugging WFS workflows

1. use strict to see the workflow that comes out

Normally you would do 

    val wflow = seq {
      "start" label noCollect { wf.nop }
    }

But, you can only see what hapenend at run time, which may cause out of memory ot other issues. To debug workflow structure, just make it strict and print it:

    val wflow = wfs strict seq {
      "start" label noCollect { wf.nop }
    }


2. wfs sub-processes must be scoped, otherwise you're in trouble - the engine modifies them

3. It is VERY advisable that, when testing new workflows, use the graph debug to avoid stack overflow and see where the problem is (the stack trace is smaller):

    razie.g.Graphs.maxDebugDepth=50

4. Collecting vs no collecting in par/seq

Important and non-intuitive. Activities created IN ANY WAY inside a seq/par are collected. While this is not a problem with seq, due to them being senquenced, this may be a problem with par.

For instance, this structure will have unexpected result:

    par {
      wf.nop + wf.nop + wf.nop
    }

If you have blocks that create structures of activities, you should wrap them in a special not collecting block:

    par {
      resultOf {
        wf.nop + wf.nop + wf.nop
      }
    }

The noCollect instructs the par to only collect its result but not any activities created INSIDE the block.

Note that the one below is worse: the seq will create an infinite loop since it collects EACH nop and then the sequences and loops back on itself.

    par {
      seq {
        wf.nop + wf.nop + wf.nop
      }
    }


5. Remember, only seq and par can take multiple builders
All other constructs only use either Unit or =>WfActivity so they can take at most one builder inside. 
For instance:

     // this is wrong - only the last activity will be executed, the first will be lost
      actWhile (_ != "done") {
          pingChannel ? w(ping)
          pongChannel ! $0
      }
     // this is good - the sequence collects both activities
      actWhile (_ != "done") {
        seq {
          pingChannel ? w(ping)
          pongChannel ! $0
        }
      }
     // this is good - because the pipes are smart about collecting
      actWhile (_ != "done") {
        pongChannel ! (pingChannel ? w(ping))
      }

