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

3. d
