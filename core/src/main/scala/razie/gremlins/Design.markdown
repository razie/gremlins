
Workflow engine design
======================

As mentioned in the vision, the components are:

* generic graph-based processing engine
Classes like <code>WfAct</code> and <code>WL</code> describe this level of the workflow.
* graph template-ing engine
* basic activity library
* default/standard embedable container
* engine cooperation API (so remote engines can coordinate sub-workflows)
* default agent structure for distribution


GGS (Generic Graph Stuff)
-------------------------

Simple, recursive, graph stuff is in the [razie.g](http://github.com/razie/razbase/tree/master/base/src/main/scala/razie/g) package, in the razbase project. there's also a JWFunc for java friendly activities.
As much generic graph manipulation stuff goes there. This includes stiching sub-graphs etc.


Activities and stuff
--------------------

Well, each gremlin must do something, so those that do, implement the [WFunc trait](http://github.com/razie/gremlins/tree/master/core/src/main/scala/razie/gremlins/WFunc.scala).
In good scala tradition, each one returns a value (it can also modify the context/state, but it shouldn't,
in the best functional tradition).

The core activities of the engine are defined in [WfBaseActivities](http://github.com/razie/gremlins/tree/master/core/src/main/scala/razie/gremlins/act/WfBaseActivities.scala). You don't normally use these directly, just take a look to see how things are defined. You'll see these deal with basic graph navigation, representing if/else and such.

Some usable activities are defined in [WfBaseLib](http://github.com/razie/gremlins/tree/master/core/src/main/scala/razie/gremlins/act/WfBaseLib.scala). These include stuff like assign to variables, logging etc and are used in most examples. Note these

* $0 and $(name) // access to variables
* script(name,lang,s) // execute a script, lang can be scala or js
* act(unifiedStr) // use razie.actionables, see below


There's a library of activities (like running shell or telnet) in [razie.actionables](http://github.com/razie/gremlins/tree/master/core/src/main/scala/razie/actionables).


Engine
------

Well, the engine traverses the graph and executes the individual nodes. It basically kiks gremlin butt 
into action. The gremlins are all WfAct, which defines the basic traversal logic to implement. 

Those nodes that actually do something implement WFunc as above. Others
are structure or workflow management nodes and just navigate/find the next thing(s) to do.


Templates
---------

In good circus tradition, a gremlin pyramid is a different beast. A template is basically a structure of other, lower-level nodes.

If we find a good base, all and any activity can be grown as templates, and packaged in libraries. 
We'll build the most common in <code>razie.wf.lib</code>. Choose whichever flavor you like.


Resources
---------

All workflows use resources - this WfRes takes care of how the resources interact with the workflows...
naturally asynchronously...


Extensibility (todo)
--------------------

Not all actions can be built from others. Especially those that interact with the environment 
(asynchronous comunication). We want to build extensibility right into the engine and we 
did that via "wf services".

A wf service is just that: a service/plugin that offers some services to workflows, examples being
scheduling, messaging etc.

Do not confuse it with an external business services called by the workflow (like a Web Service from BPEL).
 
These services are special in the sense that they are the only that can suspend an activity waiting 
for a response.


Creating new activities
-------------------

Option 1 - use the act string. This is especially interestgin if you will use DSL to define the workflow, rather than scala workflows. See sample in [razie.actionables](http://github.com/razie/gremlins/tree/master/core/src/main/scala/razie/actionables).

1. define the actual activity code, like ExecX
1. make up a factory that can create them from the unified string, like SampleExecutables
1. in your main, register your factory: <code>Executables.reg("sample", SampleExecutables)</code>



Persistency
-----------

Workflow definition is persited separately from the instance status, with links between them.


