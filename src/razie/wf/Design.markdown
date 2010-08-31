
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

Simple, recursive, graph stuff is in the <code>razie.g</code> package, in the razbase project. 
As much generic graph manipulation stuff goes there. This includes stiching sub-graphs etc.


Functionality stuff
-------------------

Well, each gremlin must do something, so those that do, implement the WFunc trait.
In good scala tradition, each one returns a value (it can also modify the context/state, but it shouldn't,
in the best functional tradition).


Engine
------

Well, the engine traverses the graph and executes the individual nodes. It basically kiks gremlin butt 
into action. The gremlins are all WfAct, which defines the basic traversal logic to implement. 

Those nodes that actually do something implement WFunc as above. Others
are structure or workflow management nodes and just navigate/find the next thing(s) to do.


Templates
---------

In good circus tradition, a gremlin pyramid is a different beast.

A template is basically a structure of other, lower-level nodes.

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


