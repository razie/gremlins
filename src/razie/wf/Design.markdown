
Workflow engine design
======================

As mentioned in the vision, the components are:

* generic graph-based processing engine
Classes like <code>WA</code> and <code>WL</code> describe this level of the workflow.
* engine cooperation API (so remote engines can coordinate sub-workflows)
* graph template-ing engine
* basic activity library
* default/standard embedable container
* default agent structure for distribution


GGS (Generic Graph Stuff)
-------------------------

Simple, recursive, graph stuff is in the <code>razie.g</code> package. As much generic graph manipulation
stuff goes there. This includes stiching sub-graphs etc.


Functionality stuff
-------------------

Well, each gremlin must do something, so those that do, implement the WFunc trait.
In good scala tradition, each one returns a value (it can also modify the context/state, but it shouldn't,
in the best functional tradition).


Engine
------

Well, the engine traverses the graph and executes the individual nodes. It basically kiks gremlin butt 
into action.


Templates
---------

In good circus tradition, a gremlin pyramid is a different beast.

Besides a basic simple workflow implementation, some others can be grown as templates, in 
<code>razie.wf.lib</code>. Choose whichever flavor you like.

A template is basically a structure of other, lower-level nodes.


