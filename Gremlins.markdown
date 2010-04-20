
Razie's Pub Workflow Engine
===========================

I've been meaning to write a workflow engine/language for sometime and recently, some scala DSL 
discussions gave me the motive. I know there's tons of workflow products but none complete and to my liking.

Anyways, evolution requires diversity, so enough rationalization (there's more at the bottom), let's 
dive into the fun stuff!


Razie's Workflow Vision: Gremlins
=================================

When I think "workflow" I imagine a graph of activities of different kinds, through which a processing
engine carves a path. As opposed to just being there for fun, these nodes actually do stuff, so they look 
much like a bunch of (more or less chaotic) Gremlins.

Some activities carry out actual processing or communication while others merely figure out what path to
follow next and yet others are there just for structure. Some complete immediately while some take their 
time. Some are on the same sequence while some are completed in parallel and on different engines/servers.

The Gremlins image is more obvious if you start to think outside the box. Literally, Gremlins can go into 
other systems and applications and do stuff. This may sound non-sensical but for any B2B developer it
sounds right: tracking/coordinating sub-processes in other parties - you'd *love* to get visibility and 
control into those, wouldn't you? You could cascade a suspend or roll-up some intermediary states...

The basic processing engine is thus nothing more than a graph traversal / visitor. However - forget the 
visitor pattern, these nodes are object-oriented and encapsulate their own logic.

Common patterns are expressed in terms of the basic constructs, to create new, higher-level logic and 
structures.

One difference from other frameworks will be that of dissolving the barriers between concurrent programming,
distribution and workflows. I see all preceeding techniques as steps, identifying patterns and leading us 
here. 
The only programming model worth learning is the workflow...the others are good maybe for a history class.

So, what we need is:
- generic graph-based processing engine
Classes like <code>WA</code> and <code>WL</code> describe this level of the workflow.
- engine cooperation API (so remote engines can coordinate sub-workflows)
- graph template-ing engine
- basic activity library
- default/standard embedable container

That's it! Sounds simple? As any other highly abstract concept, once explained it is immediately 
obvious... especially to me: I spent soo much time thinking about this that once typed, 
I'm now wondering why on earth it took me so long.

   
Context, environment
--------------------

All nodes in a workflow share the same context. Each node can modify the common context.


Actions and Actionables
-----------------------

This is the basic concept of an action/actionable. All we do is a set of steps/actions on different objects
in an environment. 
Whether, being a developer, you're thinking about <code>console.logging(a message)</code> or sending a 
message to an external system, making a WS call etc


Concurrency
-----------

Concurrent programming takes into account that most of the time, the same resource is shared for different 
purposes, often at the same time. For instance, fighter jet computers tracking multiple bogies or amazon's
ordering system handling multiple carts depleting the same book shelves, not to mention Google's robots 
reading a gazillion pages from a gazillion servers with different response times.

Things get pretty complicated very quickly when trying to express this concurrency.

The "standard" approaches are:
- shared state
The classic model where processing threads share some state (like list of users). All kinds of constructs
are used to manage concurrent access to state/resources. A monitor could wrap the logging system and lock 
it for the duration of different calls.
- message passing
Independent stateful or stateless "actors" that can respond to messages. All state is encapsulated in the messages queued for 
all actors at any one time... 
An actor could wrap the logging system and process log requests as they arrive as messages. If there would
be any result, it would send that as a message to another actor.

While the first does have some visualization options, the second can at best produce an incoherent 
abstract image in my head...although one can appreciate the late-binding flexibility of an actor system - 
their communication paths could be re-wired at runtime.


How does our workflow accomodate concurrency? 
Well, for one, there's multiple worflows than run at the same time. 
How many, for whom and what they do is unknown and as irrelevant as the number of threads.
Inside the same workflow, a few execution threads can exist at the same time. Simply activating more links 
from any node will spawn a few threads and when these meet in special "join" nodes, the threads merge 
again.

Concurrent programming will still be relevant for low-level programming. If you have shared resources, 
you have the option of using any concurrent technique you like:
- in the classic model you would use locks and other such contraptions - I recommend you use distributed 
locks not just local, since workflows can migrate all over the place...
We will provide some simple implementations
- in the actor model, you would simply use long-running actor workflows, which just listen for incoming 
messages and process them

While the shared context makes this resemble the classic concurrency model, message passing is also possible
between wofklows or between sub-workflows and especially between workflows and the outside environment.

However, the workflow enforces a shift from thinking in semaphores to thinking in transactions. 
TODO will need to elaborate on that.


### Split/join

NOTE there are two options I'm considering:
1. special "split" node that will spawn threads and all other nodes can only activate 1 link at a time
1. any node could spawn multiple links, automatically trigerring thread spawns

Since we have to use a special "join" node to merge threads, there is a symetrical beauty to having a 
"split"...


Distribution
------------

Distributed programming deals with the distribution of resources and programs all over a network. The major
difference is that communication is no longer guaranteed and response times vary.

The main concept to master at this point is "asynchronous". The actor model described above naturally 
resembles a distributed system and is very apt at being distributed, because of the embedded asynchronicity.

Asynchronous communication implies extremely late binding, flexibility and less depdencies between the
interacting parties.

TODO document how we're addressing distribution:
- remote gremlins
- gremlin API
- message passing primitives
- sync/async or async/sync bridges


Workflow
--------


Requirements
============

The following are issues normally addressed


Design
======

There are a few layers and components here:

- simple graph structures
- workflow add-on
- actions (actionables)
- engine

The first layer is the basic graph/link structures. These are inherited from <pre>razie.g.{GNode,GLink}</pre>.


How Gremlins take over the world
=======================================
   
If any software component out there starts to implement the GremlinsAPI for sub-process management.


Wait, there's more Gremlins
===========================

I noticed there's at least another project on github called "Gremlin" and interestingly, it deals with 
graphs as well...it is probable that we both have the same vivid imagination :)

I've always thought of this distributed workflow as a bunch of gremlins, chewing different problems up all
over the place and I don't want to change it just because of a name, so, as hard as it may turn out to be, 
I'll keep this name, invoking the power of visualization.
   

Why?
====

I know there's tons of workflow products but none complete, to my liking.

While different functionalities like Web Services Choreography can be grafted onto existing frameworks 
like adding libraries to Java, I feel that a new framework of concepts and constructs is needed, to 
become the fabric for all the new constructs. 
TODO reword this.

While the BPM guys claim "we've got it" they're right. But so are the BPEL boys and the XPDL gals and we're 
left to pick up the pieces in all and any language. There is no low-level workflow that we can use and 
all we can do is use all kinds of graphic designers, design all kinds of XML representation and interact 
with 3rd party engines which require yet more integration to interact with 3rd party systems.

There is an inner beauty to the simplicity of Lisp, where everything is a function...or Smalltalk where
everything is an object (except turtles, you see).

That's where Scala comes in: as a natively scalable language, it gives us the chance to setup this gremlin 
fabric where we can write both small (1 liner) as well as big (inter-enterprise) workflows...and focus on 
learning a small set of patterns and use a lot of built-in functionality that the fabric has to offer.


