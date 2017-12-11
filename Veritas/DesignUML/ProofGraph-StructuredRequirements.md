Structured Requirements for Proof Graphs (from different perspectives)
===============================================

General requirements (high-level)
-----------------------------

* Proof graphs shall provide a generic infrastructure to structure,
  store, distribute, verify, and check formal proofs. Generic =
  prover-independent, and format-independent (different formats and provers can be used with the infrastructure).



Requirements from perspective of the producer of a proof graph
-----------------------------------------------------

As somebody who creates/programs a proof graph, I want to
* create nodes that represent (unverified) proof steps.
* create links/edges between nodes that represent dependencies between proof
steps.
* indicate how exactly a parent could be proven from its children via
  a verification strategy/tactic (e.g. induction, case distinction,
  disjunction/conjunction..)
* modify a graph (adding/modifiying/deleting
  nodes/edges/subgraphs). (Maybe: Modifying a proof graph should
  affect as few verification stati of the rest of the graph as
  possible (e.g. if a change does not invalidate a previous proof, the
  previous proof and its evidence should be kept). However,
  conversely, the modification should also affect as many stati as
  necessary (i.e. if a modification changes assumptions of an existing
  proof, the verification status has to be invalidated/reset).

* verify one or more proof steps in a graph by calling one or more
external provers. Verification should generate evidence for proofs.

* inspect the current proof graph (content of nodes/edges,
verification status, produced evidence for a verification status).
* query the current proof graph (e.g. "Which steps are unverified?",
  "Which steps require induction?" etc.)

* store the current status of a proof graph (including the verification
status), along with evidence for successful proofs.
* distribute a proof graph and its current state to another person.


As somebody who creates/programs a ProofGraph, it would be nice to
have
* Versioning control: I would like to save different versions of a
  proof graph as I proceed producing/verifying nodes. I would like to
  be able to distribute that version history as well to another
  person.
* visualize the proof graph (e.g. via graphviz) 




Requirements from perspective of receiver of a (existing) Proof Graph
------------------------------------------------------------

As somebody who obtains a (potentially partially verified) proof
graph from a previous proof attempt (from myself or somebody else), I
want to
* inspect the proof graph: which parts have been verified how / which
  parts are unverified?
* check that provided proofs are correct.
* import the graph into the infrastructure so that I can become the
  producer of a proof graph and have all the capabilities listed above.




Technical Requirements
----------------------

* Proof graphs shall in principle be usable with different formats for
specifications and properties.
* Proof graphs shall be prover-independent: It shall be possible to
  use proof graphs with different provers for different parts of the
  graph.
* Verification of proof graphs shall be safely possible in parallel (both
  calling several provers in parallel and verifiying independent parts
  of the graph in parallel).
* Proof graphs should be stored efficiently (redundant information
  should not be stored twice, any expired information should not hang
  around (garbage collection)).
* Proof graphs should enable event handling, so that you can add
  listeners to (part of) a graph, to be notified as soon as something
  changes.
* Proof graphs should be a library in a general-purpose programming
  language (Scala), so that they can easily be used together with other
  existing tools/libraries.
* It should be easy to clone/copy graphs.

