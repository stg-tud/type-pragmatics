Veritas TODOs
==================

(from 07.03.17)
* implement changes in prover/transformer interfaces (should take problem+spec as arguments of methods, not as fields)
* rename VerificationStatus/Outdated/....
* implement concrete provers/transformers from existing code in benchmark/backend (André?)
* design graph as database with snapshot isolation using a library such as Xodus?
  * document requirements on proof graphs (DONE)
  * design updated proof graph interface (graph queries? listener registration..? ...)
* rethink interaction of high-level proof strategy and VerificationStrategy (tactic): Who can/should create ProofStep children?
* update UML diagram ?
* experiment with persistence: Does Xodus let us persist proof graphs
(and load them again?)
* experiment with version histories & Xodus
