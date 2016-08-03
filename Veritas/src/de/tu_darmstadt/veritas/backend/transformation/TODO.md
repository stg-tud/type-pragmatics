# Transformations TODO

- (DONE) replace FunctionPatVar and FunctionExpVar with a nullary
  FunctionPatApp/FunctionExpApp if they have the name of a constructor
  (as in resolve-vars in fof-rewriting.str) 
- (DONE) Lemmas to Axioms/Goals
- (DONE) split Module into Modules with one goal per Module (also with Strategy blocks) (Daniel)
- (DONE) rewrite function equations to Axioms
- (DONE) generate function inversion axioms
- (DONE) Core TSSL -> fof transformation cleanup/check with ToTff
- (DONE) TypingJudgment/TypingJudgmentSimple -> tcheck function Application,
with generation/inference of function signature

- create and insert type guards + domain axioms

- implement axiom selectors (from Google Doc):
  - no filtering (include everything that is imported) - default
  - no inversion axioms (??)
  - include ALL axioms that are (recursively) “reachable” from the conjecture (e.g. EQ and DIFF axioms of all constructors used in the goal, axioms generated for functions used in the goal, and then recursively include everything that is used in the included axioms) - corresponds to Vampire Sine option?
  - different “reachability” depths: like previous selector, but only allow a search depth of up to N for functions axioms (but always include all constructor axioms)
  - pre-select function equations that are likely to be used and discard the others



# Combinations of Transformations TODO

- (DONE) write convenience trait/abstract class for combining Module transformations in a
nice way on a module (maybe also inherit from ModuleTransformation)
e.g. trait has a sequence of
ModuleTranformations/ModuleDefTransformations, applies them one after
the other to a Module)
