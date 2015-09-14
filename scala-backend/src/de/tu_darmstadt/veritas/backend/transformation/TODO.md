# Transformations TODO

- (DONE) replace FunctionPatVar and FunctionExpVar with a nullary FunctionPatApp/FunctionExpApp if they have the name of a constructor (as in resolve-vars in fof-rewriting.str)
- a generic "JoinModuleDef[T <: ModuleDef]" transformation that collects all module defs of the same type in a single one (basically a generic version of JoinConstructors)
- (DONE) Lemmas to Axioms/Goals
- split Module into Modules with one goal per Module (also with Strategy blocks) (Daniel)
- rewrite function equations to Axioms
- generate function inversion axioms
- (DONE) Core TSSL -> fof transformation cleanup/check with ToTff
- TypingJudgment/TypingJudgmentSimple -> tcheck function Application

# Combinations of Transformations TODO

- (DONE?) write convenience trait/abstract class for combining Module transformations in a
nice way on a module (maybe also inherit from ModuleTransformation)
e.g. trait has a sequence of
ModuleTranformations/ModuleDefTransformations, applies them one after
the other to a Module) - this can be done easily with
VeritasConstructTransformation now
