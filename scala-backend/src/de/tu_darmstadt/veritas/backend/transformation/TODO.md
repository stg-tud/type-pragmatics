# Transformations TODO

- replace FunctionPatVar and FunctionExpVar with a nullary FunctionPatApp/FunctionExpApp if they have the name of a constructor (as in resolve-vars in fof-rewriting.str)
- a generic "JoinModuleDef[T <: ModuleDef]" transformation that collects all module defs of the same type in a single one (basically a generic version of JoinConstructors)
- Lemmas to Axioms/Goals
- split Module into Modules with one goal per Module (also with Strategy blocks)
- rewrite function equations to Axioms
- generate function inversion axioms
