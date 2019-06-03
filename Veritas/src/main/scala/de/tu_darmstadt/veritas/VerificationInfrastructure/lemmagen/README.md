# Automated Lemma Generation for Soundness Proofs of Type Systems for DSLs

by Friedrich Weber, https://github.com/fredreichbier

This package implements three algorithms for automated lemma generation:

 * Naive is implemented by ``de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive.NaiveLemmaGenerator``
 * Clever is implemented by ``de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.CleverLemmaGenerator``
 * CleverHints is implemented by ``de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.CleverHintsLemmaGenerator``
 
 
First add suitable ``@Static``, ``@Dynamic`` and ``@Preservable`` annotations to your ScalaSPL specification.
Create a ``Problem`` instance from your specification:

```scala
val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/SQLSpecAnnotated.scala")
val problem = new Problem(file)
```

Now, instantiate a lemma generator and generate a map of specification functions to generated lemmas:
```scala
val generator = new CleverHintsLemmaGenerator(problem)
val lemmas = generator.generateLemmas()
```

Alternatively, you can generate lemmas and produce an updated ScalaSPL specification as follows:
```scala
val updatedSpec = generator.generateAndUpdateSpecification()
println(updatedSpec)
```