# Automated Lemma Generation for Soundness Proofs of Type Systems for DSLs

by Friedrich Weber, https://github.com/fredreichbier

This package implements three algorithms for automated lemma generation:

 * Naive is implemented by ``de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive.NaiveLemmaGenerator``
 * Clever is implemented by ``de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.CleverLemmaGenerator``
 * CleverHints is implemented by ``de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.CleverHintsLemmaGenerator``
 
In order to use them, create a ``Problem`` instance from your ScalaSPL specification:

```scala
val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/SQLSpecAnnotated.scala")
val problem = new Problem(file)
val generator = new CleverHintsLemmaGenerator(problem)
val lemmas = generator.generateLemmas()
```

Alternatively, you can write the generated lemmas to an updated ScalaSPL specification as follows:
```scala
val updatedSpec = ScalaSPLSpecificationOutput.updateSpecification(problem, generator)
println(updatedSpec)
```