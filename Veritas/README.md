# README for Veritas ###

## Short Description ##

Veritas is a (still very young) prototype for (semi-)automatically proving type soundness using first-order theorem proving as support. It currently allows for specifying the syntax and semantics of simple typed DSL, and proof goals and axioms, via using AST classes or a subset of Scala. The current version includes a compiler family which allows for translating such language specifications and proof goals on it into TPTP, using different compilation strategies (see "Exploration of Language Specifications by Compilation to First-Order Logic" by Sylvia Grewe, Sebastian Erdweg, Michael Raulf, and Mira Mezini. In: Principles and Practice of Declarative Programming (PPDP). ACM, 2016).
The current version also allows for structuring proofs via proof graphs (see "System Description: An Infrastructure for Combining Domain Knowledge with Automated Theorem Provers." by Sylvia Grewe, Sebastian Erdweg, André Pacak, and Mira Mezini. In : Proceedings of International Symposium on Principles and Practice of Declarative Programming (PPDP). 2018).

## How to install Veritas (standalone) ##

You need:

1. Java 8
2. Installation of SBT (0.13) and Scala (2.12.8)
3. (optional) your favorite IDE for Scala - recommended: JetBrains IntelliJ, with Scala plugin
4. (optional) your favorite automated theorem prover that understands TPTP, if you would like to prove something. Current examples and tests mostly use Vampire 4.1 / 4.0, which you need to run these examples. Binaries can be found here: https://vprover.github.io/download.html and here: http://www.cse.chalmers.se/~simrob/tools.html
The binary should be accessible via your PATH.


Some first steps to try with the current version of the project:

1. Build the project using SBT or your favorite IDE or by running "sbt compile" in the project folder. This will produce a couple of warnings, but should not produce any errors. You may attempt to run some of the tests in the test folder, but not all of them will always be working (some tests test certain proof states rather than code functionality, which may vary depending on the prover binaries).
2. Create your own language specification using a subset of Scala. Examples to look at are "test/scala/scalaspl/AESpec.scala", "test/scala/scalaspl/SQLSpec.scala", and "test/scala/scalaspl/QLSpec.scala".
3. Use the proof graph API to manually construct a proof graph and to trigger verification of proof steps via external provers for proving properties of your specification (may require installing more external provers). (Executable) example file that shows how one can do this: "test/scala/VerificationInfrastructure/AESoundnessProofGraph.scala"
Executing this file will generate a .png file in the Veritas folder that visualizes the proof graph with the (intermediate) verification state.

