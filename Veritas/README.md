# README for Veritas ###

## Short Description ##

Veritas is a (still very young) prototype for (semi-)automatically proving type soundness using first-order theorem proving as support. It currently allows for specifying the syntax and semantics of very simple programming languages (such as the simply-typed lambda calculus (STLC)), type systems for this language, and proof goals and axioms, via using AST classes. The current version includes a compiler family which allows for translating such language specifications and proof goals on it into TPTP, using different compilation strategies (see "Exploration of Language Specifications by Compilation to First-Order Logic" by Sylvia Grewe, Sebastian Erdweg, Michael Raulf, and Mira Mezini. To appear in Principles and Practice of Declarative Programming (PPDP). ACM, 2016). 

##NOTE: Restructuring Branch##

Note that on this branch (restructuring), we started restructuring Veritas to a stand-alone SBT library which one can use independent of Spoofax, on which the previous structure heavily relies. Currently, the Veritas project can already be used independent of Spoofax - but using it is not very comfortable (specifying languages by using AST classes, calling Vampire manually on the resulting files etc.). We are currently working on improving the usability of the standalone SBT library.

Meanwhile, the former Spoofax part of Veritas remains functional and provides some better usability (see folder VeritasSpoofax for setup instructions).


## How to install Veritas (standalone) ##

You need:

1. Java 7 or higher
2. Installation of SBT and Scala
3. (optional) your favorite IDE for Scala
4. (optional) your favorite automated theorem prover that understands TPTP, if you would like to prove something, e.g. Vampire (This is currently not required for setting up the Veritas SBT part, but will probably soon be.)

Some optional steps to try with the project:
1. Build the project using SBT or your favorite IDE.
2. Run "sbt assembly" to produce a standalone .jar of the project (currently not particularly useful in itself, only when using Veritas with VeritasSpoofax).
3. Run the tests in test/scala, via your IDE or via "sbt test" (not all of them might work).
4. Using for example Scala REPL (or Ammonite, a more advanced REPL), create a language specification using the case classes from the Veritas package, as in some of the tests in test/scala. Your top-level-node should be a "Module". On your Module, you can call transformation steps (see package transformation). For example, you can pass your module to the "MainTrans" object together with a configuration that specifies the compilation strategy. This transformation will apply the necessary basic transformation steps to transform a Veritas module into one or more core modules. On the core module(s), you can apply TypingTrans.finalEncoding to transform the core module into a TPTP file, which you can then pretty-print. See case class EncodingComparison for the first basic steps, and Backend.scala, writeFile for how to pretty-print.
