# README for Veritas ###

## Short Description ##

Veritas is a (still very young) prototype for (semi-)automatically proving type soundness using first-order theorem proving as support. It currently allows for specifying the syntax and semantics of very simple programming languages (such as the simply-typed lambda calculus (STLC)), type systems for this language, and proof goals and axioms. Users can prove type soundness by manually breaking down relevant lemmas and theorems into individual induction cases. Veritas transforms the goals, axioms, and definitions automatically into TPTP syntax and calls the automatic  first-order theorem prover Vampire with the resulting fof-files. To get familiar with TPTP syntax and Vampire we recommend:

"First-Order Theorem Proving and VAMPIRE" by Laura Kovács and Andrei Voronkov

##NOTE: Restructuring Branch##

Note that on this branch (restructuring), we started restructuring Veritas to a stand-alone SBT library which one can use independent of Spoofax. However, currently, the former Spoofax part of the project can still be used with the Veritas SBT project like previously. This README describes how to get the Spoofax part running with the new setup.

## How to install VeritasSpoofax ##

You need:

1. Java 7 or higher
2. Scala IDE (http://scala-ide.org/download/sdk.html). It includes Scala 2.11.
3. Installation of SBT
4. First-Order Theorem Prover Vampire, Version 3.0 or 4.0 (or both). Official downloads of Vampire are currently disabled - you may obtain Vampire binaries by either contacting the authors of Vampire or by browsing the CASC webpages for the system sources: http://www.cs.miami.edu/~tptp/CASC/24/ (Vampire 3.0) or http://www.cs.miami.edu/~tptp/CASC/25/ (Vampire 4.0) and using the Systems' sources and executables link. (Note: The compressed directories are several GB!)

Installation steps:

0. Check out both the Veritas and the VeritasSpoofax folder somewhere in the same folder.
1. In the Veritas folder, run "sbt assembly" to produce a .jar of the Veritas project (required by Veritas Spoofax).
2. Install the latest version of the Spoofax Language Workbench (http://strategoxt.org/Spoofax) as Eclipse plugin in the Scala IDE: In the Scala IDE, choose "Help - Install New Software". Copy the following link to the latest stable Spoofax build: http://download.spoofax.org/update/stable , hit enter, mark everything, and install. (Note: The nightly Spoofax versions http://download.spoofax.org/update/nightly do not work) Restart Eclipse and adapt the eclipse.ini according to the warning you might get during the start-up. After modifying the eclipse.ini, restart Eclipse again.
3. Copy your Vampire binaries into the VeritasSpoofax folder. The binaries have to be named vampire-3.0 and vampire-4.0 according to the version of the sources the binaries were compiled from.
4. Build the VeritasSpoofax project in Eclipse (make sure the build is successful before continuing with further steps). The build should automatically copy the Veritas.jar from the first step to the include folder in VeritasSpoofax.


## Running the STLC case study ##

The STLC case study is in folder test/stlc.

1. Open a .stl file with a proof goal
2. Put the cursor inside the .stl file. In the top bar of Eclipse, there should appear several drop-down menus such as "Analysis", "Generation", "Verification".
3. Click "Verification - Verify with Vampire" for running a proof using Vampire. Choose a timeout from the menu entries as you like. The symbols next to each goal in the editor indicate whether the proof was successful or not.

## Some troubleshooting advice ##

* The VeritasSpoofax will probably report some errors, especially when trying to run a proof. Errors like "missing Java project 'Veritas'" oder "incompatible .jar strategoxt.jar" can be safely ignored.

* Vampire calls appear in the console when running "Verify with Vampire", but no time passes between the calls and the proofs don't work: Vampire is not really executable and probably not inside the PATH used by Eclipse. Re-check step 2 of the installation steps.

* The proofs are somehow not working and also the .stl files of the STLC case study throw parse errors in Eclipse: This behavior may occur sometimes due to a Spoofax bug that could not be resolved yet. For the proofs to work as expected, it is important that all the .stl files relevant for a particular project do not throw parse errors. For the moment, you can reach this state by opening the .stl files in their "import-order" and waiting after each file that the file is processed without errors. For the STLC case study, this order can for example be:
  1. Syntax.stl
  2. Context.stl
  3. Gensym.stl
  4. Subst.stl
  5. AlphaEquivalence.stl
  6. Types.stl
  7. SubstLemmaAux.stl
  8. SubstLemma.stl
  9. choose either call-by-name oder call-by-value folder: Reduction.stl
  10. Preservation.stl or Progress.stl in the folder chosen in 9.
  If there are still parse errors after opening the files in this order, close everything again, restart Eclipse, and try again.

* Everything seems to work (notably, the issues described before can be ruled out) but when I click "Verify with Vampire" in the STLC case study, the program runs for some time and then it is reported that no proof could be found for some goal: Currently, in the STLC case study, all the proofs should work. Clean the proof status of the .stl file by clicking "Verification - Clean" and try again with a higher timeout for Vampire (e.g. "Verify with Vampire (60)").
