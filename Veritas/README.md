# README for Veritas ###

## Short Description ##

Veritas is a (still very young) prototype for (semi-)automatically proving type soundness using first-order theorem proving as support. It currently allows for specifying the syntax and semantics of very simple programming languages (such as the simply-typed lambda calculus (STLC)), type systems for this language, and proof goals and axioms. Users can prove type soundness by manually breaking down relevant lemmas and theorems into individual induction cases. Veritas transforms the goals, axioms, and definitions automatically into TPTP syntax and calls the automatic  first-order theorem prover Vampire with the resulting fof-files. To get familiar with TPTP syntax and Vampire we recommend:

"First-Order Theorem Proving and VAMPIRE" by Laura Kovács and Andrei Voronkov

## How to install Veritas ##

You need:

1. Java 7 or higher
2. Eclipse Luna ("Eclipse IDE for Java Developers")
3. First-Order Theorem Prover Vampire, Version 3.0 (Note: The binaries are currently not available on the official webpage - here is one link to an archive from CASC that contains the Vampire binaries we currently use: http://www.cs.miami.edu/~tptp/CASC/24/Systems.tgz (entire archive ca. 3.2 GB!))

(Note: To run a first test with the Vampire run "vampire" on a console - should show "User error: Unsupported mode")


Installation steps:

1. Install latest version of the Spoofax Language Workbench (http://strategoxt.org/Spoofax) as Eclipse plugin: In Eclipse, choose "Help - Install New Software". Copy the following link to the nightly Spoofax build: http://download.spoofax.org/update/nightly , hit enter, mark everything, and install. (Note: The latest stable Spoofax version http://download.spoofax.org/update/stable should also work, but there may be more issues when running Veritas.) Restart Eclipse and adapt the eclipse.ini according to the warning you might get during the start-up. After modifying the eclipse.ini, restart Eclipse again.
2. Make sure the Vampire binary is inside the PATH used by Eclipse. For example, create a symbolic link from /usr/bin to the Vampire binary.
3. Clone the Veritas folder on your desktop and import the Veritas project in your Eclipse with Spoofax.
4. Build the Veritas project in Eclipse (make sure the build is successful before continuing with further steps).


## Running the STLC case study ##

The STLC case study is in folder test/stlc.

1. Open a .stl file with a proof goal
2. Put the cursor inside the .stl file. In the top bar of Eclipse, there should appear several drop-down menus such as "Analysis", "Generation", "Verification".
3. Click "Verification - Verify with Vampire" for running a proof using Vampire (with timeout 30 seconds). The symbols next to each goal in the editor indicate whether the proof was successful or not.

## Some troubleshooting advice ##

* Vampire calls appear in the console when running "Verify with Vampire", but no time passes between the calls and the proofs don't work: Vampire is not really executable and probably not inside the PATH used by Eclipse. Re-check step 2 of the installation steps.

* The proofs are somehow not working and also the .stl files of the STLC case study throw parse errors in Eclipse: This behavior may occur sometimes due to a bug that we could not resolve yet. For the proofs to work as expected, it is important that all the .stl files relevant for a particular project do not throw parse errors. For the moment, you can reach this state by opening the .stl files in their "import-order" and waiting after each file that the file is processed without errors. For the STLC case study, this order can for example be:
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
