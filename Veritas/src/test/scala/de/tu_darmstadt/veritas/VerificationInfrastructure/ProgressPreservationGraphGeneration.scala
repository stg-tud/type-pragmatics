package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.VerificationInfrastructure.strategies.ProgressPreservationTopLevelStrategy

object ProgressPreservationGraphGeneration extends App {
  val aesource = "src/test/scala/de/tu_darmstadt/veritas/scalaspl/AESpec.scala"
  val aestore =  "AESoundnessGeneratedProofGraph-store"

  val ppstrat = new ProgressPreservationTopLevelStrategy(aesource, aestore)

  val pg_gen = ppstrat.generateGraph()

  ppstrat.visualizeGraph("GeneratedAESoundness.png")

  ppstrat.printAllObligations()
  //ppstrat.printGoalWithName("Preservation")

//  val qlsource = "src/test/scala/de/tu_darmstadt/veritas/scalaspl/QLSpec.scala"
//  val qlstore =  "QLSoundnessGeneratedProofGraph-store"
//
//  val ppstrat_ql = new ProgressPreservationTopLevelStrategy(qlsource, qlstore)
//
//  val pg_gen_gl = ppstrat_ql.generateGraph()
//
//  ppstrat_ql.visualizeGraph("GeneratedQLSoundness.png")
//
//  ppstrat_ql.printAllObligations()

}
