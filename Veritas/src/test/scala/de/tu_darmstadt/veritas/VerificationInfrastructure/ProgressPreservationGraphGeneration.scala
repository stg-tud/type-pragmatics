package de.tu_darmstadt.veritas.VerificationInfrastructure


object ProgressPreservationGraphGeneration extends App {


  val aesource = "src/test/scala/de/tu_darmstadt/veritas/scalaspl/AESpec.scala"
  val aestore = "AESoundnessGeneratedProofGraph-store"

  val AEProof = ProgressPreservationProofGraphGeneration(aesource, aestore)

  //AEProof.visualizeProofGraph("GeneratedAESoundness_Unverified.png")
  //AEProof.checkConsistencyTopLevelProblems() //should run for 10 min
  //AEProof.checkConsistencyAll() //might run for a really long time!!
  //AEProof.verifyAll(120, true, true)
  //AEProof.visualizeProofGraph("GeneratedAESoundness_PartialVerification.png")

  val sqlsource = "src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala"
  val sqlstore = "SQLSoundnessGeneratedProofGraph-store"

  //val SQLProof = ProgressPreservationProofGraphGeneration(sqlsource, sqlstore)

  //SQLProof.visualizeProofGraph("GeneratedSQLSoundness_Unverified.png")
  //SQLProof.checkConsistencyAll()
  //SQLProof.verifyAll(150, true, true)
  //SQLProof.visualizeProofGraph("GeneratedSQLSoundness_PartialVerification.png")

  val qlsource = "src/test/scala/de/tu_darmstadt/veritas/scalaspl/QLSpec.scala"
  val qlstore = "QLSoundnessGeneratedProofGraph-store"

  val QLProof = ProgressPreservationProofGraphGeneration(qlsource, qlstore)

  QLProof.visualizeProofGraph("GeneratedQLSoundness_Unverified.png")
  //QLProof.checkConsistencyTopLevelProblems()
  QLProof.verifyAll(1, true, true)
  QLProof.visualizeProofGraph("GeneratedQLSoundness_PartialVerification.png")
}
