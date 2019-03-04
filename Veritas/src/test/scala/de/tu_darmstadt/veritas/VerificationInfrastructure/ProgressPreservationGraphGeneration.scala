package de.tu_darmstadt.veritas.VerificationInfrastructure


object ProgressPreservationGraphGeneration extends App {


  val aesource = "src/test/scala/de/tu_darmstadt/veritas/scalaspl/AESpec.scala"
  val aestore = "AESoundnessGeneratedProofGraph-store"

  val AEProof = ProgressPreservationProofGraphGeneration(aesource, aestore)

  AEProof.visualizeProofGraph("GeneratedAESoundness_Unverified.png")
  //AEProof.checkConsistencyTopLevelProblems() //should run for 10 min
  //AEProof.checkConsistencyAll() //might run for a really long time!!
  //AEProof.verifyAll(true, 30)
  //AEProof.visualizeProofGraph("GeneratedAESoundness_PartialVerification.png")

  val sqlsource = "src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala"
  val sqlstore = "SQLSoundnessGeneratedProofGraph-store"

  val SQLProof = ProgressPreservationProofGraphGeneration(sqlsource, sqlstore)

  SQLProof.visualizeProofGraph("GeneratedSQLSoundness_Unverified.png")
  //SQLProof.checkConsistencyAll()
  SQLProof.verifyAll(true, 120)
  SQLProof.visualizeProofGraph("GeneratedSQLSoundness_PartialVerification.png")

  //val qlsource = "src/test/scala/de/tu_darmstadt/veritas/scalaspl/QLSpec.scala"
  //val qlstore = "QLSoundnessGeneratedProofGraph-store"

  //val QLProof = ProgressPreservationProofGraphGeneration(qlsource, qlstore)

  //QLProof.visualizeProofGraph("GeneratedQLSoundness_Unverified.png")
  //QLProof.verifyAll()
  //QLProof.visualizeProofGraph("GeneratedQLSoundness_PartialVerification.png")
}
