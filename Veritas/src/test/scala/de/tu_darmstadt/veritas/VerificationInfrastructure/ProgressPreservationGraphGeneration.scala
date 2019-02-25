package de.tu_darmstadt.veritas.VerificationInfrastructure


object ProgressPreservationGraphGeneration extends App {


  val aesource = "src/test/scala/de/tu_darmstadt/veritas/scalaspl/AESpec.scala"
  val aestore = "AESoundnessGeneratedProofGraph-store"

  val AEProof = ProgressPreservationProofGraphGeneration(aesource, aestore)

  AEProof.visualizeProofGraph("GeneratedAESoundness_Unverified.png")
  AEProof.verifyAll()
  AEProof.visualizeProofGraph("GeneratedAESoundness_PartialVerification.png")
}
