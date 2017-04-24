package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * UI for proof graph ->
  * more comfortable access to subobligation (e.g. via names)
  * traversals
  *
  * @param goaldesc extracting a descriptive string from any Goal (e.g. with VeritasConstruct:
  *                 goal name, lemma name)
  */
class ProofGraphUI[Spec, Goal](val pg: ProofGraph[Spec, Goal],
                              val goaldesc: Goal => String) {

  protected val obligationMap: Map[pg.Obligation, String] = Map()
  //TODO construct map using goaldesc

  def getObl(name: String): pg.Obligation = ???


  //TODO traversals map/filter/fold


}
