package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

trait DomainSpecificKnowledge[Type, FDef, Prop] {
  def failableTypes: Seq[Type]
  def recursiveFunctions: Map[FDef, (Type, Seq[Int])] //Second part of pair Seq[Int] contains the position at which the function is marked recursive
  def progressProperties: Map[FDef, Set[Prop]]
  def preservationProperties: Map[FDef, Set[Prop]]

  def properties: Set[Prop]
  def staticFunctions: Set[FDef]
  def dynamicFunctions: Set[FDef]

  def lookupByFunName[T](mp: Map[FDef, Set[T]], funname: String): Iterable[T] = {
    val allkeys: Iterable[FDef] = mp.keys.filter ((fd: FDef) => retrieveFunName(fd) == funname)
    allkeys.flatMap(mp)
  }

  def lookupByFunName(fs: Set[FDef], funname: String): Option[FDef] = {
    fs find (fd => retrieveFunName(fd) == funname)
  }

  def lookupTRByName(ts: Set[Prop], trname: String): Option[Prop] = {
    ts find (tr => retrievePropName(tr) == trname)
  }

  def retrieveFunName(fd: FDef): String
  def retrievePropName(p: Prop): String
}
