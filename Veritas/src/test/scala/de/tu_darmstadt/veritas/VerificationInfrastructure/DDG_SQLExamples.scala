package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.backend.ast.VeritasConstruct
import de.tu_darmstadt.veritas.inputdsl.{DataTypeDSL, FunctionDSL, SymTreeDSL}
import quiver.{LEdge, LNode}


/**
  * Created by sylvia on 27/02/2017.
  */
class DDG_SQLExamples {

  import DataTypeDSL._
  import FunctionDSL._
  import SymTreeDSL._

  import de.tu_darmstadt.veritas.inputdsl.testexamples.SQLDefs._

  // put here again for reference only
  //  val Query = data('Query) of
  //    'tvalue ('Table) |
  //      'selectFromWhere ('Select, 'Name, 'Pred) |
  //      'Union ('Query, 'Query) |
  //      'Intersection ('Query, 'Query) |
  //      'Difference ('Query, 'Query)

  val queryroot = LNode("dtquery",
    ExpressionDomainNode[VeritasConstruct, String](Query, Map(Query.name -> ~'query)))

  val tvaluecons = Query.constrs.head
  val selectFromWherecons = Query.constrs(1)
  val unioncons = Query.constrs(2)
  val intersectioncons = Query.constrs(3)
  val differencecons = Query.constrs(4)

  val tvaluenode = LNode("tvaluecons",
    ExpressionDomainNode[VeritasConstruct, String](tvaluecons,
      Map(tvaluecons.in.head.name -> ~'table)))

  val tvalueedge = LEdge("dtquery", "tvaluecons", DTConstructor[String](Query.name))

  val selectFromWherenode = LNode("selectFromWherecons",
    ExpressionDomainNode[VeritasConstruct, String](selectFromWherecons,
      Map(selectFromWherecons.in.head.name -> ~'select,
        selectFromWherecons.in(1).name -> ~'name,
        selectFromWherecons.in(2).name -> ~'pred)))

  val selectFromWhereedge = LEdge("dtquery", "selectFromWherecons", DTConstructor[String](Query.name))

  val unionnode = LNode("unioncons",
    ExpressionDomainNode[VeritasConstruct, String](unioncons,
      Map(unioncons.in.head.name + "1" -> ~'query1,
        unioncons.in(1).name + "2" -> ~'query2)))

  val unionedge = LEdge("dtquery", "unioncons", DTConstructor[String](Query.name))

  val intersectionnode = LNode("intersectioncons",
    ExpressionDomainNode[VeritasConstruct, String](intersectioncons,
      Map(intersectioncons.in.head.name + "1" -> ~'query1,
        intersectioncons.in(1).name + "2" -> ~'query2)))

  val intersectionedge = LEdge("dtquery", "intersectioncons", DTConstructor[String](Query.name))

  val differencenode = LNode("differencecons",
    ExpressionDomainNode[VeritasConstruct, String](differencecons,
      Map(differencecons.in.head.name + "1" -> ~'query1,
        differencecons.in(1).name + "2" -> ~'query2)))

  val differenceedge = LEdge("dtquery", "differencecons", DTConstructor[String](Query.name))

  val basicDomainGraph = DomainGraph(
    Seq(queryroot, tvaluenode, selectFromWherenode, unionnode, intersectionnode, differencenode),
    Seq(tvalueedge, selectFromWhereedge, unionedge, intersectionedge, differenceedge))


}
