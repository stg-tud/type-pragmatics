package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.backend.ast.{MetaVar, VeritasConstruct}
import de.tu_darmstadt.veritas.inputdsl.{DataTypeDSL, FunctionDSL, SymTreeDSL}
import org.scalatest.FunSuite
import quiver.{LEdge, LNode}


/**
  * Created by sylvia on 27/02/2017.
  */
//class DDG_SQLExamples extends FunSuite {
//
//  import DataTypeDSL._
//  import FunctionDSL._
//  import SymTreeDSL._
//
//  import de.tu_darmstadt.veritas.inputdsl.SQLDefs._
//
//  // put here again for reference only
//  //  val Query = data('Query) of
//  //    'tvalue ('Table) |
//  //      'selectFromWhere ('Select, 'Name, 'Pred) |
//  //      'Union ('Query, 'Query) |
//  //      'Intersection ('Query, 'Query) |
//  //      'Difference ('Query, 'Query)
//
//  val queryroot = LNode("Data type query",
//    ExpressionDomainNode[VeritasConstruct, String](Query, Map(Query.name -> MetaVar("query"))))
//
//  val tvaluecons = Query.constrs.head
//  val selectFromWherecons = Query.constrs(1)
//  val unioncons = Query.constrs(2)
//  val intersectioncons = Query.constrs(3)
//  val differencecons = Query.constrs(4)
//
//  val tvaluenode = LNode("tvaluecons",
//    ExpressionDomainNode[VeritasConstruct, String](tvaluecons,
//      Map(tvaluecons.in.head.name -> MetaVar("table"))))
//
//  val tvalueedge: LEdge[String, ExpressionDomainEdgeLabel] =
//    LEdge(queryroot.vertex, tvaluenode.vertex, DTConstructor[String](Query.name))
//
//  val selectFromWherenode = LNode("selectFromWherecons",
//    ExpressionDomainNode[VeritasConstruct, String](selectFromWherecons,
//      Map(selectFromWherecons.in.head.name -> MetaVar("select"),
//        selectFromWherecons.in(1).name -> MetaVar("name"),
//        selectFromWherecons.in(2).name -> MetaVar("pred"))))
//
//  val selectFromWhereedge: LEdge[String, ExpressionDomainEdgeLabel] =
//    LEdge(queryroot.vertex, tvaluenode.vertex, DTConstructor[String](Query.name))
//
//  val unionnode = LNode("unioncons",
//    ExpressionDomainNode[VeritasConstruct, String](unioncons,
//      Map(unioncons.in.head.name + "1" -> MetaVar("query1"),
//        unioncons.in(1).name + "2" -> MetaVar("query2"))))
//
//  val unionedge: LEdge[String, ExpressionDomainEdgeLabel] =
//    LEdge(queryroot.vertex, tvaluenode.vertex, DTConstructor[String](Query.name))
//
//  val intersectionnode = LNode("intersectioncons",
//    ExpressionDomainNode[VeritasConstruct, String](intersectioncons,
//      Map(intersectioncons.in.head.name + "1" -> MetaVar("query1"),
//        intersectioncons.in(1).name + "2" -> MetaVar("query2"))))
//
//  val intersectionedge: LEdge[String, ExpressionDomainEdgeLabel] =
//    LEdge(queryroot.vertex, tvaluenode.vertex, DTConstructor[String](Query.name))
//
//  val differencenode = LNode("differencecons",
//    ExpressionDomainNode[VeritasConstruct, String](differencecons,
//      Map(differencecons.in.head.name + "1" -> MetaVar("query1"),
//        differencecons.in(1).name + "2" -> MetaVar("query2"))))
//
//  val differenceedge: LEdge[String, ExpressionDomainEdgeLabel] =
//    LEdge(queryroot.vertex, tvaluenode.vertex, DTConstructor[String](Query.name))
//
//  val basicDomainGraph =
//    DomainGraph[VeritasConstruct, String, ExpressionDomainNode[VeritasConstruct, String], ExpressionDomainEdgeLabel](
//    Seq(queryroot, tvaluenode, selectFromWherenode, unionnode, intersectionnode, differencenode),
//    Seq(tvalueedge, selectFromWhereedge, unionedge, intersectionedge, differenceedge))
//
//
//  test("Print basic graph") {
//    println("Basic Domain Graph:")
//    println(basicDomainGraph.getInternalGraph.toString())
//    println("")
//  }
//
//  val tablenode = LNode("Data type Table",
//    ExpressionDomainNode[VeritasConstruct, String](Table, Map(Table.name -> MetaVar("table"))))
//
//  val tablecons = Table.constrs.head
//  val tableconsnode = LNode("tablecons",
//    ExpressionDomainNode[VeritasConstruct, String](tablecons,
//      Map(tablecons.in.head.name -> MetaVar("attrl"),
//        tablecons.in(1).name -> MetaVar("rawtable"))))
//
//  val tableedge: LEdge[String, ExpressionDomainEdgeLabel] = LEdge(tvaluenode.vertex, tablenode.vertex, DTConstructorArg[String](tvaluecons.in.head.name))
//  val tableconsedge: LEdge[String, ExpressionDomainEdgeLabel] = LEdge(tablenode.vertex, tableconsnode.vertex, DTConstructor[String](Table.name))
//
//  val tablesubgraph =
//    DomainGraph[VeritasConstruct, String, ExpressionDomainNode[VeritasConstruct, String], ExpressionDomainEdgeLabel](
//      Seq(tvaluenode, tablenode, tableconsnode),
//      Seq(tableedge, tableconsedge)
//    )
//
//  test("Print table subgraph") {
//    println("Table subgraph: ")
//    println(tablesubgraph.getInternalGraph.toString())
//    println("")
//  }
//
//  test("Add a subgraph") {
//    val combined = basicDomainGraph +++ tablesubgraph
//    println("First combined graph: ")
//    println(combined.getInternalGraph.toString())
//    println("")
//  }
//
//}
