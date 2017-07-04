package de.tu_darmstadt.veritas.backend.transformation

import javax.security.auth.login.Configuration

import de.tu_darmstadt.veritas.backend.ast.{DataTypeConstructor, Module, SortRef}
import de.tu_darmstadt.veritas.backend.smtlib._
import de.tu_darmstadt.veritas.backend.transformation.collect.{CollectTypes, CollectTypesClass}


//object SMTLibTrans extends SeqTrans(
//  FilterGoalModules, //optimization: only interested in modules with goals!
//  ResolveImports,
//  VarToApp0,
//  DesugarLemmas)

class ToSMTLib {
  private var closedDatatypeDeclarations: Seq[DataTypeDeclaration] = Seq()
  private var openDatatypeDeclarations: Seq[Sort] = Seq()
  private var functionDeclarations: Seq[FunctionDeclaration] = Seq()
  private var assertions: Seq[Assertion] = Seq()
  private var goal: Option[SMTLib] = None

  private var types: CollectTypes = _
  def toSMTLibFile(veritasModule: Module)(implicit config: Configuration): SMTLibFile = {
    //make sure every mutable state is initialized when applying this!
    closedDatatypeDeclarations = Seq()
    openDatatypeDeclarations = Seq()
    functionDeclarations = Seq()
    assertions = Seq()
    goal = None

    types = new CollectTypesClass
    types.apply(Seq(veritasModule))

    for ((n, (isOpen, cotrs)) <- types.dataTypes)
      if (isOpen)
        openDatatypeDeclarations :+= Sort(n)
      else
        closedDatatypeDeclarations :+= encodeClosedDataType(n, cotrs)
    // collect constants TODO: why does ToTFF not add constants?
    // collect function defs
    for ((name, (in, out)) <- types.functypes ++ types.pfunctypes)
      functionDeclarations :+= encodeFunctionType(name, in, out)
    // collect judgements
    // collect axioms
    // collect the goal
    veritasModule match {
      case Module(name, Seq(), body) => SMTLibFile(null, null, closedDatatypeDeclarations ++ openDatatypeDeclarations ++ functionDeclarations ++ assertions)
      case Module(name, _, _) => throw TransformationError(s"Failed to transform Module ${name} to SMTLib: Module still contained imports!")
    }
  }


  private def encodeClosedDataType(name: String, constructors: Seq[DataTypeConstructor]): DataTypeDeclaration = {
    val encodedCotrs = constructors.map {
      encodeConstructor(name, _)
    }
    DataTypeDeclaration(name, encodedCotrs)
  }

  private def encodeConstructor(dataTypeName: String, cotr: DataTypeConstructor): Constructor = {
    // because we dont have the information of selector names we encode them as dataTypename_indexOfParam
    // TODO: maybe create global map for class to know the selector names for all datatypes
    val encodedSelectors = cotr.in.zipWithIndex.map { case (sr, index) =>
      val selectorName = s"${dataTypeName}_${index}"
      Selector(selectorName, Type(sr.name))
    }
    Constructor(cotr.name, encodedSelectors)
  }

  private def encodeFunctionType(name: String, parameter: Seq[SortRef], result: SortRef): FunctionDeclaration = {
    val encodedParams = parameter.map { sr => Type(sr.name) }
    val encodedResult = Type(result.name)
    FunctionDeclaration(name, encodedParams, encodedResult)
  }
}
