package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.smtlib._
import de.tu_darmstadt.veritas.backend.transformation.collect.{CollectTypes, CollectTypesClass}
import de.tu_darmstadt.veritas.backend.util.FreeVariables

class ToSMTLib {
  private var closedDatatypeDeclarations: Seq[DataTypeDeclaration] = Seq()
  private var openDatatypeDeclarations: Seq[Sort] = Seq()
  private var functionDeclarations: Seq[FunctionDeclaration] = Seq()
  private var assertions: Seq[Assertion] = Seq()
  private var goal: Option[Goal] = None

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
    for ((name, (in, out)) <- types.functypes ++ types.pfunctypes)
      functionDeclarations :+= encodeFunctionType(name, in, out)
    veritasModule match {
      case Module(name, Seq(), body) => {
        encodeBody(body)
        constructFinalSMTLib(name)
      }
      case Module(name, _, _) => throw TransformationError(s"Failed to transform Module ${name} to SMTLib: Module still contained imports!")
    }
  }

  private def encodeClosedDataType(name: String, constructors: Seq[DataTypeConstructor]): DataTypeDeclaration = {
    val encodedCotrs = constructors.map {
      encodeConstructor(_)
    }
    DataTypeDeclaration(name, encodedCotrs)
  }

  private def encodeConstructor(cotr: DataTypeConstructor): Constructor = {
    // because we dont have the information of selector names we encode them as dataTypename_indexOfParam
    val encodedSelectors = cotr.in.zipWithIndex.map { case (sr, index) =>
      val selectorName = s"${cotr.name}_${index}"
      Selector(selectorName, Type(sr.name))
    }
    Constructor(cotr.name, encodedSelectors)
  }

  private def encodeFunctionType(name: String, parameter: Seq[SortRef], result: SortRef): FunctionDeclaration = {
    val encodedParams = parameter.map { sr => Type(sr.name) }
    val encodedResult = Type(result.name)
    FunctionDeclaration(name, encodedParams, encodedResult)
  }

  private def encodeBody(body: Seq[ModuleDef]): Unit = {
    body.dropRight(1).foreach {
      case a@Axioms(_) => assertions ++= encodeAssertions(a)
      case Goals(gs, _) => throw TransformationError("Found goal in Module which was not at last position!")
      case DataType(_, name, cs) => {}
      case Consts(cs, _) => {}
      case Sorts(s) => {}
      case Functions(fds) => {}
      case PartialFunctions(fds) => {}
      case _ => throw TransformationError("Unsupported top-level construct!")
    }
    body.last match {
      case Goals(gs, _) => goal = Some(encodeGoal(gs.head))
      case _ => throw TransformationError("Module contained no goal or goal in module was not at last position!")
    }
  }

  private def encodeAssertions(axioms: Axioms): Seq[Assertion] = {
    axioms.axioms.map { axiom =>
      Assertion(encodeTypingRule(axiom.premises, axiom.consequences))
    }
  }

  /**
    * translates goals to SMTLib
    */
  private def encodeGoal(g: TypingRule): Goal =
    g match {
      case TypingRule(name, prems, conseqs) =>
        Goal(name, Assertion(Not(encodeTypingRule(prems, conseqs))))
    }

  private def encodeTypingRule(premises: Seq[TypingRuleJudgment], consequences: Seq[TypingRuleJudgment]): Term = {
    val quantifiedVars = FreeVariables.freeVariables(premises ++ consequences)
    val jdgs = premises ++ consequences
    types.inferMetavarTypes(quantifiedVars, jdgs)
    val vars = makeVarlist(quantifiedVars.toSeq, jdgs)

    val transformedprems = premises map encodeJudgement

    if (transformedprems == Seq(True) || transformedprems == Seq())
      ForAll(vars, And(consequences map encodeJudgement))
    else
      ForAll(vars, Impl(And(transformedprems), And(consequences map encodeJudgement)))
  }

  private def makeVarlist(vars: Seq[MetaVar], jdgs: Seq[TypingRuleJudgment]): Seq[SortedVariable] = {
    for (v <- vars)
      yield SortedVariable(v.name, Type(v.sortType.name))
  }

  /**
    * translates individual clauses (premises or conclusion) to SMTLib (-> Term)
    */
  private def encodeJudgement(jdg: TypingRuleJudgment): Term =
    jdg match {
      case FunctionExpJudgment(f) => encodeFunctionExp(f)
      case ExistsJudgment(vars, jdglist) => {
        val mappedvars = makeVarlist(vars, jdglist)
        if (mappedvars.isEmpty)
          And(jdglist map encodeJudgement)
        else
          Exists(mappedvars, And(jdglist map encodeJudgement))
      }
      case ForallJudgment(vars, jdglist) => {
        val mappedvars = makeVarlist(vars, jdglist)
        if (mappedvars.isEmpty)
          And(jdglist map encodeJudgement)
        else
          ForAll(mappedvars, And(jdglist map encodeJudgement))
      }
      case NotJudgment(jdg) => Not(encodeJudgement(jdg))
      case OrJudgment(ors) => {
        val translatedors = ors map (orcase => And(orcase map encodeJudgement))
        if (translatedors.isEmpty)
          True
        else if (translatedors.length == 1)
          translatedors.head
        else Or(translatedors)
      }
      case _ => throw TransformationError("Encountered unsupported judgment while translating a goal or axiom (e.g. typing judgment)")
    }

  /**
    * translate individual function expressions to SMTLib (-> Term);
    */
  private def encodeFunctionExp(f: FunctionExp): Term =
    f match {
      case FunctionExpNot(f) => Not(encodeFunctionExp(f))
      case FunctionExpEq(f1, f2) => Eq(encodeFunctionExpMeta(f1), encodeFunctionExpMeta(f2))
      case FunctionExpNeq(f1, f2) => Not(Eq(encodeFunctionExpMeta(f1), encodeFunctionExpMeta(f2)))
      case FunctionExpAnd(l, r) => And(Seq(encodeFunctionExp(l), encodeFunctionExp(r)))
      case FunctionExpOr(l, r) => Or(Seq(encodeFunctionExp(l), encodeFunctionExp(r)))
      case FunctionExpBiImpl(l, r) => encodeBiImpl(l, r)
      case FunctionExpLet(name, binding, body) => Let(Seq(VariableBinding(name, encodeFunctionExpMeta(binding))), encodeFunctionExp(body.asInstanceOf[FunctionExp]))
      case FunctionExpIf(cond, thenE, elseE) => IfThenElse(encodeFunctionExp(cond), encodeFunctionExp(thenE.asInstanceOf[FunctionExp]), encodeFunctionExp(elseE.asInstanceOf[FunctionExp]))
      case FunctionExpApp(n, args) => Appl(n, args map encodeFunctionExpMeta)
      case FunctionExpTrue => True
      case FunctionExpFalse => False
      case _ => throw TransformationError("Encountered unsupported function expression while translating (e.g. if or let expression)")
    }

  private def encodeBiImpl(left: FunctionExp, right: FunctionExp): Term = {
    val encodedLeft = encodeFunctionExp(left)
    val encodedRight = encodeFunctionExp(right)
    encodedLeft match {
      case True => encodedRight
      case False => Not(encodedRight)
      case _ => // do nothing
    }
    encodedRight match {
      case True => encodedLeft
      case False => Not(encodedLeft)
      case _ => Eq(encodedLeft, encodedRight)
    }
  }

  private def encodeFunctionExpMeta(f: FunctionExpMeta): Term =
    f match {
      case FunctionMeta(MetaVar(m)) => VariableReference(m)
      case FunctionExpVar(n) => VariableReference(n) //can occur inside lets, but should not occur elsewhere!
      case FunctionExpApp(n, args) => Appl(n, args map encodeFunctionExpMeta)
      case FunctionExpLet(name, binding, body) => Let(Seq(VariableBinding(name, encodeFunctionExpMeta(binding))), encodeFunctionExpMeta(body))
      case FunctionExpIf(cond, thenE, elseE) => IfThenElse(encodeFunctionExp(cond), encodeFunctionExpMeta(thenE), encodeFunctionExpMeta(elseE))
      case _ => throw TransformationError("Encountered unexpected construct in encodeFunctionExpMeta.")
    }

  private def constructFinalSMTLib(name: String): SMTLibFile = {
    goal match {
      case Some(g) => SMTLibFile(name, g.name,
        openDatatypeDeclarations ++ closedDatatypeDeclarations ++ functionDeclarations ++ assertions ++ Seq(g, CheckSat))
      case None => throw TransformationError(s"There was no goal in Module ${name}; SMTLib Transformation failed!")
    }
  }
}

object ToSMTLib {
  def apply(m: Module)(implicit config: Configuration) = (new ToSMTLib).toSMTLibFile(m)(config)
}
