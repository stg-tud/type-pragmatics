package de.tu_darmstadt.veritas.scalaspl.util

import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.ast.function._

case class VeritasAugmentedCallGraph(funname: String) extends AugmentedCallGraph[FunctionEq, FunctionExp, FunctionExpMeta] {

  override val toplevel_fun: String = funname

  override protected def expressionToString(expression: FunctionExpMeta): String = expression.toPrettyString()

  override protected def getRHSOfEquation(eq: FunctionEq): FunctionExpMeta = eq.right

  override protected def makeLabelFromSingleEq(eq: FunctionEq): String = {
    val patterns = for (p <- eq.patterns) yield p.toPrettyString()
    patterns.mkString(", ")
  }

  override protected def criteriaToString(c: FunctionExp): String = c.toPrettyString()

  override def getVariableName(mv: FunctionExpMeta): String = mv match {
    case FunctionMeta(MetaVar(name)) => name
    case _ => sys.error(s"Could not retrieve a variable name from $mv")
  }

  override def getVarExpAtDistarg_pos(arglist: Seq[FunctionExpMeta], distposlist: Seq[Int]): FunctionExpMeta = {

    def retrieveAndArg(fexpand: FunctionExpMeta, i: Int): FunctionExpMeta = fexpand match {
      case eq@FunctionExpEq(_, _) => eq
      case FunctionExpAnd(l, r) => if (i == 0) l else retrieveAndArg(r, i-1)
    }


    def retrieveArgList(exp: FunctionExpMeta, eqindex: Int): Seq[FunctionExpMeta] =
      exp match {
        case FunctionExpEq(_, FunctionExpApp(_, args)) => args
        case FunctionExpApp(_, args) => args
        case a@FunctionExpAnd(_, _) => retrieveArgList(retrieveAndArg(a, eqindex), eqindex) //should probably be recursive
        case a => Seq(a)
      }

    //length of distarg_pos determines how far we have to look for a variable within arglist (is this really always true?)
    //for complex argument expressions (at top-level only!), head of distposlist chooses the equation where we have to look
    //(this might not be true in every case!)
    val varlist = for (i <- distposlist.indices) yield retrieveArgList(arglist(i), distposlist.head)(distposlist(i))
    val res = varlist.last

    res match {
      case fm@FunctionMeta(_) => fm
      case _ => sys.error(s"Variable name from $arglist at $distposlist could not be retrieved.")
    }
  }

}
