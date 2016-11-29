package system

import system.Syntax.{ISort, Symbol, Var}

class Gensym {
  private var counters = Map[String, Int]()

  private def getNextAndSet(name: String): String = {
    counters.get(name) match {
      case None =>
        counters += (name -> 1)
        name + "_0"
      case Some(c) =>
        counters += (name -> (c + 1))
        name + "_" + c
    }
  }

  def freshSymbol(name: String, in: List[ISort], out: ISort): Symbol =
    Symbol(getNextAndSet(name), in, out)

  def freshVar(name: String, sort: ISort): Var =
    Var(getNextAndSet(name), sort)
}