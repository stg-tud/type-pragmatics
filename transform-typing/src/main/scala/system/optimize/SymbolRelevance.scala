package system.optimize

import system.{Language, Transformation}
import system.Syntax._

/*
 * Computes which set of symbols is relevant for proving statements about another symbol.
 * Note that we only consider Prop symbols here.
 */
object SymbolRelevance {

  type Relevance = Map[Symbol, Set[Symbol]]

  def mergeRelevance(rel1: Relevance, rel2: Relevance): Relevance = {
    var res = rel1
    for ((s, ss) <- rel2 if !ss.isEmpty)
      rel1.get(s) match {
        case None => res += s -> ss
        case Some(ss0) => res += s -> (ss0 ++ ss)
      }
    res
  }

  def transitiveClosure(rel: Relevance): Relevance = {
    var res = rel
    while (true) {
      val newres = res.map { case (s, ss) =>
        val newss = ss.flatMap(s => res.getOrElse(s, Set()) + s)
        s -> newss
      }
      if (res == newres)
        return res
      res = newres
    }
    throw new MatchError(this)
  }

  private var relevanceCache = Map[Language, Relevance]()


  def relevance(lang: Language): Relevance = relevanceCache.get(lang) match {
    case Some(rel) => rel
    case None =>
      val rel = computeRelevance(lang)
      relevanceCache += lang -> rel
      rel
  }

  def computeRelevance(lang: Language): Relevance = {
    val ruleRel = lang.rules.foldLeft[Relevance](Map())((rel, r) => mergeRelevance(rel, computeRelevance(r)))
    val transRel = lang.transs.foldLeft[Relevance](Map())((rel, trans) => mergeRelevance(rel, computeRelevance(trans)))
    val mergedRel = mergeRelevance(ruleRel, transRel)
    transitiveClosure(mergedRel)
  }

  def computeRelevance(trans: Transformation): Relevance =
    (Seq(trans.contract._1) ++ trans.lemmas.keys).foldLeft[Relevance](Map())((rel, r) => mergeRelevance(rel, computeRelevance(r)))

  def computeRelevance(rule: Rule): Relevance =
    Map(rule.conclusion.sym -> rule.premises.map(_.sym).toSet)
}
