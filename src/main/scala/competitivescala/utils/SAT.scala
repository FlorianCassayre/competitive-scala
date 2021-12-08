package competitivescala.utils

object SAT {

  def checkSAT[I](cnf: Set[Set[(I, Boolean)]], assignment: Map[I, Boolean]): Boolean =
    cnf.flatMap(_.map(_._1)).subsetOf(assignment.keySet) &&
      cnf.forall(_.exists { case (v, b) => assignment(v) == b })

  // (x11 \/ ... \/ x1a) /\ ... /\ (xk1 \/ ... \/ xkq)
  // ("a", true) stands for `a`, while ("a", false) stands for `~a`
  def solveDPPL[I](cnf: Set[Set[(I, Boolean)]]): Option[Map[I, Boolean]] = {
    def repeat(cnf: Set[Set[(I, Boolean)]], assignment: Map[I, Boolean]):
    Either[(Set[Set[(I, Boolean)]], Map[I, Boolean]), Option[Map[I, Boolean]]] = {
      val units = cnf.filter(_.sizeIs == 1).map(_.head)
      val newAssignment = assignment ++ units
      val newCnf = units.foldLeft(cnf) { case (acc, (v, b)) => acc.filter(!_.contains((v, b))).map(_ - ((v, !b))) }
      if(newCnf.isEmpty) {
        Right(Some(newAssignment))
      } else if(newCnf.contains(Set.empty)) {
        Right(None)
      } else {
        if(units.nonEmpty) {
          repeat(newCnf, newAssignment)
        } else {
          Left((cnf, newAssignment))
        }
      }
    }
    repeat(cnf, Map.empty) match {
      case Left((cnf, assignment)) =>
        val v = cnf.head.head._1
        solveDPPL(cnf + Set((v, false))).orElse(solveDPPL(cnf + Set((v, true)))).map(_ ++ assignment)
      case Right(verdict) => verdict
    }
  }

}
