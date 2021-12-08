package competitivescala.utils

import org.scalatest.funsuite.AnyFunSuite
import SAT._

class SATTest extends AnyFunSuite {

  test("sat solving") {
    def v(id: String, positive: Boolean = true): (String, Boolean) = (id, positive)

    def check(cnf: Set[Set[(String, Boolean)]], sat: Boolean): Unit = {
      if(sat) {
        val result = solveDPPL(cnf)
        assert(result.nonEmpty)
        val assignment = result.get
        assert(checkSAT(cnf, assignment))
      } else {
        assert(solveDPPL(cnf).isEmpty)
      }
    }
    def testSAT(clauses: Set[(String, Boolean)]*): Unit = check(clauses.toSet, sat = true)
    def testUNSAT(clauses: Set[(String, Boolean)]*): Unit = check(clauses.toSet, sat = false)

    testSAT()
    testUNSAT(Set.empty)
    testUNSAT(Set.empty, Set(v("a")))
    testSAT(Set(v("a")))
    testSAT(Set(v("a", false)))
    testSAT(Set(v("a"), v("b")))
    testSAT(Set(v("a")), Set(v("b")))
    testSAT(Set(v("a")), Set(v("a")))
    testUNSAT(Set(v("a")), Set(v("a", false)))
    testSAT(Set(v("a"), v("b", false)), Set(v("b"), v("c", false)), Set(v("c"), v("a", false)))
  }

}
