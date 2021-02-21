package competitivescala.utils

import Memoization._

import org.scalatest.funsuite.AnyFunSuite

class MemoizationTest extends AnyFunSuite {

  test("memoized fibonacci") {
    val fibonacci = memoizedEval[Long, Long]((bottom, recursive) => {
      case n@(0 | 1) => bottom(n)
      case n => for {
        a <- recursive(n - 1)
        b <- recursive(n - 2)
      } yield a + b
    })

    assert(fibonacci(0) == 0)
    assert(fibonacci(1) == 1)
    assert(fibonacci(2) == 1)
    assert(fibonacci(3) == 2)
    assert(fibonacci(4) == 3)
    assert(fibonacci(5) == 5)
    assert(fibonacci(6) == 8)
    assert(fibonacci(10) == 55)
    assert(fibonacci(20) == 6765)
    assert(fibonacci(50) == 12586269025L)
    assert(fibonacci(90) == 2880067194370816120L)
  }

}
