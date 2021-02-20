package competitivescala.graphs

import UnionFind._
import org.scalatest.funsuite.AnyFunSuite

class UnionFindTest extends AnyFunSuite {

  def findExpect[U](x: U, expectedY: U): UnionFindState[U, U] = for {
    y <- find(x)
    _ = assert(y == expectedY)
  } yield y

  test("union find") {
    val operations = for {
      _ <- findExpect('a', 'a')
      _ <- union('a', 'b')
      _ <- findExpect('a', 'a')
      _ <- findExpect('b', 'a')
      _ <- union('c', 'd')
      _ <- findExpect('d', 'c')
      _ <- union('a', 'c')
      _ <- findExpect('d', 'a')
      _ <- findExpect('c', 'a')
      _ <- findExpect('b', 'a')
      _ <- union('b', 'e')
      _ <- findExpect('e', 'a')
      end <- UnionFindState.get
    } yield end

    operations.run(InitialInternal)
  }

}
