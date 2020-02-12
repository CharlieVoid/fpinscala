package fpinscala

import org.scalatest.{FreeSpec, Matchers}

import scala.{List => Lista, Nil => Nila}

class PolymorphicSorting extends FreeSpec with Matchers {

  "Empty int" in {
    isSorted(Lista(), intAsc) shouldBe true
  }
  "One int" in {
    isSorted(Lista(1), intAsc) shouldBe true
  }
  "Ints asc" in {
    isSorted(Lista(1, 2, 3), intAsc) shouldBe true
  }
  "Ints asc false" in {
    isSorted(Lista(4, 2, 3), intAsc) shouldBe false
  }
  "Strings asc" in {
    isSorted(Lista("aabc", "abbc", "abc"), strAsc) shouldBe true
  }
  "String asc false" in {
    isSorted(Lista("b", "a", "aa"), strAsc) shouldBe false
  }

  def intAsc = (a: Int, b: Int) => a <= b
  def strAsc = (a: String, b: String) => a <= b

  @annotation.tailrec
  final def isSorted[A](as: Lista[A], ordered: (A,A) => Boolean): Boolean = {
    as match {
      case Nila => true
      case _ :: Nila => true
      case a1 :: a2 :: Nila =>
        ordered(a1, a2)
      case a1 :: a2 :: tail =>
        if (ordered(a1, a2)) isSorted(a2 :: tail, ordered)
        else false
    }
  }
}
