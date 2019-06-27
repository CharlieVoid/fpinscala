package fpinscala

import org.scalatest.{FreeSpec, Matchers}

class PolymorphicSorting extends FreeSpec with Matchers {

  "Empty int" in {
    isSorted(Seq(), intAsc) shouldBe true
  }
  "One int" in {
    isSorted(Seq(1), intAsc) shouldBe true
  }
  "Ints asc" in {
    isSorted(Seq(1, 2, 3), intAsc) shouldBe true
  }
  "Ints asc false" in {
    isSorted(Seq(4, 2, 3), intAsc) shouldBe false
  }
  "Strings asc" in {
    isSorted(Seq("aabc", "abbc", "abc"), strAsc) shouldBe true
  }
  "String asc false" in {
    isSorted(Seq("b", "a", "aa"), strAsc) shouldBe false
  }

  def intAsc = (a: Int, b: Int) => a <= b
  def strAsc = (a: String, b: String) => a <= b

  def isSorted[A](as: Seq[A], ordered: (A,A) => Boolean): Boolean
    = isSortedTailRec(as, ordered)

  def isSortedRec[A](as: Seq[A], ordered: (A,A) => Boolean): Boolean = {
    as match {
      case Nil =>
        true
      case _ :: Nil =>
        true
      case a1 :: a2 :: tail =>
        if (ordered(a1, a2)) isSortedRec(a2 :: tail, ordered)
        else false
    }
  }

  def isSortedTailRec[A](as: Seq[A], ordered: (A,A) => Boolean): Boolean = {

    @annotation.tailrec
    def go(i: Int, as: Seq[A]): Boolean = {
      if (i >= as.length - 1)
        true
      else if (!ordered(as(i), as(i+1)))
        false
      else
        go(i+1, as)
    }

    go(0, as)
  }
}
