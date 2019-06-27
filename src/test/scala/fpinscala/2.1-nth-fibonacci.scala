package fpinscala

import org.scalatest.{FreeSpec, Matchers}

class NthFibonacci extends FreeSpec with Matchers {

  "1st is 0" in {
    fibTailRec(1) shouldBe 0
    fibRec(1) shouldBe 0
  }
  "2nd is 1" in {
    fibTailRec(2) shouldBe 1
    fibRec(2) shouldBe 1
  }
  "3rd is 1" in {
    fibTailRec(3) shouldBe 1
    fibRec(3) shouldBe 1
  }
  "4th is 2" in {
    fibTailRec(4) shouldBe 2
    fibRec(4) shouldBe 2
  }
  "5th is 3" in {
    fibTailRec(5) shouldBe 3
    fibRec(5) shouldBe 3
  }
  "6th is 5" in {
    fibTailRec(6) shouldBe 5
    fibRec(6) shouldBe 5
  }
  "7th is 8" in {
    fibTailRec(7) shouldBe 8
    fibRec(7) shouldBe 8
  }

  def fibRec(n: Int): Int = {
    if (n <= 1)
      0
    else if (n == 2)
      1
    else
      fibRec(n-1) + fibRec(n-2)
  }

  def fibTailRec(n: Int): Int = {

    @annotation.tailrec
    def go(i: Int, next: Int, sum: Int): Int = {
      if (i >= n)
        sum
      else
        go(i + 1, sum, sum + next)
    }

    go(1, 1, 0)
  }
}
