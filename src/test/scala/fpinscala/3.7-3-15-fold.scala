package fpinscala

import org.scalatest.{FreeSpec, Matchers}
import fpinscala.Fold._

class FoldSpec extends FreeSpec with Matchers {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(i, is) => i + sum(is)
  }

  def product(doubles: List[Double]): Double = doubles match {
    case Nil => 1.0
    case Cons(d, ds) => d * product(ds)
  }

  "3.8 - Nil and Cons to foldRight" in {
    val x = Fold.foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_))
    println(x)
  }

  "3.9 - Length of list" in {
    List.length(List(1,2,3,4)) shouldBe 4
  }

  "3.10 - foldLeft tail-recursive " in {
    foldLeft(List(1,2,3,4), 0)(_ + _) shouldBe 10
  }

  "3.11 - sum product count" in {

    sum(List(1,2,3,4)) shouldBe 10

    product(List(2,2,2)) shouldBe 8

    count(List("a", "b", "c")) shouldBe 3
  }

  "3.12 - reverse" in {
    reverse(List(1,2,3,4)) shouldBe List(4,3,2,1)
  }

  "3.13 - foldRight tailrec via foldLeft" in {
    foldRightTailRec (List("a","b","c","d"), "")((a,b) => b + a) shouldBe "dcba"
    foldRight        (List("a","b","c","d"), "")((a,b) => b + a) shouldBe "dcba"
  }
}
