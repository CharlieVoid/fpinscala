package fpinscala

import org.scalatest.{FreeSpec, Matchers}

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
    Fold.foldLeft(List(1,2,3,4), 0)(_ + _) shouldBe 10
  }
}
