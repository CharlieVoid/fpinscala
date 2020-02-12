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
    val x = Fold.foldRightNotTailRec(List(1,2,3), Nil: List[Int])(Cons(_,_))
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
    foldRight           (List("a","b","c","d"), "")((a,b) => b + a) shouldBe "dcba"
    foldRightNotTailRec (List("a","b","c","d"), "")((a,b) => b + a) shouldBe "dcba"
  }

  "3.14 - append using fold" in {
    append(4, List(1,2,3)) shouldBe List(1,2,3,4)
  }

  "3.15 - concatenate list of lists" in {
    concatenate(List(List(1,2), List(3,4), List(5))) shouldBe List(1,2,3,4,5)
  }

  "3.16 - add 1 to integers" in {
    def add1(is: List[Int]): List[Int] =
      foldRight[Int, List[Int]](is, Nil)((a,b) => Cons(a+1, b))

    add1(List(0, 1, 2)) shouldBe List(1,2,3)
  }

  "3.17 - doubles to strings" in {
    def dToS(ds: List[Double]): List[String] =
      foldRight[Double, List[String]](ds, Nil)((a, b) => Cons(a.toString, b))

    dToS(List(1.1, 2.2, 3.3)) shouldBe List("1.1", "2.2", "3.3")
  }

  "3.18 - map" in {
    map(List(1.1, 2.2, 3.3))(_.toString) shouldBe List("1.1", "2.2", "3.3")
  }

  "3.19 - filter" in {
    filter(List(1,2,3,4,5))(_ % 2 == 0) shouldBe List(2,4)
  }

  "3.20 - flatMap" in {
    flatMap(List(1,2,3))(i => List(i, i)) shouldBe List(1,1,2,2,3,3)
  }

  "3.21 - filter using flatMap" in {
    filterUsingFlatMap(List(1,2,3,4,5))(_ % 2 == 0) shouldBe List(2,4)
  }

  "3.23 - zipWith" in {
    zipWith[Int](List(1, 2, 3), List(4, 5, 6), (a,b) => a + b) shouldBe List(5, 7, 9)
  }

  "3.24 - subsequence" in {
    hasSubsequence(List(1,2,3,4), List(1,2,3)) shouldBe true
    hasSubsequence(List(1,2,3,4), List(2,3)) shouldBe true
    hasSubsequence(List(1,2,3,4), List(4)) shouldBe true
    hasSubsequence(List(1,2,3,4), List(5)) shouldBe false
    hasSubsequence(List(1,2,3,4), List(1,2,4)) shouldBe false
  }
  "3.25 - tree size" in {
    Tree.size(
      Leaf(1)
    ) shouldBe 1

    Tree.size(
      Branch[Int](
        Leaf(1),
        Leaf(2))
    ) shouldBe 3

    Tree.size(
      Branch[Int](
        Branch[Int](
          Leaf(1),
          Leaf(2)
        ),
        Leaf(3)
    )) shouldBe 5
  }

  "3.26 - tree max" in {
    Tree.max[Int](
      Leaf(1),
      0,
      _.max(_)
    ) shouldBe 1

    Tree.max[Int](
      Branch(
        Branch(
          Leaf(55),
          Leaf(77)
        ),
        Leaf(66)
      ),
      0,
      _.max(_)
    ) shouldBe 77

    Tree.max[Int](
      Branch(
        Branch(
          Leaf(66),
          Leaf(55)
        ),
        Leaf(77)
      ),
      0,
      _.max(_)
    ) shouldBe 77
  }

  "3.27 - tree depth" in {
    Tree.depth(Leaf(1)) shouldBe 1

    Tree.depth(
      Branch(
        Leaf(1),
        Leaf(2),
      )
    ) shouldBe 2

    Tree.depth(
      Branch(
        Branch(
          Leaf(1),
          Leaf(2)
        ),
        Branch(
          Branch(
            Leaf(1),
            Leaf(2)
          ),
          Leaf(2)
        )
      )
    ) shouldBe 4
  }

  "3.28 - tree map" in {
    Tree.map(
      Branch(
        Branch(
          Leaf(55),
          Leaf(77)
        ),
        Leaf(66)
      ),
      (a: Int) => a * 10
    ) shouldBe
      Branch(
        Branch(
          Leaf(550),
          Leaf(770)
        ),
        Leaf(660)
      )
  }

  "3.29 - tree fold size" in {
    Tree.sizeFold[Int](
      Leaf(1)
    ) shouldBe 1

    Tree.sizeFold[Int](
      Branch[Int](
        Leaf(1),
        Leaf(2)
      )
    ) shouldBe 3

    Tree.sizeFold[Int](
      Branch[Int](
        Branch[Int](
          Leaf(1),
          Leaf(2)
        ),
        Leaf(3)
      )
    ) shouldBe 5
  }

  "3.29 - tree fold max" in {
    Tree.maxFold[Int](
      Leaf(1),
      0,
      _.max(_)
    ) shouldBe 1

    Tree.maxFold[Int](
      Branch(
        Branch(
          Leaf(55),
          Leaf(77)
        ),
        Leaf(66)
      ),
      0,
      _.max(_)
    ) shouldBe 77

    Tree.maxFold[Int](
      Branch(
        Branch(
          Leaf(66),
          Leaf(55)
        ),
        Leaf(77)
      ),
      0,
      _.max(_)
    ) shouldBe 77
  }

  "3.29 - tree fold depth" in {
    Tree.depthFold(
      Leaf(1)
    ) shouldBe 1

    Tree.depthFold(
      Branch(
        Leaf(1),
        Leaf(2),
      )
    ) shouldBe 2

    Tree.depthFold(
      Branch(
        Branch(
          Leaf(1),
          Leaf(2)
        ),
        Branch(
          Branch(
            Leaf(1),
            Leaf(2)
          ),
          Leaf(2)
        )
      )
    ) shouldBe 4
  }
}
