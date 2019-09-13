package fpinscala

import org.scalatest.{FreeSpec, Matchers}

class Tail extends FreeSpec with Matchers {

  "3.2 - tail" in {
    List.tail(List(1,2,3,4)) shouldEqual List(2,3,4)
  }

  "3.3 - setHead" in {
    List.setHead(666, List(1,2,3)) shouldEqual List(666,2,3)
    List.setHead(666, Nil) shouldEqual List(666)
  }

  "3.4 - drop" in {
    List.drop(List("a","b","c","d","e"), 3) shouldEqual List("d","e")
    List.drop(List(1,2,3), 5) shouldEqual Nil
  }

  "3.5 - dropWhile" in {
    List.dropWhile(List(1,2,3,4,5))(a => a < 4) shouldEqual List(4,5)
    List.dropWhile(List("a","ab","abc"))(a => a.length < 3) shouldEqual List("abc")
  }

  "3.6 - init" in {
    List.init(List(1,2,3,4)) shouldEqual List(1,2,3)
  }
}
