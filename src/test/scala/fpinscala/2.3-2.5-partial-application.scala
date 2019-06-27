package fpinscala

import org.scalatest.{FreeSpec, Matchers}

class PartialApplication extends FreeSpec with Matchers {

  "2.3 - Curry" in {
    def curry[A,B,C](f: (A,B) => C): A => B => C = {
      a => b => f(a, b)
    }
  }

  "2.4 - Uncurry" in {
    def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
      (a,b) => f(a)(b)
    }
  }

  "2.5 - HOF composing two functions" in {
    def compose[A,B,C](f: B => C, g: A => B): A => C = {
      a => f(g(a))
    }
  }
}
