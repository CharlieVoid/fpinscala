package fpinscala

object Fold {

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {

    @annotation.tailrec
    def go(a: A, as: List[A], b: B): B = {
      as match {
        case Nil            => b
        case Cons(x, xs)    => go(x, xs, f(b, x))
      }
    }

    as match {
      case Nil          => z
      case Cons(x, xs)  => go(x, xs, f(z, x))
    }
  }
}
