package fpinscala

object Fold {

  def foldRightNotTailRec[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRightNotTailRec(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil          => z
      case Cons(x, xs)  => foldLeft(xs, f(z, x))(f)
    }
  }

  def sum(as: List[Int]): Int = {
    foldLeft(as, 0)(_ + _)
  }

  def product(as: List[Int]): Int = {
    foldLeft(as, 1)(_ * _)
  }

  def count[A](as: List[A]): Int = {
    foldLeft[A, Int](as, 0)((s, _) => s + 1)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft[A, List[A]](as, Nil)((b: List[A], a: A) =>
      Cons(a, b)
    )
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b: B, a: A) => f(a,b))
  }

  def append[A](a: A, as: List[A]): List[A] = {
    foldRight(as, List(a))(Cons(_, _))
  }

  def concatenate[A](l: List[List[A]]): List[A] = {
    foldLeft[List[A], List[A]](l, Nil)((b: List[A], a: List[A]) =>
      foldLeft[A, List[A]](a, b)((b2, a2) => append(a2, b2))
    )
  }

}
