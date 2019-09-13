package fpinscala

object Fold {

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil          => z
      case Cons(x, xs)  => foldLeft(xs, f(z, x))(f)
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldRightNotTailRec[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRightNotTailRec(xs, z)(f))
    }

  def sum(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)

  def product(as: List[Int]): Int =
    foldLeft(as, 1)(_ * _)

  def count[A](as: List[A]): Int =
    foldLeft[A, Int](as, 0)((s, _) => s + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft[A, List[A]](as, Nil)((b, a) => Cons(a, b))

  def append[A](a: A, as: List[A]): List[A] =
    foldRight(as, List(a))(Cons(_, _))

  def concatenate[A](l: List[List[A]]): List[A] =
    foldLeft[List[A], List[A]](l, Nil)((b, a) =>
      foldLeft[A, List[A]](a, b)((b2, a2) => append(a2, b2))
    )

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldLeft[A, List[B]](as, Nil)((b, a) => append(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldLeft[A, List[A]](as, Nil)((b, a) => {
      if (f(a)) append(a, b)
      else b
    })
}
