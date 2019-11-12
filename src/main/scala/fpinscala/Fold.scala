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
    foldLeft[A, List[A]](as, Nil)((b, a) =>
      if (f(a)) append(a, b)
      else b
    )

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft[A, List[B]](as, Nil)((bs, a) =>
      foldLeft(f(a), bs)((bs2, b2) => append(b2, bs2))
    )

  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a =>
      if (f(a)) List(a)
      else Nil
    )

  def zipWith[A](as1: List[A], as2: List[A], f: (A,A) => A): List[A] = {
    def go(aa1: List[A], aa2: List[A], z: List[A]): List[A] = {
      (aa1, aa2) match {
        case (Nil, _) => z
        case (_, Nil) => z
        case (Cons(h1, t1), Cons(h2, t2)) =>
          go(t1, t2, append(f(h1, h2), z))
      }
    }

    go(as1, as2, Nil)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def go(sup: List[A], sub: List[A], toGo: List[A]): Boolean = {
      (sup, toGo) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(h1, t1), Cons(h2, t2)) =>
          if (h1 == h2) go(t1, sub, t2)
          else go(t1, sub, sub)
      }
    }

    go(sup, sub, sub)
  }

}
