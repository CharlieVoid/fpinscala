package fpinscala

sealed trait List[+A]

case object Nil
  extends List[Nothing]

case class Cons[+A](head: A, tail: List[A])
  extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }

  def setHead[A](a: A, l: List[A]): List[A] =
    Cons(a, List.tail(l))

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else drop(tail(l), n-1)
  }

  def dropWhile[A](l: List[A])(shouldDrop: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) if shouldDrop(h) => dropWhile(t)(shouldDrop)
      case left => left
    }

  def init[A](l: List[A]): List[A] = {
    def go[A](head: A, tail: List[A]): Cons[A] = {
      tail match {
        case Nil =>
          Cons(head, Nil)
        case Cons(_, Nil) =>
          Cons(head, Nil)
        case Cons(h, t) =>
          Cons(head, go(h, t))
      }
    }
    l match {
      case Nil => Nil
      case Cons(h, t) => go(h,t)
    }
  }

  def length[A](as: List[A]): Int = {
    Fold.foldRightNotTailRec[A, Int](as, 0)((_, sum) => sum + 1)
  }

}
