package fpinscala

sealed trait Tree[+A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = {
    def go(subtree: Tree[A], size: Int): Int = {
      subtree match {
        case Leaf(_) =>
          size + 1
        case Branch(l, r) =>
          size + 1 + go(l, size) + go(r, size)
      }
    }
    go(tree, 0)
  }

  def max[A](tree: Tree[A], a: A, f:(A, A) => A): A = {
    def go(subtree: Tree[A], max: A): A = {
      subtree match {
        case Leaf(a) =>
          f(a, max)
        case Branch(l, r) =>
          f(max, f(go(l, max), go(r, max)))
      }
    }
    go(tree, a)
  }

  def depth[A](tree: Tree[A]): Int = {
    def go(subtree: Tree[A], d: Int): Int = {
      subtree match {
        case Leaf(_) =>
          d + 1
        case Branch(l, r) =>
          d + 1 + math.max(go(l, 0), go(r, 0))
      }
    }
    go(tree, 0)
  }

  def map[A](tree: Tree[A], f: A => A): Tree[A] = {
    def go(subtree: Tree[A]): Tree[A] = {
      subtree match {
        case l: Leaf[A] =>
          Leaf(f(l.value))
        case Branch(l, r) =>
          Branch(go(l), go(r))
      }
    }
    go(tree)
  }

  def fold[A, B](tree: Tree[A], z: B, f: (B, Tree[A]) => B): B = {
    def go(subtree: Tree[A], b: B): B = {
      subtree match {
        case leaf: Leaf[A] =>
          f(b, leaf)
        case branch @ Branch(l, r) =>
          f(go(r, go(l, b)), branch)
      }
    }
    go(tree, z)
  }

  def sizeFold[A](tree: Tree[A]): Int =
    fold[A, Int](tree, 0, (size, _) => size + 1)

  def maxFold[A](tree: Tree[A], z: A, f: (A, A) => A): A = {
    val cmp = (max: A, node: Tree[A]) =>
      node match {
        case Leaf(value) =>
          f(max, value)
        case Branch(_, _) =>
          max
      }

    fold[A, A](tree, z, cmp)
  }

  def depthFold[A](tree: Tree[A]): Int = {
    def count: (Int, Tree[A]) => Int = (_: Int, subtree: Tree[A]) =>
      subtree match {
        case Leaf(_) => 1
        case Branch(l, r) =>
          1 + math.max(count(0, l), count(0, r))
      }
    fold[A, Int](tree, 0, count)
  }
}