package chapter2

import scala.annotation.tailrec

sealed abstract class Set[+A <% Ordered[A]] {
  import Set._
  def isEmpty: Boolean
  def insert[B >: A <% Ordered[B]](x: B): Set[B] = this match {
    case Leaf => Branch(Leaf, x, Leaf)
    case b@Branch(l,y,r) =>
      if (x < y)
        Branch(l.insert(x), y, r)
      else if (x > y)
        Branch(l, y, r.insert(x))
      else b
  }
  /*
  def member[B >: A <% Ordered[B]](x: B): Boolean = this match {
      case Leaf => false
      case Branch(l,y,r) =>
        if (x < y)
          l.member(x)
        else if (x > y)
          l.member(x)
        else
          true
    }
  */
  
  def member[B >: A <% Ordered[B]](x: B): Boolean = {
    @tailrec def local(a: A, s: Set[A]): Boolean = s match {
      case Leaf => x == a
      case Branch(l,y,r) =>
        if (x < y)
          local(a,l)
        else
          local(y, r)
    }
    this match {
      case Leaf => false
      case Branch(l,y,r) =>
        if (x < y)
          l.member(x)
        else
          local(y,r)
    }
  }
}

object Set {
  def empty[A]: Set[A] = Leaf
  def apply[A <% Ordered[A]](as: A*): Set[A] = {
    var s: Set[A] = Leaf
    for (a <- as) {
      s = s.insert(a)
    }
    s
  }
  
  case object Leaf extends Set[Nothing] {
    override def isEmpty = true
  }

  final case class Branch[+A <% Ordered[A]](
      private val left: Set[A],
      private val elem: A,
      private val right: Set[A]) extends Set[A] {
    override def isEmpty = false
  }
}