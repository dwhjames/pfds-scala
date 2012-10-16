package chapter3

import scala.annotation.tailrec

sealed abstract class RedBlackSet[+A <% Ordered[A]] {
  import RedBlackSet._

  def isEmpty: Boolean

/* Standard membership function
  def member[B >: A <% Ordered[B]](x: B): Boolean = this match {
    case Leaf => false
    case Branch(_, l,y,r) =>
      if (x < y)
        l.member(x)
      else if (x > y)
        r.member(x)
      else
        true
  }
*/

  def member[B >: A <% Ordered[B]](x: B): Boolean = {
    // cand is most recent element that might equal x
    @tailrec def local(cand: A, s: RedBlackSet[A]): Boolean = s match {
      case Leaf => x == cand
      case Branch(_, l,y,r) =>
        if (x < y)
          local(cand,l)
        else
          local(y, r)
    }
    this match {
      case Leaf => false
      case Branch(_, l,y,r) =>
        if (x < y)
          l.member(x)
        else
          local(y,r)
    }
  }

  def insert[B >: A <% Ordered[B]](x: B): RedBlackSet[B] = {
    def ins(set: RedBlackSet[A]): Branch[B] = set match {
      case Leaf => Branch(Red, Leaf, x, Leaf)
      case s @ Branch(color, a, y, b) =>
        if (x < y)
          balance(color, ins(a), y, b)
        else if (x > y)
          balance(color, a, y, ins(b))
        else s
    }
    val Branch(_, a, y, b) = ins(this)
    Branch(Black, a, y, b)
  }
}

object RedBlackSet {
  def empty[A]: RedBlackSet[A] = Leaf

  def apply[A <% Ordered[A]](as: A*): RedBlackSet[A] = {
    var s: RedBlackSet[A] = Leaf
    for (a <- as) {
      s = s.insert(a)
    }
    s
  }

  private[chapter3] def balance[A <% Ordered[A]](
    color: Color,
    left: RedBlackSet[A],
    elem: A,
    right: RedBlackSet[A]): Branch[A] = color match {
      case Red => Branch(color, left, elem, right)
      case Black => (left, elem, right) match {
        case (Branch(Red, Branch(Red, a, x, b), y, c), z, d) =>
          Branch(Red, Branch(Black, a, x, b), y, Branch(Black, c, z, d))
        case (Branch(Red, a, x, Branch(Red, b, y, c)), z, d) =>
          Branch(Red, Branch(Black, a, x, b), y, Branch(Black, c, z, d))
        case (a, x, Branch(Red, Branch(Red, b, y, c), z, d)) =>
          Branch(Red, Branch(Black, a, x, b), y, Branch(Black, c, z, d))
        case (a, x, Branch(Red, b, y, Branch(Red, c, z, d))) =>
          Branch(Red, Branch(Black, a, x, b), y, Branch(Black, c, z, d))
        case _ => Branch(color, left, elem, right)
      }
  }

  sealed abstract class Color
  case object Red extends Color
  case object Black extends Color

  case object Leaf extends RedBlackSet[Nothing] {
    override def isEmpty = true
  }

  final case class Branch[+A <% Ordered[A]](
      private val color: Color,
      private val left: RedBlackSet[A],
      private val elem: A,
      private val right: RedBlackSet[A]) extends RedBlackSet[A] {
    override def isEmpty = false
  }
}
