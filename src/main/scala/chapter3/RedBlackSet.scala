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

/* insert with monolithic balancing
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
*/

/* insert with split balancing smart constructors (lbalance, rbalance)
  def insert[B >: A <% Ordered[B]](x: B): RedBlackSet[B] = {
    def ins(set: RedBlackSet[A]): Branch[B] = set match {
      case Leaf => Branch(Red, Leaf, x, Leaf)
      case s @ Branch(color, a, y, b) =>
        if (x < y)
          lbalance(color, ins(a), y, b)
        else if (x > y)
          rbalance(color, a, y, ins(b))
        else s
    }
    val Branch(_, a, y, b) = ins(this)
    Branch(Black, a, y, b)
  }
*/

  def insert[B >: A <% Ordered[B]](x: B): RedBlackSet[B] = {
    def insl(branch: Branch[A]): Branch[B] = branch.left match {
      case Leaf =>
        branch.copyWith(left = Branch(Red, Leaf, x, Leaf))
      case l @ Branch(_, _, y, _) =>
        if (x < y)
          llbalance(branch.color, insl(l), branch.elem, branch.right)
        else if (x > y)
          lrbalance(branch.color, insr(l), branch.elem, branch.right)
        else
          branch
    }
    def insr(branch: Branch[A]): Branch[B] = branch.right match {
      case Leaf =>
        branch.copyWith(right = Branch(Red, Leaf, x, Leaf))
      case r @ Branch(_, _, y, _) =>
        if (x < y)
          rlbalance(branch.color, branch.left, branch.elem, insl(r))
        else if (x > y)
          rrbalance(branch.color, branch.left, branch.elem, insr(r))
        else
          branch
    }
    this match {
      case Leaf => Branch(Black, Leaf, x, Leaf)
      case b @ Branch(_, _, y, _) => {
        if (x < y)
          insl(b).copyWith(color = Black)
        else if (x > y)
          insr(b).copyWith(color = Black)
        else this
      }
    }
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

  def fromSortedList[A <% Ordered[A]](list: List[A]): RedBlackSet[A] = {
    def local(n: Int, l: List[A]): (Branch[A], List[A]) = {
      if (n == 1)
        (Branch(Red, Leaf, l.head, Leaf), l.tail)
      else if (n == 2) {
        val (left, x :: rest) = local(1, l)
        (Branch(Black, left, x, Leaf), rest)
      } else if (n % 2 == 1) {
        val (left, x :: restl) = local(n / 2, l)
        val (right, rest) = local(n / 2, restl)
        (Branch(Black, left, x, right), rest)
      } else {
        val (left, x :: restl) = local(n / 2, l)
        val (right, rest) = local(n / 2 - 1, restl)
        (Branch(Black, left, x, right.copyWith(color = Black)), rest)
      }
    }
    if (list.isEmpty)
      Leaf
    else
      local(list.length, list)._1
  }

/* monolithic balancing function
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
*/

/* left balancing for Exercise 3.10(a)
  private[chapter3] def lbalance[A <% Ordered[A]](
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
        case _ => Branch(color, left, elem, right)
      }
  }
*/

  private[chapter3] def llbalance[A <% Ordered[A]](
    color: Color,
    left:  RedBlackSet[A],
    elem:  A,
    right: RedBlackSet[A]): Branch[A] = (color, left, elem, right) match {
      case (Black, Branch(Red, Branch(Red, a, x, b), y, c), z, d) =>
        Branch(Red, Branch(Black, a, x, b), y, Branch(Black, c, z, d))
      case _ => Branch(color, left, elem, right)
  }

  private[chapter3] def lrbalance[A <% Ordered[A]](
    color: Color,
    left:  RedBlackSet[A],
    elem:  A,
    right: RedBlackSet[A]): Branch[A] = (color, left, elem, right) match {
      case (Black, Branch(Red, a, x, Branch(Red, b, y, c)), z, d) =>
        Branch(Red, Branch(Black, a, x, b), y, Branch(Black, c, z, d))
      case _ => Branch(color, left, elem, right)
  }

/* right balancing for Exercise 3.10(a)
  private[chapter3] def rbalance[A <% Ordered[A]](
    color: Color,
    left: RedBlackSet[A],
    elem: A,
    right: RedBlackSet[A]): Branch[A] = color match {
      case Red => Branch(color, left, elem, right)
      case Black => (left, elem, right) match {
        case (a, x, Branch(Red, Branch(Red, b, y, c), z, d)) =>
          Branch(Red, Branch(Black, a, x, b), y, Branch(Black, c, z, d))
        case (a, x, Branch(Red, b, y, Branch(Red, c, z, d))) =>
          Branch(Red, Branch(Black, a, x, b), y, Branch(Black, c, z, d))
        case _ => Branch(color, left, elem, right)
      }
  }
*/

  private[chapter3] def rlbalance[A <% Ordered[A]](
    color: Color,
    left:  RedBlackSet[A],
    elem:  A,
    right: RedBlackSet[A]): Branch[A] = (color, left, elem, right) match {
      case (Black, a, x, Branch(Red, Branch(Red, b, y, c), z, d)) =>
        Branch(Red, Branch(Black, a, x, b), y, Branch(Black, c, z, d))
      case _ => Branch(color, left, elem, right)
  }

  private[chapter3] def rrbalance[A <% Ordered[A]](
    color: Color,
    left:  RedBlackSet[A],
    elem:  A,
    right: RedBlackSet[A]): Branch[A] = (color, left, elem, right) match {
      case (Black, a, x, Branch(Red, b, y, Branch(Red, c, z, d))) =>
        Branch(Red, Branch(Black, a, x, b), y, Branch(Black, c, z, d))
      case _ => Branch(color, left, elem, right)
  }

  sealed abstract class Color
  case object Red extends Color
  case object Black extends Color

  case object Leaf extends RedBlackSet[Nothing] {
    override def isEmpty = true
  }

  final case class Branch[+A <% Ordered[A]](
      val color: Color,
      val left:  RedBlackSet[A],
      val elem:  A,
      val right: RedBlackSet[A]) extends RedBlackSet[A] {
    override def isEmpty = false

    def copyWith[B >: A <% Ordered[B]]
                (color: Color          = color,
                 left:  RedBlackSet[B] = left,
                 elem:  B              = elem,
                 right: RedBlackSet[B] = right): Branch[B] = {
      Branch(color, left, elem, right)
    }
  }
}
