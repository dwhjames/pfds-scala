package chapter3

sealed abstract class LeftistHeap[+A <% Ordered[A]] {
  import LeftistHeap._
  def rank: Int
  def isEmpty: Boolean
  def findMin: A
  def deleteMin: LeftistHeap[A]
  
  private def mkHeap[B >: A <% Ordered[B]](x: B, a: LeftistHeap[B], b: LeftistHeap[B]): LeftistHeap[B] =
    if (a.rank >= b.rank)
      Branch(b.rank + 1, x, a, b)
    else
      Branch(a.rank + 1, x, b, a)
  
  def merge[B >: A <% Ordered[B]](that: LeftistHeap[B]): LeftistHeap[B] = (this, that) match {
    case (Leaf, _) => that
    case (_, Leaf) => this
    case (Branch(_, x, a1, b1), Branch(_, y, a2, b2)) =>
      if (x <= y)
        mkHeap(x, a1, b1.merge(that))
      else
        mkHeap(y, a2, b2.merge(this))
  }
  
  def insert[B >: A <% Ordered[B]](x: B): LeftistHeap[B] =
    merge(Branch(1, x, Leaf, Leaf))
}

object LeftistHeap {
  def empty[A]: LeftistHeap[A] = Leaf
  def apply[A <% Ordered[A]](as: A*): LeftistHeap[A] = {
    var h: LeftistHeap[A] = Leaf
    for (a <- as) {
      h = h.insert(a)
    }
    h
  }
  
  private[chapter3] case object Leaf extends LeftistHeap[Nothing] {
    override def rank = 0
    override def isEmpty = true
    override def findMin = throw new Exception("Leaf.findMin: empty heap")
    override def deleteMin = throw new Exception("Leaf.deleteMin: empty heap")
  }
  
  private[chapter3] case class Branch[+A <% Ordered[A]](
      override val rank: Int,
      val elem: A,
      val left: LeftistHeap[A],
      val right: LeftistHeap[A]) extends LeftistHeap[A] {
    override def isEmpty = false
    override def findMin = elem
    override def deleteMin = left.merge(right)
  }
}