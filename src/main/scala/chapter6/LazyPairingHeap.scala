package chapter6

sealed abstract class LazyPairingHeap[+A <% Ordered[A]] {
  import LazyPairingHeap._
  def isEmpty: Boolean
  def findMin: A
  def deleteMin: LazyPairingHeap[A]

  def merge[B >: A <% Ordered[B]](that: LazyPairingHeap[B]): LazyPairingHeap[B] =
    if (that.isEmpty)
      this
    else if (this.isEmpty)
      that
    else {
      val a = this.asInstanceOf[Branch[A]]
      val b = that.asInstanceOf[Branch[B]]
      if (a.elem <= b.elem)
        link(a, b)
      else
        link(b, a)
    }

  def insert[B >: A <% Ordered[B]](x: B): LazyPairingHeap[B] =
    merge(new Branch(x, Leaf, Leaf))
}

object LazyPairingHeap {

  def empty[A]: LazyPairingHeap[A] = Leaf

  def apply[A <% Ordered[A]](as: A*): LazyPairingHeap[A] = {
    var h: LazyPairingHeap[A] = Leaf
    for (a <- as) {
      h = h.insert(a)
    }
    h
  }

  def link[A <% Ordered[A]](b1: Branch[A], b2: Branch[A]): Branch[A] =
    if (b1.left.isEmpty)
      new Branch(b1.elem, b2, b1.right)
    else
      new Branch(b1.elem, Leaf, b2 merge b1.left merge b1.right)

  private[chapter6] object Leaf extends LazyPairingHeap[Nothing] {
    override def isEmpty = true
    override def findMin = throw new Exception("Leaf.findMin: empty heap")
    override def deleteMin = throw new Exception("Leaf.deleteMin: empty heap")
  }

  private[chapter6] class Branch[+A <% Ordered[A]](
      val elem: A,
      val left: LazyPairingHeap[A],
      r: => LazyPairingHeap[A]) extends LazyPairingHeap[A] {
    lazy val right: LazyPairingHeap[A] = r
    override def isEmpty = false
    override def findMin = elem
    override def deleteMin = left merge right
  }
}
