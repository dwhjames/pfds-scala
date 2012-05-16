package chapter3

sealed abstract class WeightBiasedLeftistHeap[+A <% Ordered[A]] {
  import WeightBiasedLeftistHeap._
  def size: Int
  def isEmpty: Boolean
  def findMin: A
  def deleteMin: WeightBiasedLeftistHeap[A]
  
  def merge[B >: A <% Ordered[B]](that: WeightBiasedLeftistHeap[B]): WeightBiasedLeftistHeap[B] = (this, that) match {
    case (Leaf, _) => that
    case (_, Leaf) => this
    case (Branch(s1, x, a1, b1), Branch(s2, y, a2, b2)) => {
      val s = s1 + s2
      val (z, l, r1, r2) = if (x <= y) (x, a1, b1, that) else (y, a2, b2, this)
      if (l.size >= r1.size + r2.size)
        Branch(s, z, l, r1.merge(r2))
      else
        Branch(s, z, r1.merge(r2), l)
    }
  }
  
  def insert[B >: A <% Ordered[B]](x: B): WeightBiasedLeftistHeap[B] =
    merge(Branch(1, x, Leaf, Leaf))
}

object WeightBiasedLeftistHeap {
  def empty[A]: WeightBiasedLeftistHeap[A] = Leaf
  def apply[A <% Ordered[A]](as: A*): WeightBiasedLeftistHeap[A] = {
    var h: WeightBiasedLeftistHeap[A] = Leaf
    for (a <- as) {
      h = h.insert(a)
    }
    h
  }
  
  private[chapter3] case object Leaf extends WeightBiasedLeftistHeap[Nothing] {
    override def size = 0
    override def isEmpty = true
    override def findMin = throw new Exception("Leaf.findMin: empty heap")
    override def deleteMin = throw new Exception("Leaf.deleteMin: empty heap")
  }
  
  private[chapter3] case class Branch[+A <% Ordered[A]](
      override val size: Int,
      val elem: A,
      val left: WeightBiasedLeftistHeap[A],
      val right: WeightBiasedLeftistHeap[A]) extends WeightBiasedLeftistHeap[A] {
    override def isEmpty = false
    override def findMin = elem
    override def deleteMin = left.merge(right)
  }

}