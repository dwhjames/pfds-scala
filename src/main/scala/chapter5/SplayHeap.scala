package chapter5

sealed abstract class SplayHeap[+A <% Ordered[A]] {
  import SplayHeap._
  def isEmpty: Boolean
  def findMin: A
  def deleteMin: SplayHeap[A]
  
  private[chapter5] def partition[B >: A <% Ordered[B]](pivot: B):
    (SplayHeap[B], SplayHeap[B]) = this match {
      case Leaf => (Leaf, Leaf)
      case Branch(a, x, b) =>
        if (x <= pivot) {
          b match {
            case Leaf => (this, Leaf)
            case Branch(b1, y, b2) =>
              if (y <= pivot) {
                val (small, big) = b2.partition(pivot)
                (Branch(Branch(a, x, b1), y, small), big)
              } else {
                val (small, big) = b1.partition(pivot)
                (Branch(a, x, small), Branch(big, y, b2))
              }
          }
        } else {
          a match {
            case Leaf => (Leaf, this)
            case Branch(a1, y, a2) =>
              if (y <= pivot) {
                val (small, big) = a2.partition(pivot)
                (Branch(a1, y, small), Branch(big, x, b))
              } else {
                val (small, big) = a1.partition(pivot)
                (small, Branch(big, y, Branch(a2, x, b)))
              }
          }
        }
    }
  
  def insert[B >: A <% Ordered[B]](x: B): SplayHeap[B] = {
    val (a, b) = partition(x)
    Branch(a, x, b)
  }
  
  def merge[B >: A <% Ordered[B]](that: SplayHeap[B]): SplayHeap[B] = this match {
    case Leaf => that
    case Branch(a, x, b) => {
      val (thata, thatb) = that.partition(x)
      Branch(a.merge(thata), x, b.merge(thatb))
    }
  }
}

object SplayHeap {
  def empty[A]: SplayHeap[A] = Leaf
  def apply[A <% Ordered[A]](as: A*): SplayHeap[A] = {
    var h: SplayHeap[A] = Leaf
    for (a <- as) {
      h = h.insert(a)
    }
    h
  }
  
  private[chapter5] case object Leaf extends SplayHeap[Nothing] {
    override def isEmpty = true
    override def findMin = throw new Exception("Leaf.findMin: empty heap")
    override def deleteMin = throw new Exception("Leaf.deleteMin: empty heap")
  }
  
  private[chapter5] case class Branch[+A <% Ordered[A]](
      val left: SplayHeap[A],
      val elem: A,
      val right: SplayHeap[A]) extends SplayHeap[A] {
    override def isEmpty = false
    override def findMin = if (left.isEmpty) elem else left.findMin
    override def deleteMin = left match {
      case Leaf => right
      case Branch(a, x, b) =>
        if (a.isEmpty)
          Branch(b, elem, right)
        else
          Branch(a.deleteMin, x, Branch(b, elem, right))
    }
  }
}
