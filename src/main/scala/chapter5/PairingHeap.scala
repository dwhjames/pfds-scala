package chapter5

sealed abstract class PairingHeap[+A <% Ordered[A]] {
  import PairingHeap._
  def isEmpty: Boolean
  def findMin: A
  def deleteMin: PairingHeap[A]
  
  def insert[B >: A <% Ordered[B]](x: B): PairingHeap[B] = merge(Branch(x, Nil))
  
  def merge[B >: A <% Ordered[B]](that: PairingHeap[B]): PairingHeap[B] = (this, that) match {
    case (_, Leaf) => this
    case (Leaf, _) => that
    case (Branch(x, xs), Branch(y, ys)) =>
      if (x <= y)
        Branch(x, that :: xs)
      else
        Branch(y, this :: ys)
  }
}

object PairingHeap {
  def empty[A]: PairingHeap[A] = Leaf
  def apply[A <% Ordered[A]](as: A*): PairingHeap[A] = {
    var h: PairingHeap[A] = Leaf
    for (a <- as) {
      h = h.insert(a)
    }
    h
  }
  
  def mergePairs[A <% Ordered[A]](l: List[PairingHeap[A]]): PairingHeap[A] = l match {
    case Nil => Leaf
    case h1 :: t => t match {
      case Nil => h1
      case h2 :: hs => h1 merge h2 merge mergePairs(hs)
    }
  }
  
  private[chapter5] case object Leaf extends PairingHeap[Nothing] {
    override def isEmpty = true
    override def findMin = throw new Exception("Leaf.findMin: empty heap")
    override def deleteMin = throw new Exception("Leaf.deleteMin: empty heap")
  }
  
  private[chapter5] case class Branch[+A <% Ordered[A]](
      val elem: A,
      val children: List[PairingHeap[A]]) extends PairingHeap[A] {
    override def isEmpty = false
    override def findMin = elem
    override def deleteMin = mergePairs(children)
  }
}
