package chapter3

class BinomialHeap[+A <% Ordered[A]] private(private[chapter3] val heap: List[BinomialHeap.Tree[A]]) {
  import BinomialHeap._
  
  def isEmpty = heap.isEmpty
  
  def insert[B >: A <% Ordered[B]](x: B): BinomialHeap[B] =
    new BinomialHeap(insertTree(Tree(0,x,Nil), heap))
  
  def merge[B >: A <% Ordered[B]](that: BinomialHeap[B]): BinomialHeap[B] =
    new BinomialHeap(mergeHeaps(this.heap, that.heap))
  
  def findMin: A = removeMinTree(this.heap)._1.root
  
  def deleteMin: BinomialHeap[A] = {
    val (Tree(_, x, xs), ys) = removeMinTree(this.heap)
    new BinomialHeap(mergeHeaps(xs.reverse, ys))
  }
  
  override def toString() = heap.toString()
}

object BinomialHeap {
  type Heap[+A] = List[Tree[A]]
  
  def empty[A <% Ordered[A]]: BinomialHeap[A] = new BinomialHeap[A](Nil)
  
  def apply[A <% Ordered[A]](as: A*): BinomialHeap[A] = {
    var h: BinomialHeap[A] = new BinomialHeap[A](Nil)
    for (a <- as) {
      h = h.insert(a)
    }
    h
  }
  
  private def insertTree[A <% Ordered[A]](t: Tree[A], l: Heap[A]): Heap[A] = l match {
    case Nil => List(t)
    case x :: xs =>
      if (t.rank < x.rank) t :: l else insertTree(t link x, xs)
  }
  
  private def mergeHeaps[A <% Ordered[A]](h1: Heap[A], h2: Heap[A]): Heap[A] = (h1, h2) match {
    case (_, Nil) => h1
    case (Nil, _) => h2
    case (x :: xs, y :: ys) =>
      if (x.rank < y.rank)
        x :: mergeHeaps(xs, h2)
      else if (y.rank < x.rank)
        y :: mergeHeaps(h1, ys)
      else
        insertTree(x link y, mergeHeaps(xs, ys))
  }
  
  private def removeMinTree[A <% Ordered[A]](h: Heap[A]): (Tree[A], Heap[A]) = h match {
    case Nil => throw new Exception("BinomialHeap.removeMinTree: empty heap")
    case t :: Nil => (t, Nil)
    case x :: xs => {
      val (y, ys) = removeMinTree(xs)
      if (x.root <= y.root) (x, xs) else (y, x :: ys)
    }
  }
  
  private[chapter3] case class Tree[+A <% Ordered[A]](
    val rank: Int,
    val root: A,
    val subTrees: List[Tree[A]]) {
  
    def link[B >: A <% Ordered[B]](that: Tree[B]): Tree[B] =
      if (root <= that.root)
        Tree(rank + 1, root, that :: subTrees)
      else
        Tree(rank + 1, that.root, this :: that.subTrees)
  }
}

