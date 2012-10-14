package chapter7

class ScheduledBinomialHeap[+A <% Ordered[A]] private(
  private[chapter7] val digits: ScheduledBinomialHeap.Digits[A],
  private[chapter7] val schedule: ScheduledBinomialHeap.Schedule[A]) {
  import ScheduledBinomialHeap._

  def isEmpty = digits.isEmpty

  def insert[B >: A <% Ordered[B]](x: B): ScheduledBinomialHeap[B] = {
    val ds = insertTree(Tree(x, Nil), digits)
    new ScheduledBinomialHeap(ds, exec(exec(ds :: schedule)))
  }

  def merge[B >: A <% Ordered[B]](that: ScheduledBinomialHeap[B]): ScheduledBinomialHeap[B] = {
    val ds = normalize(mergeDigits(this.digits, that.digits))
    new ScheduledBinomialHeap(ds, Nil)
  }

  def findMin: A = removeMinTree(digits)._1.root

  def deleteMin: ScheduledBinomialHeap[A] = {
    val (Tree(x, c), ds) = removeMinTree(digits)
    val ds1 = mergeDigits(c.reverseMap(One(_)).toStream, ds)
    new ScheduledBinomialHeap(ds1, Nil)
  }

  override def toString() = digits.toString()
}

object ScheduledBinomialHeap {
  type Digits[+A] = Stream[Digit[A]]
  type Schedule[+A] = List[Digits[A]]

  def empty[A <% Ordered[A]]: ScheduledBinomialHeap[A] = new ScheduledBinomialHeap[A](Stream.empty, Nil)

  def apply[A <% Ordered[A]](as: A*): ScheduledBinomialHeap[A] = {
    var h = empty[A]
    for (a <- as) {
      h = h.insert(a)
    }
    h
  }

  private[chapter7] def insertTree[A <% Ordered[A]](t: Tree[A], digits: Digits[A]): Digits[A] =
    if (digits.isEmpty)
      Stream(One(t))
    else digits match {
      case Zero #:: ds => One(t) #:: ds
      case One(u) #:: ds => Zero #:: insertTree(t link u, ds)
    }

  private[chapter7] def mergeDigits[A <% Ordered[A]](digits1: Digits[A], digits2: Digits[A]): Digits[A] =
    if (digits2.isEmpty)
      digits1
    else if (digits1.isEmpty)
      digits2
    else (digits1, digits2) match {
      case (Zero #:: ds1, d    #:: ds2) => d #:: mergeDigits(ds1, ds2)
      case (d    #:: ds1, Zero #:: ds2) => d #:: mergeDigits(ds1, ds2)
      case (One(t1) #:: ds1, One(t2) #:: ds2) =>
        Zero #:: insertTree(t1 link t2, mergeDigits(ds1, ds2))
    }

  private[chapter7] def normalize[A](digits: Digits[A]): Digits[A] =
    if (digits.isEmpty)
      digits
    else {
      normalize(digits.tail)
      digits
    }

  private[chapter7] def exec[A](schedule: Schedule[A]): Schedule[A] = schedule match {
    case Nil => Nil
    case (Zero #:: job) :: sched => job :: sched
    case _ :: sched => sched
  }

  private[chapter7] def removeMinTree[A <% Ordered[A]](digits: Digits[A]): (Tree[A], Digits[A]) =
    if (digits.isEmpty)
      throw new Exception("ScheduledBinomialHeap.removeMinTree: empty heap")
    else digits match {
      case Zero #:: ds => {
        val (t1, ds1) = removeMinTree(ds)
        (t1, Zero #:: ds1)
      }
      case One(t) #:: ds =>
        if (ds.isEmpty)
          (t, Stream.empty)
        else {
          val (t1, ds1) = removeMinTree(ds)
          if (t.root <= t1.root)
            (t, Zero #:: ds)
          else
            (t1, One(t) #:: ds1)
        }
    }

  private[chapter7] case class Tree[+A <% Ordered[A]](
    val root: A,
    val subTrees: List[Tree[A]]) {

    def link[B >: A <% Ordered[B]](that: Tree[B]): Tree[B] =
      if (root <= that.root)
        Tree(root, that :: subTrees)
      else
        Tree(that.root, this :: that.subTrees)
  }

  private[chapter7] sealed abstract class Digit[+A]
  private[chapter7] case object Zero extends Digit[Nothing]
  private[chapter7] case class One[+A](tree: Tree[A]) extends Digit[A]
}
