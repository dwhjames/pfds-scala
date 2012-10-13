package chapter6

class BankersQueue[+A] private (
  private[chapter6] val lenfront: Int,
  private[chapter6] val front: Stream[A],
  private[chapter6] val lenrear: Int,
  private[chapter6] val rear: Stream[A]) {
  import BankersQueue._

  def isEmpty: Boolean = lenfront == 0

  def size: Int = lenfront + lenrear

  def enqueue[B >: A](x: B): BankersQueue[B] =
    check(lenfront, front, lenrear+1, x +: rear)

  def :+[B >: A](x: B) = enqueue(x)

  def head: A =
    front.headOption getOrElse { throw new Exception("BankersQueue.head: empty queue") }

  def tail: BankersQueue[A] =
    if (lenfront > 0)
      check(lenfront-1, front.tail, lenrear, rear)
    else
      throw new Exception("BankersQueue.tail: empty queue")
}

object BankersQueue {
  def empty[A]: BankersQueue[A] = new BankersQueue(0, Stream.empty, 0, Stream.empty)

  def apply[A](as: A*): BankersQueue[A] = {
    var q = empty[A]
    for (a <- as) {
      q = q.enqueue(a)
    }
    q
  }

  private def appRev[A](s: Stream[A], t: Stream[A]): Stream[A] =
    if (s.isEmpty)
      t.reverse
    else
      s.head #:: appRev(s.tail, t)

  private[chapter6] def check[A](lenf: Int, f: Stream[A],
                                 lenr: Int, r: Stream[A]): BankersQueue[A] = {
    if (lenr <= lenf)
      new BankersQueue(lenf, f, lenr, r)
    else
      new BankersQueue(lenf + lenr, appRev(f, r), 0, Stream.empty)
  }
}
