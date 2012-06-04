package chapter6

class BankersQueue[+A] private (
  private[chapter6] val lenfront: Int,
  private[chapter6] val front: Stream[A],
  private[chapter6] val lenrear: Int,
  private[chapter6] val rear: Stream[A]) {

  def isEmpty: Boolean = lenfront == 0

  def size: Int = lenfront + lenrear

  private[chapter6] def check[B](lenf: Int, f: Stream[B],
                                 lenr: Int, r: Stream[B]): BankersQueue[B] = {
    if (lenr <= lenf)
      new BankersQueue(lenf, f, lenr, r)
    else
      new BankersQueue(lenf + lenr, f ++ r.reverse, 0, Stream.empty)
  }

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
}
