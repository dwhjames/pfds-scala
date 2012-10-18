package chapter8

class BankersDeque[+A](
  private[chapter8] val lenf:  Int,
  private[chapter8] val front: Stream[A],
  private[chapter8] val lenr:  Int,
  private[chapter8] val rear:  Stream[A]) {
  import BankersDeque._

  def isEmpty: Boolean = lenf + lenr == 0

  def head: A =
    front.headOption getOrElse {
      rear.headOption getOrElse {
        throw new Exception("BankersDeque.head: empty deque")
      }
    }

  def last: A =
    rear.headOption getOrElse {
      front.headOption getOrElse {
        throw new Exception("BankersDeque.last: empty deque")
      }
    }

  def tail: BankersDeque[A] =
    if (front.isEmpty)
      if (rear.isEmpty)
        throw new Exception("BankersDeque.tail: empty deque")
      else
        empty
    else
      check(lenf - 1, front.tail, lenr, rear)

  def init: BankersDeque[A] =
    if (rear.isEmpty)
      if (front.isEmpty)
        throw new Exception("BankersDeque.init: empty deque")
      else
        empty
    else
      check(lenf, front, lenr - 1, rear.tail)

  def enqueue[B >: A](x: B): BankersDeque[B] = check(lenf, front, lenr + 1, x +: rear)

  def :+[B >: A](x: B): BankersDeque[B] = enqueue(x)

  def +:[B >: A](x: B): BankersDeque[B] = check(lenf + 1, x +: front, lenr, rear)
}

object BankersDeque {
  def empty[A]: BankersDeque[A] = new BankersDeque(0, Stream.empty, 0, Stream.empty)

  def apply[A](as: A*): BankersDeque[A] = {
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

  private[chapter8] val c = 2

  private[chapter8] def check[A](lenf: Int, front: Stream[A], lenr: Int, rear: Stream[A]): BankersDeque[A] =
    if (lenf > c * lenr + 1) {
      val i = (lenf + lenr) / 2
      val j = lenf + lenr - i
      val (f1, f2) = front splitAt i
      new BankersDeque(i, f1, j, appRev(rear, f2))
    } else if (lenr > c * lenf + 1) {
      val j = (lenf + lenr) / 2
      val i = lenf + lenr - j
      val (r1, r2) = rear splitAt i
      new BankersDeque(i, appRev(front, r2), j, r1)
    } else
      new BankersDeque(lenf, front, lenr, rear)
}
