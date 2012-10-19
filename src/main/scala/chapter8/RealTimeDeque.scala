package chapter8

class RealTimeDeque[+A](
  private[chapter8] val lenf:   Int,
  private[chapter8] val front:  Stream[A],
  private[chapter8] val schedf: Stream[A],
  private[chapter8] val lenr:   Int,
  private[chapter8] val rear:   Stream[A],
  private[chapter8] val schedr: Stream[A]) {
  import RealTimeDeque._

  def isEmpty: Boolean = lenf + lenr == 0

  def head: A =
    front.headOption getOrElse {
      rear.headOption getOrElse {
        throw new Exception("RealTimeDeque.head: empty deque")
      }
    }

  def last: A =
    rear.headOption getOrElse {
      front.headOption getOrElse {
        throw new Exception("RealTimeDeque.last: empty deque")
      }
    }

  def tail: RealTimeDeque[A] =
    if (front.isEmpty)
      if (rear.isEmpty)
        throw new Exception("RealTimeDeque.tail: empty deque")
      else
        empty
    else
      check(lenf - 1, front.tail, exec2(schedf), lenr, rear, exec2(schedr))

  def init: RealTimeDeque[A] =
    if (rear.isEmpty)
      if (front.isEmpty)
        throw new Exception("RealTimeDeque.init: empty deque")
      else
        empty
    else
      check(lenf, front, exec2(schedf), lenr - 1, rear.tail, exec2(schedr))

  def enqueue[B >: A](x: B): RealTimeDeque[B] = check(lenf, front, exec1(schedf), lenr + 1, x +: rear, exec1(schedr))

  def :+[B >: A](x: B): RealTimeDeque[B] = enqueue(x)

  def +:[B >: A](x: B): RealTimeDeque[B] = check(lenf + 1, x +: front, exec1(schedf), lenr, rear, exec1(schedr))
}

object RealTimeDeque {
  def empty[A]: RealTimeDeque[A] = new RealTimeDeque(0, Stream.empty, Stream.empty, 0, Stream.empty, Stream.empty)

  def apply[A](as: A*): RealTimeDeque[A] = {
    var q = empty[A]
    for (a <- as) {
      q = q.enqueue(a)
    }
    q
  }

  private[chapter8] val c = 2

  private def exec1[A](s: Stream[A]): Stream[A] = s match {
    case _ #:: t => t
    case _       => s
  }

  private def exec2[A](s: Stream[A]): Stream[A] = exec1(exec1(s))

  private def rotateRev[A](front: Stream[A], rear: Stream[A], accum: Stream[A]): Stream[A] =
    if (front.isEmpty)
      rear.reverse ++ accum
    else
      front.head #:: rotateRev(front.tail, rear.drop(c), rear.take(c).reverse ++ accum)

  private def rotateDrop[A](front: Stream[A], j: Int, rear: Stream[A]): Stream[A] =
    if (j < c)
      rotateRev(front, rear.drop(j), Stream.empty)
    else
      front.head #:: rotateDrop(front.tail, j - c, rear.drop(c))

  private[chapter8] def check[A](
    lenf:   Int,
    front:  Stream[A],
    schedf: Stream[A],
    lenr:   Int,
    rear:   Stream[A],
    schedr: Stream[A]): RealTimeDeque[A] =
    if (lenf > c * lenr + 1) {
      val i = (lenf + lenr) / 2
      val j = lenf + lenr - i
      val nfront = front.take(i)
      val nrear = rotateDrop(rear, i, front)
      new RealTimeDeque(i, nfront, nfront, j, nrear, nrear)
    } else if (lenr > c * lenf + 1) {
      val j = (lenf + lenr) / 2
      val i = lenf + lenr - j
      val nrear = rear.take(j)
      val nfront = rotateDrop(front, j, rear)
      new RealTimeDeque(i, nfront, nfront, j, nrear, nrear)
    } else
      new RealTimeDeque(lenf, front, schedf, lenr, rear, schedr)
}
