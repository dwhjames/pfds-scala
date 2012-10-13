package chapter7

class RealTimeQueue[+A] private (
  private[chapter7] val front: Stream[A],
  private[chapter7] val rear: List[A],
  private[chapter7] val schedule: Stream[A]) {
  import RealTimeQueue._

  def isEmpty: Boolean = front.isEmpty

  def enqueue[B >: A](x: B): RealTimeQueue[B] =
    exec(front, x :: rear, schedule)

  def :+[B >: A](x: B) = enqueue(x)

  def head: A =
    front.headOption getOrElse { throw new Exception("RealTimeQueue.head: empty queue") }

  def tail: RealTimeQueue[A] =
    if (front.isEmpty)
      throw new Exception("RealTimeQueue.tail: empty queue")
    else
      exec(front.tail, rear, schedule)
}

object RealTimeQueue {
  def empty[A]: RealTimeQueue[A] = new RealTimeQueue(Stream.empty, Nil, Stream.empty)

  def apply[A](as: A*): RealTimeQueue[A] = {
    var q = empty[A]
    for (a <- as) {
      q = q.enqueue(a)
    }
    q
  }

  private[chapter7] def rotate[A](front: Stream[A], rear: List[A], schedule: Stream[A]): Stream[A] =
    if (front.isEmpty)
      rear.head #:: schedule
    else
      front.head #:: rotate(front.tail, rear.tail, rear.head #:: schedule)

  private[chapter7] def exec[A](front: Stream[A], rear: List[A], schedule: Stream[A]): RealTimeQueue[A] =
    if (schedule.isEmpty) {
      val newFront = rotate(front, rear, Stream.empty)
      new RealTimeQueue(newFront, Nil, newFront)
    } else
      new RealTimeQueue(front, rear, schedule.tail)
}
