package chapter5

class BatchedQueue[+A] private (
  private[chapter5] val front: List[A],
  private[chapter5] val rear: List[A]) {
  
  def isEmpty: Boolean = front.isEmpty
  
  def head: A =
    front.headOption getOrElse { throw new Exception("BatchedQueue.head: empty queue") }
  
  // ensure front.isEmpty implies rear.isEmpty
  private def checkFront[B](front: List[B], rear: List[B]): BatchedQueue[B] =
    if (front.isEmpty)
      new BatchedQueue(rear.reverse, Nil)
    else
      new BatchedQueue(front, rear)
  
  def tail: BatchedQueue[A] = front match {
    case Nil    => throw new Exception("BatchedQueue.tail: empty queue")
    case h :: t => checkFront(t, rear)
  }
  
  def enqueue[B >: A](x: B): BatchedQueue[B] = checkFront(front, x :: rear)
  
  def :+[B >: A](x: B): BatchedQueue[B] = enqueue(x)
}

object BatchedQueue {
  def empty[A]: BatchedQueue[A] = new BatchedQueue(Nil, Nil)
  
  def apply[A](as: A*): BatchedQueue[A] = {
    var q = empty[A]
    for (a <- as) {
      q = q.enqueue(a)
    }
    q
  }
}
