package chapter5

class Deque[+A] private (
  private[chapter5] val front: List[A],
  private[chapter5] val rear: List[A]) {
  
  def isEmpty: Boolean = front.isEmpty && rear.isEmpty
  
  def head: A =
    front.headOption getOrElse {
      rear.headOption getOrElse {
        throw new Exception("Deque.head: empty deque")
      }
    }
  
  def last: A =
    rear.headOption getOrElse {
      front.headOption getOrElse {
        throw new Exception("Deque.last: empty deque")
      }
    }
  
  private def check[B](front: List[B], rear: List[B]): Deque[B] =
    if (front.isEmpty) {
      val l = rear.length
      if (l > 1) {
        val (r, f) = rear splitAt (l / 2)
        new Deque(f.reverse, r)
      } else new Deque(front, rear)
    } else if (rear.isEmpty) {
      val l = front.length
      if (l > 1) {
        val (f, r) = front splitAt (l / 2)
        new Deque(f, r.reverse)
      } else new Deque(front, rear)
    } else new Deque(front, rear)
  
  def tail: Deque[A] = front match {
    case Nil =>
      if (rear.isEmpty)
        throw new Exception("Deque.tail: empty deque")
      else
        Deque.empty[A]
    case h :: t => check(t, rear)
  }
  
  def init: Deque[A] = rear match {
    case Nil =>
      if (front.isEmpty)
        throw new Exception("Deque.init: empty deque")
      else
        Deque.empty[A]
    case h :: t => check(front, t)
  }
  
  def enqueue[B >: A](x: B): Deque[B] = check(front, x :: rear)
  
  def :+[B >: A](x: B): Deque[B] = enqueue(x)
  
  def +:[B >: A](x: B): Deque[B] = check(x :: front, rear)
}

object Deque {
  def empty[A]: Deque[A] = new Deque(Nil, Nil)
  
  def apply[A](as: A*): Deque[A] = {
    var q = empty[A]
    for (a <- as) {
      q = q.enqueue(a)
    }
    q
  }
}
