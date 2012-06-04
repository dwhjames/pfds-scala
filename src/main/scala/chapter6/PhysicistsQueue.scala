package chapter6

class PhysicistsQueue[+A] private (
  private[chapter6] val working: List[A],
  private[chapter6] val lenfront: Int,
  f: => List[A],
  private[chapter6] val lenrear: Int,
  private[chapter6] val rear: List[A]) {
  import PhysicistsQueue._

  private[chapter6] lazy val front: List[A] = f

  def isEmpty: Boolean = lenfront == 0

  def size: Int = lenfront + lenrear

  def head: A =
    working.headOption getOrElse { throw new Exception("PhysicistsQueue.head: empty queue") }

  def tail: PhysicistsQueue[A] = working match {
    case Nil    => throw new Exception("PhysicistsQueue.tail: empty queue")
    case _ :: w => check(w, lenfront - 1, front.tail, lenrear, rear)
  }

  def enqueue[B >: A](x: B): PhysicistsQueue[B] =
    check(working, lenfront, front, lenrear+1, x :: rear)

  def :+[B >: A](x: B): PhysicistsQueue[B] = enqueue(x)
}

object PhysicistsQueue {
  def empty[A]: PhysicistsQueue[A] = new PhysicistsQueue(Nil, 0, Nil, 0, Nil)

  def apply[A](as: A*): PhysicistsQueue[A] = {
    var q = empty[A]
    for (a <- as) {
      q = q.enqueue(a)
    }
    q
  }

  private def checkw[A] (w: List[A], lenf: Int, f: => List[A], lenr: Int, r: List[A]): PhysicistsQueue[A] =
    if (w.isEmpty) {
      lazy val g = f
      new PhysicistsQueue(g, lenf, g, lenr, r)
    } else
      new PhysicistsQueue(w, lenf, f, lenr, r)

  private def check[A](w: List[A], lenf: Int, f: => List[A], lenr: Int, r: List[A]): PhysicistsQueue[A] =
    if (lenf >= lenr)
      checkw(w, lenf, f, lenr, r)
    else {
      val g = f
      checkw(g, lenf+lenr, g ++ r.reverse, 0, Nil)
    }
}
