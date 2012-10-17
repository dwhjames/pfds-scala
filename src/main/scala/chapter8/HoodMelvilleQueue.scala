package chapter8

class HoodMelvilleQueue[+A] private(
  private[chapter8] val lenf:  Int,
  private[chapter8] val front: List[A],
  private[chapter8] val state: HoodMelvilleQueue.RotationState[A],
  private[chapter8] val lenr:  Int,
  private[chapter8] val rear:  List[A]) {
  import HoodMelvilleQueue._

  def copyWith[B >: A]
              (lenf:  Int              = lenf,
               front: List[B]          = front,
               state: RotationState[B] = state,
               lenr:  Int              = lenr,
               rear:  List[B]          = rear): HoodMelvilleQueue[B] = {
    new HoodMelvilleQueue(lenf, front, state, lenr, rear)
  }

  def isEmpty: Boolean = lenf == 0

  private def exec: HoodMelvilleQueue[A] = state.exec match {
    case Done(newf) => this.copyWith(front = newf, state = Idle)
    case newState   => this.copyWith(state = newState)
  }

  private def check: HoodMelvilleQueue[A] =
    if (lenr <= lenf)
      this.exec
    else {
      val newState = Reversing(0, front, Nil, rear, Nil).exec
      new HoodMelvilleQueue(lenf + lenr, front, newState, 0, Nil).exec
    }

  def enqueue[B >: A](x: B): HoodMelvilleQueue[B] =
    this.copyWith(lenr = lenr + 1, rear = x :: rear).check

  def :+[B >: A](x: B) = enqueue(x)

  def head: A =
    front.headOption getOrElse { throw new Exception("HoodMelvilleQueue.head: empty queue") }

  def tail: HoodMelvilleQueue[A] =
    if (front.isEmpty)
      throw new Exception("RealTimeQueue.tail: empty queue")
    else
      this.copyWith(lenf = lenf - 1, front = front.tail, state = state.invalidate).check

  override def toString = (lenf, front, state, lenr, rear).toString

}

object HoodMelvilleQueue {
  def empty[A]: HoodMelvilleQueue[A] = new HoodMelvilleQueue(0, Nil, Idle, 0, Nil)

  def apply[A](as: A*): HoodMelvilleQueue[A] = {
    var q = empty[A]
    for (a <- as) {
      q = q.enqueue(a)
    }
    q
  }

  sealed abstract class RotationState[+A] {
    def exec: RotationState[A]
    def invalidate: RotationState[A]
  }
  case object Idle extends RotationState[Nothing] {
    override def exec = this
    override def invalidate = this
  }
  final case class Reversing[+A](ok: Int, f: List[A], newf: List[A], r: List[A], newr: List[A]) extends RotationState[A] {
    override def exec = f match {
      case Nil =>
        Appending(ok, newf, r.head :: newr)
      case x :: f =>
        Reversing(ok + 1, f, x :: newf, r.tail, r.head :: newr)
    }
    override def invalidate = Reversing(ok - 1, f, newf, r, newr)
  }
  final case class Appending[+A](ok: Int, f: List[A], r: List[A]) extends RotationState[A] {
    override def exec =
      if (ok == 0)
        Done(r)
      else
        Appending(ok - 1, f.tail, f.head :: r)
    override def invalidate =
      if (ok == 0)
        Done(r.tail)
      else
        Appending(ok - 1, f, r)
  }
  final case class Done[+A](newf: List[A]) extends RotationState[A] {
    override def exec = this
    override def invalidate = this
  }
}
