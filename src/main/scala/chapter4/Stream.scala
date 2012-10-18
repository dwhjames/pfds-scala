package chapter4

sealed abstract class Stream[+A] {
  import Stream._
  
  def head: A
  
  def tail: Stream[A]
  
  def ++[B >: A](that: => Stream[B]): Stream[B] = this match {
    case SNil => that
    case _    => SCons(head, tail ++ that)
  }
  
  def take(n: Int): Stream[A] = (n, this) match {
    case (_, SNil)        => this
    case (n, _) if n <= 0 => SNil
    case (1, _)           => SCons(head, SNil)
    case _                => SCons(head, tail.take(n-1))
  }
  
  def drop(n: Int): Stream[A] = (n, this) match {
    case (_, SNil)        => this
    case (n, _) if n <= 0 => this
    case _                => tail.drop(n-1)
  }
  
  def reverse: Stream[A] = {
    def reverseAcc(s: Stream[A], r: Stream[A]): Stream[A] = s match {
      case SNil        => r
      case SCons(h, t) => reverseAcc(t, SCons(h, r))
    }
    reverseAcc(this, SNil)
  }
}

object Stream {
  
  def empty[A]: Stream[A] = SNil
  
  case object SNil extends Stream[Nothing] {
    override def head = throw new Exception("Stream.head: nil stream")
    override def tail = throw new Exception("Stream.tail: nil stream")
  }
  
  final class SCons[+A](override val head: A, tl: => Stream[A]) extends Stream[A] {
    override lazy val tail = tl
  }
  
  object SCons {
    def apply[A](hd: A, tl: => Stream[A]) = new SCons(hd, tl)
    
    def unapply[A](s: Stream[A]): Option[(A, Stream[A])] = s match {
      case SNil => None
      case _    => Some((s.head, s.tail))
    }
  }
}