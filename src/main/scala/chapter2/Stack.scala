package chapter2

sealed abstract class Stack[+A] {
  import Stack._
  def isEmpty: Boolean
  def head: A
  def tail: Stack[A]
  
  def ++[B >: A](that: Stack[B]): Stack[B] = this match {
    case Nil => that
    case Cons(hd, tl) => Cons(hd, tl ++ that)
  }
  
  def update[B >: A](i: Int, y: B): Stack[B] = (this, i) match {
    case (Nil, _) => throw new Exception("Stack.update: index out of bounds")
    case (Cons(_,xs), 0) => Cons(y,xs)
    case (Cons(x,xs), i) => Cons(x, xs.update(i-1, y))
  }
}

object Stack {
  def empty[A]: Stack[A] = Nil
  def apply[A](as: A*): Stack[A] = {
    var s: Stack[A] = Nil
    for (a <- as.reverse) {
      s = Cons(a, s)
    }
    s
  }
  
  case object Nil extends Stack[Nothing] {
    override def isEmpty = true
  
    override def head: Nothing =
      throw new Exception("Nil.head: empty stack")
  
    override def tail: Stack[Nothing] =
      throw new Exception("Nil.tail: empty stack")
  }

  final case class Cons[A](
      override val head: A,
      override val tail: Stack[A]) extends Stack[A] {
    override def isEmpty = false
  }
}