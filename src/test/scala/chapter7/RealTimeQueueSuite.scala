package chapter7

import org.scalatest.FunSuite

class RealTimeQueueSuite extends FunSuite {

  test("head of empty queue fails") {
    val q = RealTimeQueue.empty
    intercept[Exception] {
      q.head
    }
  }

  test("tail of empty queue fails") {
    val q = RealTimeQueue.empty
    intercept[Exception] {
      q.tail
    }
  }

  test("empty queue is empty") {
    val q = RealTimeQueue.empty
    assert(q.isEmpty)
  }

  test("singleton queue is non-empty") {
    val q = RealTimeQueue(1)
    assert(!q.isEmpty)
  }

  def invariant[A](q: RealTimeQueue[A]) =
    q.schedule.size == q.front.size - q.rear.size

  test("queue invariant") {
    var q = RealTimeQueue.empty[Int]
    assert(invariant(q))

    q = q :+ 1
    assert(invariant(q))

    q = q :+ 2
    assert(invariant(q))

    q = q :+ 3
    assert(invariant(q))

    expect(1) { q.head }

    q = q.tail
    assert(invariant(q))

    expect(2) { q.head }

    q = q.tail
    assert(invariant(q))

    expect(3) { q.head }

    q = q.tail
    assert(invariant(q))
    assert(q.isEmpty)
  }
}
