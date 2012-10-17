package chapter8

import org.scalatest.FunSuite

class HoodMelvilleQueueSuite extends FunSuite {

  test("head of empty queue fails") {
    val q = HoodMelvilleQueue.empty
    intercept[Exception] {
      q.head
    }
  }

  test("tail of empty queue fails") {
    val q = HoodMelvilleQueue.empty
    intercept[Exception] {
      q.tail
    }
  }

  test("empty queue is empty") {
    val q = HoodMelvilleQueue.empty
    assert(q.isEmpty)
    assert(q.lenf == 0)
    assert(q.lenr == 0)
  }

  test("singleton queue is non-empty") {
    val q = HoodMelvilleQueue(1)
    assert(!q.isEmpty)
  }

  test("enqueue lengthens rear") {
    var q = HoodMelvilleQueue(1)
    assert(q.lenf === 1)
    assert(q.lenr === 0)
    q = q :+ 2
    assert(q.lenf === 1)
    assert(q.lenr === 1)
  }
  
  test("tail shortens front") {
    var q = HoodMelvilleQueue(1, 2, 3)
    assert(q.lenf === 3)
    assert(q.lenr === 0)
    q = q.tail
    assert(q.lenf === 2)
    assert(q.lenr === 0)
  }

  def invariant[A](q: HoodMelvilleQueue[A]) =
    q.lenf >= q.lenr

  test("queue invariant") {
    var q = HoodMelvilleQueue.empty[Int]
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
