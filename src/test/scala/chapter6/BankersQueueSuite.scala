package chapter6

import org.scalatest.FunSuite

class BankersQueueSuite extends FunSuite {

  test("head of empty queue fails") {
    val q = BankersQueue.empty
    intercept[Exception] {
      q.head
    }
  }

  test("tail of empty queue fails") {
    val q = BankersQueue.empty
    intercept[Exception] {
      q.tail
    }
  }

  test("empty queue is empty") {
    val q = BankersQueue.empty
    assert(q.isEmpty)
  }

  test("empty queue has size zero") {
    val q = BankersQueue.empty
    expect(0) { q.size }
  }

  test("singleton queue is non-empty") {
    val q = BankersQueue(1)
    assert(!q.isEmpty)
  }

  test("singleton queue has size one") {
    val q = BankersQueue(1)
    expect(1) { q.size }
  }

  def invariant[A](q: BankersQueue[A]) =
    q.lenfront >= q.lenrear

  test("queue invariant") {
    var q = BankersQueue.empty[Int]
    assert(invariant(q))
    
    q = q :+ 1
    assert(invariant(q))
    
    q = q :+ 2
    assert(invariant(q))
    
    q = q :+ 3
    assert(invariant(q))
    
    expect(3) { q.size }
    
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
