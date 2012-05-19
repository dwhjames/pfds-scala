package chapter5

import org.scalatest.FunSuite
 
class BatchedQueueSuite extends FunSuite {
  
  test("head of empty queue fails") {
    val q = BatchedQueue.empty
    intercept[Exception] {
      q.head
    }
  }
  
  test("tail of empty queue fails") {
    val q = BatchedQueue.empty
    intercept[Exception] {
      q.tail
    }
  }
  
  test("empty queue is empty") {
    val q = BatchedQueue.empty
    assert(q.isEmpty)
  }
  
  test("singleton queue is non-empty") {
    val q = BatchedQueue(1)
    assert(!q.isEmpty)
  }
  
  def invariant[A](q: BatchedQueue[A]) =
    // ensure front.isEmpty implies rear.isEmpty
    (!q.front.isEmpty) || q.rear.isEmpty
  
  test("queue invariant") {
    var q: BatchedQueue[Int] = BatchedQueue.empty
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
