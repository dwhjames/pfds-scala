package chapter5

import org.scalatest.FunSuite
 
class DequeSuite extends FunSuite {
  
  test("head of empty queue fails") {
    val q = Deque.empty
    intercept[Exception] {
      q.head
    }
  }
  
  test("last of empty queue fails") {
    val q = Deque.empty
    intercept[Exception] {
      q.last
    }
  }
  
  test("tail of empty queue fails") {
    val q = Deque.empty
    intercept[Exception] {
      q.tail
    }
  }
  
  test("init of empty queue fails") {
    val q = Deque.empty
    intercept[Exception] {
      q.init
    }
  }
  
  test("empty queue is empty") {
    val q = Deque.empty
    assert(q.isEmpty)
  }
  
  test("singleton queue is non-empty") {
    var q = Deque.empty[Int]
    q = q :+ 1
    assert(!q.isEmpty)
    q = Deque.empty
    q = 1 +: q
    assert(!q.isEmpty)
  }
  
  test("head after cons") {
    var q = Deque.empty[Int]
    q = 1 +: q
    expect(1) { q.head }
  }
  
  test("head after snoc") {
    var q = Deque.empty[Int]
    q = q :+ 1
    expect(1) { q.head }
  }
  
  test("last after cons") {
    var q = Deque.empty[Int]
    q = 1 +: q
    expect(1) { q.last }
  }
  
  test("last after snoc") {
    var q = Deque.empty[Int]
    q = q :+ 1
    expect(1) { q.last }
  }
  
  def invariant[A](q: Deque[A]) = {
    val l = q.front.length + q.rear.length
    (l < 2) || (!q.front.isEmpty && !q.rear.isEmpty)
  }
  
  test("queue invariant") {
    var q = Deque.empty[Int]
    assert(invariant(q))
    
    q = q :+ 4
    assert(invariant(q))
    
    q = 3 +: q
    assert(invariant(q))
    
    q = q :+ 5
    assert(invariant(q))
    
    q = 2 +: q
    assert(invariant(q))
    
    q = q :+ 6
    assert(invariant(q))
    
    q = 1 +: q
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
    
    expect(6) { q.last }
    
    q = q.init
    assert(invariant(q))
    
    expect(5) { q.last }
    
    q = q.init
    assert(invariant(q))
    
    expect(4) { q.last }
    
    q = q.init
    assert(invariant(q))
    
    assert(q.isEmpty)
  }
}
