package chapter5

import org.scalatest.FunSuite

class SplayHeapSuite extends FunSuite {
  
  test("empty heap is empty") {
    val h = SplayHeap.empty
    assert(h.isEmpty)
  }
  
  test("findMin of empty heap fails") {
    val h = SplayHeap.empty
    intercept[Exception] {
      h.findMin
    }
  }
  
  test("deleteMin of empty heap fails") {
    val h = SplayHeap.empty
    intercept[Exception] {
      h.deleteMin
    }
  }
  
  test("singleton heap is non-empty") {
    val h = SplayHeap(1)
    assert(!h.isEmpty)
  }
  
  test("findMin in range 1 to 10") {
    val h = SplayHeap(2,4,6,8,10,1,3,5,7,9)
    expect(1) { h.findMin }
  }
  
  test("findMin of merged heaps") {
    val h1 = SplayHeap(2 to 10 by 2: _*)
    val h2 = SplayHeap(1 to 10 by 2: _*)
    expect(1) {
      val h = h1 merge h2
      h.findMin
    }
  }
  
  import SplayHeap._
  def forallHeap[A](h: SplayHeap[A], p: (A) => Boolean): Boolean = h match {
    case Leaf => true
    case Branch(a, x, b) =>
      p(x) && forallHeap(a, p) && forallHeap(b, p)
  }
  
  def checkHeap[A <% Ordered[A]](h: SplayHeap[A]): Boolean = h match {
    case Leaf => true
    case Branch(a, x, b) =>
      forallHeap(a, (y:A) => x >= y) &&
      forallHeap(b, (y:A) => x <  y) &&
      checkHeap(a)
      checkHeap(b)
  }
  
  test("check heap structure") {
    var h = SplayHeap(2,4,6,8,10,1,3,5,7,9)
    assert(checkHeap(h))
    h = SplayHeap(1 to 10: _*)
    assert(checkHeap(h))
    h = SplayHeap(1 to 10 reverse: _*)
    assert(checkHeap(h))
  }
}
