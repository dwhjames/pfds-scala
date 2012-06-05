package chapter6

import org.scalatest.FunSuite

class LazyPairingHeapSuite extends FunSuite {

  test("empty heap is empty") {
    val h = LazyPairingHeap.empty
    assert(h.isEmpty)
  }

  test("findMin of empty heap fails") {
    val h = LazyPairingHeap.empty
    intercept[Exception] {
      h.findMin
    }
  }

  test("deleteMin of empty heap fails") {
    val h = LazyPairingHeap.empty
    intercept[Exception] {
      h.deleteMin
    }
  }

  test("singleton heap is non-empty") {
    val h = LazyPairingHeap(1)
    assert(!h.isEmpty)
  }

  test("findMin in range 1 to 10") {
    val h = LazyPairingHeap(2,4,6,8,10,1,3,5,7,9)
    expect(1) { h.findMin }
  }

  import LazyPairingHeap._
  def checkHeap[A <% Ordered[A]](h: LazyPairingHeap[A]): Boolean = {
    def checkHeapH(h: LazyPairingHeap[A], p: (A) => Boolean): Boolean =
      if (h.isEmpty)
        true
      else {
        val b = h.asInstanceOf[Branch[A]]
        p(b.elem) &&
          checkHeapH(b.left, (x:A) => x >= b.elem) &&
          checkHeapH(b.right, (x:A) => x >= b.elem)
      }
    checkHeapH(h, (x:A) => true)
  }

  test("check heap structure") {
    var h = LazyPairingHeap(2,4,6,8,10,1,3,5,7,9)
    assert(checkHeap(h))
    h = LazyPairingHeap(1 to 10: _*)
    assert(checkHeap(h))
    h = LazyPairingHeap(1 to 10 reverse: _*)
    assert(checkHeap(h))
  }
}
