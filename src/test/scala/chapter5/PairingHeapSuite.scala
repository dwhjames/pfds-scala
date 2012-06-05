package chapter5

import org.scalatest.FunSuite

class PairingHeapSuite extends FunSuite {
  
  test("empty heap is empty") {
    val h = PairingHeap.empty
    assert(h.isEmpty)
  }
  
  test("findMin of empty heap fails") {
    val h = PairingHeap.empty
    intercept[Exception] {
      h.findMin
    }
  }
  
  test("deleteMin of empty heap fails") {
    val h = PairingHeap.empty
    intercept[Exception] {
      h.deleteMin
    }
  }
  
  test("singleton heap is non-empty") {
    val h = PairingHeap(1)
    assert(!h.isEmpty)
  }
  
  test("findMin in range 1 to 10") {
    val h = PairingHeap(2,4,6,8,10,1,3,5,7,9)
    expect(1) { h.findMin }
  }
  
  import PairingHeap._
  def checkHeap[A <% Ordered[A]](h: PairingHeap[A]): Boolean = {
    def checkHeapH(h: PairingHeap[A], p: (A) => Boolean): Boolean = h match {
      case Leaf => true
      case Branch(x, xs) =>
        p(x) && xs.forall(checkHeapH(_, (y:A) => x <= y))
    }
    checkHeapH(h, (x:A) => true)
  }
  
  test("check heap structure") {
    var h = PairingHeap(2,4,6,8,10,1,3,5,7,9)
    assert(checkHeap(h))
    h = PairingHeap(1 to 10: _*)
    assert(checkHeap(h))
    h = PairingHeap(1 to 10 reverse: _*)
    assert(checkHeap(h))
  }
}
