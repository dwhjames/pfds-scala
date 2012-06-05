package chapter3

import org.scalatest.FunSuite
 
class WeightBiasedLeftistHeapSuite extends FunSuite {
  
  test("empty heap is empty") {
    val h = WeightBiasedLeftistHeap.empty
    assert(h.isEmpty)
  }
  
  test("empty heap has size 0") {
    val h = WeightBiasedLeftistHeap.empty
    expect(0) { h.size }
  }
  
  test("findMin of empty heap fails") {
    val h = WeightBiasedLeftistHeap.empty
    intercept[Exception] {
      h.findMin
    }
  }
  
  test("deleteMin of empty heap fails") {
    val h = WeightBiasedLeftistHeap.empty
    intercept[Exception] {
      h.deleteMin
    }
  }
  
  test("singleton heap is non-empty") {
    val h = WeightBiasedLeftistHeap(1)
    assert(!h.isEmpty)
  }
  
  test("singleton heap has size 1") {
    val h = WeightBiasedLeftistHeap(1)
    expect(1) { h.size }
  }
  
  test("findMin in range 1 to 10") {
    val h = WeightBiasedLeftistHeap(2,4,6,8,10,1,3,5,7,9)
    expect(1) { h.findMin }
  }

  import WeightBiasedLeftistHeap._
  def checkSize[A](h: WeightBiasedLeftistHeap[A]): Boolean = {
    def checkSizeH(h: WeightBiasedLeftistHeap[A]): (Int, Boolean) = h match {
      case Leaf => (0, true)
      case Branch(s, _, l, r) => {
        val (ls, lb) = checkSizeH(l)
        val (rs, rb) = checkSizeH(r)
        val s2 = 1+ls+rs
        (s2, lb && rb && (s == s2))
      }
    }
    checkSizeH(h)._2
  }

  test("check size of empty heap") {
    val h = WeightBiasedLeftistHeap.empty
    assert(checkSize(h))
  }
  
  test("check size of singleton heap") {
    val h = WeightBiasedLeftistHeap(1)
    assert(checkSize(h))
  }
  
  test("check sizes of heap with 1 to 10") {
    val h = WeightBiasedLeftistHeap(2,4,6,8,10,1,3,5,7,9)
    assert(checkSize(h))
  }
  
  def checkLeftist[A](h: WeightBiasedLeftistHeap[A]): Boolean = {
    h match {
      case Leaf => true
      case Branch(_, _, l, r) =>
        (l.size >= r.size) &&
        checkLeftist(l) &&
        checkLeftist(r)
    }
  }
  
  test("check leftist property with 1 to 10") {
    val h = WeightBiasedLeftistHeap(2,4,6,8,10,1,3,5,7,9)
    assert(checkLeftist(h))
  }
}