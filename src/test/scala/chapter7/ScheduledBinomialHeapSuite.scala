package chapter7

import org.scalatest.FunSuite

class ScheduledBinomialHeapSuite extends FunSuite {

  test("empty heap is empty") {
    val h = ScheduledBinomialHeap.empty[Int]
    assert(h.isEmpty)
  }

  test("findMin of empty heap fails") {
    val h = ScheduledBinomialHeap.empty[Int]
    intercept[Exception] {
      h.findMin
    }
  }

  test("deleteMin of empty heap fails") {
    val h = ScheduledBinomialHeap.empty[Int]
    intercept[Exception] {
      h.deleteMin
    }
  }

  test("singleton heap is non-empty") {
    val h = ScheduledBinomialHeap(1)
    assert(!h.isEmpty)
  }

  test("findMin in range 1 to 10") {
    var h = ScheduledBinomialHeap(2,4,6,8,10,1,3,5,7,9)
    expect(1) { h.findMin }
    h = h.deleteMin
    expect(2) { h.findMin }
  }

  import ScheduledBinomialHeap._
  def checkRank[A](t: Tree[A]): (Int, Boolean) = t match {
    case Tree(_, Nil) => (0, true)
    case Tree(_, xs)  => {
      val (ranks, bools) = xs.map(checkRank(_)).unzip
      val r = ranks.head + 1
      val checkOrder = (r == xs.length) && (ranks.tail, (0 until ranks.head).reverse).zipped.forall(_ == _)
      (r, bools.forall(identity) && checkOrder)
    }
  }

  def checkRanks[A](h: ScheduledBinomialHeap[A]): Boolean = {
    def checkRanksH(rank: Int, digits: Digits[A]): Boolean =
      if (digits.isEmpty)
        true
      else digits match {
        case Zero   #:: ds =>
          checkRanksH(rank+1, ds)
        case One(t) #:: ds => {
          val (r, b) = checkRank(t)
          (rank == r) && b && checkRanksH(rank+1, ds)
        }
      }
    checkRanksH(0, h.digits)
  }

  test("check ranks after insertions") {
    val h = ScheduledBinomialHeap(2,4,6,8,10,1,3,5,7,9)
    assert(checkRanks(h))
  }

  test("check ranks after deletions") {
    var h = ScheduledBinomialHeap(2,4,6,8,10,1,3,5,7,9)
    assert(checkRanks(h))
    h = h.deleteMin
    assert(checkRanks(h))
    h = h.deleteMin
    assert(checkRanks(h))
    h = h.deleteMin
    assert(checkRanks(h))
  }
}
