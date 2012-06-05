package chapter6

import org.scalatest.FunSuite
import scala.annotation.tailrec

class LazyBinomialHeapSuite extends FunSuite {

  test("empty heap is empty") {
    val h = LazyBinomialHeap.empty[Int]
    assert(h.isEmpty)
  }

  test("findMin of empty heap fails") {
    val h = LazyBinomialHeap.empty[Int]
    intercept[Exception] {
      h.findMin
    }
  }

  test("deleteMin of empty heap fails") {
    val h = LazyBinomialHeap.empty[Int]
    intercept[Exception] {
      h.deleteMin
    }
  }

  test("singleton heap is non-empty") {
    val h = LazyBinomialHeap(1)
    assert(!h.isEmpty)
  }

  test("findMin in range 1 to 10") {
    val h = LazyBinomialHeap(2,4,6,8,10,1,3,5,7,9)
    expect(1) { h.findMin }
  }

  import LazyBinomialHeap._
  def checkRank[A](t: Tree[A]): Boolean = {
    def checkRankH(t: Tree[A]): (Int, Boolean) = t match {
      case Tree(rnk, _, Nil) => (0, rnk == 0)
      case Tree(rnk, _, xs)  => {
        val (ranks, bools) = xs.map(checkRankH(_)).unzip
        val checkOrder = (ranks, (0 until rnk).reverse).zipped.forall(_ == _)
        (ranks.head+1, bools.forall(identity) && (xs.length == rnk) && checkOrder)
      }
    }
    checkRankH(t)._2
  }
  def checkRanks[A](l: List[Tree[A]]): Boolean = l.forall(checkRank(_))

  test("check ranks after insertions") {
    val h = LazyBinomialHeap(2,4,6,8,10,1,3,5,7,9)
    assert(checkRanks(h.heap))
  }

  test("check ranks after deletions") {
    var h = LazyBinomialHeap(2,4,6,8,10,1,3,5,7,9)
    assert(checkRanks(h.heap))
    h = h.deleteMin
    assert(checkRanks(h.heap))
    h = h.deleteMin
    assert(checkRanks(h.heap))
    h = h.deleteMin
    assert(checkRanks(h.heap))
  }

  def checkRankOrder[A](l: List[Tree[A]]): Boolean = {
    val ranks = l.map(_.rank)
    (ranks, ranks.tail).zipped.forall(_ <= _)
  }

  test("check rank ordering after insertions") {
    val h = LazyBinomialHeap(2,4,6,8,10,1,3,5,7,9)
    assert(checkRankOrder(h.heap))
  }

  test("check rank ordering after deletions") {
    var h = LazyBinomialHeap(2,4,6,8,10,1,3,5,7,9)
    assert(checkRankOrder(h.heap))
    h = h.deleteMin
    assert(checkRankOrder(h.heap))
    h = h.deleteMin
    assert(checkRankOrder(h.heap))
    h = h.deleteMin
    assert(checkRankOrder(h.heap))
  }
}
