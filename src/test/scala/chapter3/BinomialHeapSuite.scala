package chapter3

import org.scalatest.FunSuite
import scala.annotation.tailrec

class BinomialHeapSuite extends FunSuite {
  
  test("empty heap is empty") {
    val h = BinomialHeap.empty[Int]
    assert(h.isEmpty)
  }
  
  test("findMin of empty heap fails") {
    val h = BinomialHeap.empty[Int]
    intercept[Exception] {
      h.findMin
    }
  }
  
  test("deleteMin of empty heap fails") {
    val h = BinomialHeap.empty[Int]
    intercept[Exception] {
      h.deleteMin
    }
  }
  
  test("singleton heap is non-empty") {
    val h = BinomialHeap(1)
    assert(!h.isEmpty)
  }
  
  test("findMin in range 1 to 10") {
    val h = BinomialHeap(2,4,6,8,10,1,3,5,7,9)
    expect(1) { h.findMin }
  }
  
  import BinomialHeap._
  def checkRank[A](t: Tree[A]): Boolean = {
    def computeRank[A](t: Tree[A]): Int = {
      @tailrec def computeRankAcc[A](t: Tree[A], acc: Int): Int = t match {
        case Tree(_, _, Nil) => acc
        case Tree(_, _, x :: _) => computeRankAcc(x, acc+1)
      }
      computeRankAcc(t, 0)
    }
    def checkRankList[A](l: List[Tree[A]], r: Int): Boolean = l match {
      case Nil => r == -1
      case x :: xs =>
        (r == computeRank(x)) &&
        checkRankList(xs, r-1)
    }
    t match {
      case Tree(r, _, l) => {
        val rank = computeRank(t)
        (r == rank) &&
        checkRankList(l, r-1)
      }
    }
  }
  def checkRanks[A](l: List[Tree[A]]): Boolean =
    l.foldRight(true)(checkRank(_) && _)
  
  test("check ranks after insertions") {
    val h = BinomialHeap(2,4,6,8,10,1,3,5,7,9)
    assert(checkRanks(h.heap))
  }
  
  test("check ranks after deletions") {
    var h = BinomialHeap(2,4,6,8,10,1,3,5,7,9)
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
    (ranks, ranks.tail).zipped.view.map(x => x._1 <= x._2).fold(true)(_ && _)
  }
  
  test("check rank ordering after insertions") {
    val h = BinomialHeap(2,4,6,8,10,1,3,5,7,9)
    assert(checkRankOrder(h.heap))
  }
  
  test("check rank ordering after deletions") {
    var h = BinomialHeap(2,4,6,8,10,1,3,5,7,9)
    assert(checkRankOrder(h.heap))
    h = h.deleteMin
    assert(checkRankOrder(h.heap))
    h = h.deleteMin
    assert(checkRankOrder(h.heap))
    h = h.deleteMin
    assert(checkRankOrder(h.heap))
  }
}