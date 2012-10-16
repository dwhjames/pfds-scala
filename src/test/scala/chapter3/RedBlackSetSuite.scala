package chapter3

import org.scalatest.FunSuite
 
class RedBlackSetSuite extends FunSuite {

  test("empty set is empty") {
    val s = RedBlackSet.empty
    assert(s.isEmpty)
  }

  test("singleton set is non-empty") {
    val s = RedBlackSet(1)
    assert(!s.isEmpty)
  }

  test("no membership empty set") {
    val s = RedBlackSet.empty
    assert(!s.member(1))
  }

  test("double insert ignored") {
    val s1 = RedBlackSet(1)
    val s2 = s1.insert(1)
    assert(s1 == s2)
  }

  test("membership") {
    val s = RedBlackSet(1,2,3)
    assert(s.member(1))
    assert(s.member(2))
    assert(s.member(3))
    assert(!s.member(4))
  }

  def invariant[A](set: RedBlackSet[A]): Boolean = {
    import RedBlackSet._
    // (satisfies invariant, number of black nodes, node is red)
    def invariantH(s: RedBlackSet[A]): (Boolean, Int, Boolean) = s match {
      case Leaf => (true, 1, false)
      case Branch(c, l, _, r) => {
          val (ok_l, blk_l, red_l) = invariantH(l)
          val (ok_r, blk_r, red_r) = invariantH(r)
          c match {
            case Red =>
              (ok_l && ok_r && (blk_l == blk_r) && !red_l && !red_r, blk_l, true)
            case Black =>
              (ok_l && ok_r && (blk_l == blk_r), 1 + blk_l, false)
          } 
      }
    }
    invariantH(set)._1
  }

  test("insertion preserves invariant") {
    var s = RedBlackSet.empty[Int]
    assert(invariant(s))
    s = s.insert(1)
    assert(invariant(s))
    s = s.insert(2)
    assert(invariant(s))
    s = s.insert(3)
    assert(invariant(s))
    s = s.insert(4)
    assert(invariant(s))
    s = s.insert(5)
    assert(invariant(s))
    s = s.insert(6)
    assert(invariant(s))
  }

  test("fromSortedList ensures invariant") {
    import RedBlackSet._
    assert(invariant(fromSortedList(Nil)))
    assert(invariant(fromSortedList(List(1))))
    assert(invariant(fromSortedList(List(1,2))))
    assert(invariant(fromSortedList(List(1,2,3))))
    assert(invariant(fromSortedList(List(1,2,3,4))))
    assert(invariant(fromSortedList(List(1,2,3,4,5))))
    assert(invariant(fromSortedList(List(1,2,3,4,5,6))))
    assert(invariant(fromSortedList(List(1,2,3,4,5,6,7))))
  }
}
