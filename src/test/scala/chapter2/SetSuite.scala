package chapter2

import org.scalatest.FunSuite
 
class SetSuite extends FunSuite {
  
  test("empty set is empty") {
    val s = Set.empty[Int]
    assert(s.isEmpty)
  }
  
  test("singleton set is non-empty") {
    val s = Set(1)
    assert(!s.isEmpty)
  }
  
  test("no membership empty set") {
    val s = Set.empty[Int]
    assert(!s.member(1))
  }
  
  test("double insert ignored") {
    var s = Set.empty[Int]
    s = s.insert(1)
    val s2 = s.insert(1)
    assert(s.eq(s2))
  }
  
  test("membership") {
    val s = Set(1,2,3)
    assert(s.member(1))
    assert(s.member(2))
    assert(s.member(3))
    assert(!s.member(4))
  }
}