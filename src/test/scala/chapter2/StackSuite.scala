package chapter2

import org.scalatest.FunSuite
 
class StackSuite extends FunSuite {
  
  test("head of empty stack fails") {
    val s = Stack.empty
    intercept[Exception] {
      s.head
    }
  }
  
  test("tail of empty stack fails") {
    val s = Stack.empty
    intercept[Exception] {
      s.tail
    }
  }
  
  test("empty stack is empty") {
    val s = Stack.empty
    assert(s.isEmpty)
  }
  
  test("singleton stack is non-empty") {
    val s = Stack(1)
    assert(!s.isEmpty)
  }
  
  test("empty ++ non-empty") {
    val s = Stack(1,2,3)
    assert(s.eq(Stack.empty[Int] ++ s))
  }
  
  test("non-empty ++ empty") {
    val s = Stack(1,2,3)
    assert(s === (s ++ Stack.empty[Int]))
  }
  
  test("non-empty ++ non-empty") {
    val s1 = Stack(1,2,3)
    val s2 = Stack(4,5,6)
    assert(s1.head === (s1 ++ s2).head)
    assert((s1.tail ++ s2) === (s1 ++ s2).tail)
  }
  
  test("update of empty stack fails") {
    val s = Stack.empty[Int]
    intercept[Exception] {
      s(0) = 1
    }
  }
  
  test("out-of-bounds update fails") {
    val s = Stack(1,2,3)
    intercept[Exception] {
      s(3) = 1
    }
  }
  
  test("update head") {
    val s = Stack(1,2,3)
    val s2 = s(0) = 4
    expect(4) {
      s2.head
    }
    assert(s.tail.eq(s2.tail))
  }
}