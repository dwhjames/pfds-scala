package chapter4

import org.scalatest.FunSuite
 
class StreamSuite extends FunSuite {
  import Stream._
  
  def bottom = throw new Exception("bottom was forced!")
  
  test("head of empty stream fails") {
    val s = Stream.empty
    intercept[Exception] {
      s.head
    }
  }
  
  test("tail of empty stream fails") {
    val s = Stream.empty
    intercept[Exception] {
      s.tail
    }
  }
  
  test("cons is lazy in the tail") {
    val s = SCons(1, bottom)
  }
  
  test("tail is forced on request") {
    val s = SCons(1,bottom)
    intercept[Exception] {
      s.tail
    }
  }
  
  test("tail is call-by-need") {
    var n = 0
    def sideEffect = { n += 1; SNil }
    val s = SCons(1, sideEffect)
    s.tail
    s.tail
    expect(1) { n }
  }
  
  test("appending to nil returns same") {
    val s = SCons(1, SNil)
    assert(s.eq(SNil ++ s))
  }
  
  test("appending is lazy") {
    val s = SCons(1, bottom)
    s ++ s
  }
  
  test("take n from nil") {
    expect(SNil) {
      SNil take 0
    }
    expect(SNil) {
      SNil take 5
    }
  }
  
  test("take 0") {
    val s = SCons(1, bottom)
    expect(SNil) {
      s take 0
    }
  }
  
  test("take 1") {
    val s = SCons(1, bottom)
    val s1 = s take 1
    assert(s1.head === 1)
    assert(s1.tail === SNil)
  }
  
  test("drop n from nil") {
    expect(SNil) {
      SNil drop 0
    }
    expect(SNil) {
      SNil drop 5
    }
  }
  
  test("drop 0") {
    val s = SCons(1, bottom)
    assert(s.eq(s drop 0))
  }
  
  test("drop 1") {
    val t = SCons(2, bottom)
    val s = SCons(1, t)
    assert(t.eq(s drop 1))
  }
  
  test("reverse nil") {
    expect(SNil) {
      SNil.reverse
    }
  }
  
  test("reverse singleton") {
    val s = SCons(1, SNil)
    val t = s.reverse
    assert(s.head === t.head)
    assert(s.tail === t.tail)
  }
  
  test("reverse two") {
    val s = SCons(1, SCons(2, SNil))
    val t = s.reverse
    assert(s.head === t.tail.head)
    assert(s.tail.head === t.head)
  }
}