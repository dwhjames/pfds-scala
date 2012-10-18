package chapter8

import org.scalatest.FunSuite

class BankersDequeSuite extends FunSuite {

  test("head of empty queue fails") {
    val q = BankersDeque.empty
    intercept[Exception] {
      q.head
    }
  }

  test("last of empty queue fails") {
    val q = BankersDeque.empty
    intercept[Exception] {
      q.last
    }
  }

  test("tail of empty queue fails") {
    val q = BankersDeque.empty
    intercept[Exception] {
      q.tail
    }
  }

  test("init of empty queue fails") {
    val q = BankersDeque.empty
    intercept[Exception] {
      q.init
    }
  }

  test("empty queue is empty") {
    val q = BankersDeque.empty
    assert(q.isEmpty)
  }

  test("singleton queue is non-empty") {
    var q = BankersDeque.empty[Int]
    q = q :+ 1
    assert(!q.isEmpty)
    q = BankersDeque.empty
    q = 1 +: q
    assert(!q.isEmpty)
  }

  test("head after cons") {
    var q = BankersDeque.empty[Int]
    q = 1 +: q
    expect(1) { q.head }
  }

  test("head after snoc") {
    var q = BankersDeque.empty[Int]
    q = q :+ 1
    expect(1) { q.head }
  }

  test("last after cons") {
    var q = BankersDeque.empty[Int]
    q = 1 +: q
    expect(1) { q.last }
  }

  test("last after snoc") {
    var q = BankersDeque.empty[Int]
    q = q :+ 1
    expect(1) { q.last }
  }

  def invariant[A](q: BankersDeque[A]) =
    (q.lenf <= BankersDeque.c * q.lenr + 1) &&
    (q.lenr <= BankersDeque.c * q.lenf + 1)

  test("queue invariant") {
    var q = BankersDeque.empty[Int]
    assert(invariant(q))

    q = q :+ 4
    assert(invariant(q))

    q = 3 +: q
    assert(invariant(q))

    q = q :+ 5
    assert(invariant(q))

    q = 2 +: q
    assert(invariant(q))

    q = q :+ 6
    assert(invariant(q))

    q = 1 +: q
    assert(invariant(q))

    expect(1) { q.head }

    q = q.tail
    assert(invariant(q))

    expect(2) { q.head }

    q = q.tail
    assert(invariant(q))

    expect(3) { q.head }

    q = q.tail
    assert(invariant(q))

    expect(6) { q.last }

    q = q.init
    assert(invariant(q))

    expect(5) { q.last }

    q = q.init
    assert(invariant(q))

    expect(4) { q.last }

    q = q.init
    assert(invariant(q))

    assert(q.isEmpty)
  }

  test("check delays reversal") {
    def bottom: Stream[Int] = throw new Exception("bottom was forced!")
    val q = BankersDeque.check(4, 1 #:: 2 #:: 3 #:: 4 #:: bottom, 1, 5 #:: bottom)
    expect(1) { q.head }
    expect(5) { q.last }

    val r = BankersDeque.check(1, 1 #:: bottom, 4, 5 #:: 4 #:: 3 #:: 2 #:: bottom)
    expect(1) { r.head }
    expect(5) { r.last}
  }
}
