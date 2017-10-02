package ru.spbau.maxim

import org.scalatest.{FlatSpec, Matchers}

class MultisetTest extends FlatSpec with Matchers {
  "Multiset " should " add this elements" in {
    val multiset1: Multiset[Int] = Multiset[Int]()
    val multiset2: Multiset[Int] = ((((multiset1 + 1) + 2) + 3) + 5) + 3

    multiset1(1) should be (0)
    multiset2(3) should be (2)
    multiset2(1) should be (1)
    multiset2(4) should be (0)

    (for (a <- multiset2; if a > 2) yield a).toList.sorted should be (List(3, 3, 5))
  }

  it should " union/intersect this multisets" in {
    val multiset2: Multiset[Int] = Multiset(1, 2, 3, 5, 3)
    val multiset3: Multiset[Int] = Multiset[Int](3, 6, 5, 5)
    multiset3.foreach(println)

    val multiset4 = multiset3 & multiset2
    val multiset5 = multiset3 | multiset2
    (1 to 6).toList.map(multiset4) should be (List(0, 0, 1, 0, 1, 0))
    (1 to 6).toList.map(multiset5) should be (List(1, 1, 2, 0, 2, 1))
  }

  it should " unapply this seq" in {
    Multiset.unapplySeq[Int](Multiset(1, -1, 0)).get.toList.sorted should be (List(-1, 0, 1))
    val res = Multiset(1, 1) match {
      case Multiset(1, _) => true
      case _ => false
    }
    res should be (true)
  }

}
