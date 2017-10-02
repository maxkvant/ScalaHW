package ru.spbau.maxim

import scala.collection.generic.GenericCompanion
import scala.collection.{AbstractIterable, Iterable, mutable}

trait Multiset[A] extends (A => Int)
                  with Iterable[A] {
  def +(el: A): Multiset[A]
  def &(other: Multiset[A]): Multiset[A]
  def |(other: Multiset[A]): Multiset[A]

  override def companion: GenericCompanion[Multiset] = Multiset
}

object Multiset extends GenericCompanion[Multiset] {
  def unapplySeq[A](multiset: Multiset[A]): Option[Seq[A]] = Some(multiset.toSeq)

  override def apply[A](elems: A*): Multiset[A] = {
    val builder = this.newBuilder[A]
    for (a: A <- elems) {
      builder += a
    }
    builder.result()
  }

  override def newBuilder[A]: mutable.Builder[A, Multiset[A]] = new mutable.ReusableBuilder[A, Multiset[A]] {
    private def empty() = mutable.Map[A, List[A]]().withDefaultValue(List())

    private var cur: mutable.Map[A, List[A]] = empty()

    override def clear(): Unit = { cur = empty() }

    override def result(): Multiset[A] = new immutable.Multiset(cur.toMap)

    override def +=(elem: A): this.type = {
      val lst = cur(elem)
      cur(elem) = elem :: lst
      this
    }
  }
}

abstract class AbstractMultiset[A] extends AbstractIterable[A] with Multiset[A]

