package ru.spbau.maxim.immutable
import ru.spbau.maxim
import ru.spbau.maxim.AbstractMultiset

import scala.collection.JavaConverters._
import scala.collection.immutable.Map.WithDefault
import scala.collection.mutable


class Multiset[A](private[maxim] val elements: Map[A, List[A]]) extends AbstractMultiset[A] {
  def this() = this(Map[A, List[A]]().withDefaultValue(List()))

  override def +(el: A): Multiset[A] = {
    elements.get(el) match {
      case Some(list) => new Multiset[A](elements + ((el, el :: list)))
      case _ => new Multiset[A](elements + ((el, List(el))))
    }
  }

  override def &(other: maxim.Multiset[A]): maxim.Multiset[A] = {
    other match {
      case multiset: Multiset[A] => choseElements(multiset, _ <= _)
      case _ => other & this
    }
  }

  override def |(other: maxim.Multiset[A]): maxim.Multiset[A] = {
    other match {
      case multiset: Multiset[A] => choseElements(multiset, _ >= _)
      case _ => other | this
    }
  }

  override def iterator: Iterator[A] = elements.iterator.flatMap { case (_, lst) => lst }

  override def apply(el: A): Int = {
    elements.get(el) match {
      case Some(list) => list.length
      case _ => 0
    }
  }

  private def choseElements(multiset: Multiset[A], cmp: (Int, Int) => Boolean): Multiset[A] = {
    val res = Map.newBuilder[A, List[A]]
    (elements.keySet ++ multiset.elements.keySet).foreach {
      key => {
        def getList(elements: Map[A, List[A]]): List[A] = elements.get(key) match {
          case Some(lst) => lst
          case _ => List[A]()
        }
        val list = getList(elements)
        val otherList = getList(multiset.elements)
        val curList = if (cmp(list.length, otherList.length)) list else otherList

        if (curList != List())
          res += ((key, curList))
      }
    }
    new Multiset[A](res.result())
  }
}