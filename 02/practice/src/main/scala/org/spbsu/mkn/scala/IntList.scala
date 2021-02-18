package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

sealed trait IntList {
    def head: Int
    def tail: IntList
    def drop(n: Int): IntList
    def take(n: Int): IntList
    def map(f: Int => Int): IntList
    def ::(head: Int): IntList = IntCons(head, this)
}

case object IntNil extends IntList {
    override def head: Int = undef

    override def tail: IntList = undef

    override def drop(n: Int): IntList = n match {
        case 0 => IntNil
        case _ => undef
    }

    override def take(n: Int): IntList = n match {
        case 0 => IntNil
        case _ => undef
    }

    override def map(f: Int => Int): IntList = IntNil
}

case class IntCons(_head: Int, _tail: IntList) extends IntList {
    override def head: Int = _head

    override def tail: IntList = _tail

    override def drop(n: Int): IntList = n match {
        case 0 => this
        case n => _tail.drop(n - 1)
    }

    override def take(n: Int): IntList = n match {
        case 0 => IntNil
        case n => IntCons(_head, _tail.take(n - 1))
    }

    override def map(f: Int => Int): IntList = IntCons(f(_head), _tail.map(f))
}


object IntList {
    def undef: Nothing =
        throw new UnsupportedOperationException("operation is undefined")

    def fromSeq(seq: Seq[Int]): IntList = seq.size match {
        case 0 => IntNil
        case _ => IntCons(seq.head, fromSeq(seq.tail))
    }

    def sum(intList: IntList): Int = intList match {
        case IntNil                => undef
        case IntCons(head, IntNil) => head
        case IntCons(head, tail)   => head + sum(tail)
    }

    def size(intList: IntList): Int = intList match {
        case IntNil  => 0
        case intList => sum(intList.map(_ => 1))
    }
}
