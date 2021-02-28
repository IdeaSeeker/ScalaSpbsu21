package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._

sealed trait MyGenericList[+T] {
    def head: T
    def tail: MyGenericList[T]
    def drop(n: Int): MyGenericList[T]
    def take(n: Int): MyGenericList[T]
    def map[U](f: T => U): MyGenericList[U]
    def ::[U >: T](head: U): MyGenericList[U] = MyCons(head, this)
    def foldLeft[S](init: S)(f: (S, T) => S): S
}

case object MyNil extends MyGenericList[Nothing] {
    override def head: Nothing = undef

    override def tail: Nothing = undef

    override def drop(n: Int): MyGenericList[Nothing] = n match {
        case 0 => MyNil
        case _ => undef
    }

    override def take(n: Int): MyGenericList[Nothing] = n match {
        case 0 => MyNil
        case _ => undef
    }

    override def map[U](f: Nothing => U): MyGenericList[U] = MyNil

    override def foldLeft[S](init: S)(f: (S, Nothing) => S): S = init
}

case class MyCons[T](override val head: T,
                     override val tail: MyGenericList[T]) extends MyGenericList[T] {

    override def drop(n: Int): MyGenericList[T] = n match {
        case 0 => this
        case n => tail.drop(n - 1)
    }

    override def take(n: Int): MyGenericList[T] = n match {
        case 0 => MyNil
        case n => MyCons(head, tail.take(n - 1))
    }

    override def map[U](f: T => U): MyGenericList[U] =
        MyCons(f(head), tail.map(f))

    override def foldLeft[S](init: S)(f: (S, T) => S): S =
        tail.foldLeft(f(init, head))(f)
}

object MyGenericList {
    def undef: Nothing =
        throw new UnsupportedOperationException("operation is undefined")

    def fromSeq[T](seq: Seq[T]): MyGenericList[T] =
        seq.foldRight(MyNil: MyGenericList[T])((head, tail) => head :: tail)

    def sum[U : Numeric](myList: MyGenericList[U]): U = myList match {
        case MyNil  => undef
        case myList => myList.foldLeft(Numeric[U].zero)((a, b) => Numeric[U].plus(a, b))
    }

    def size(myList: MyGenericList[_]): Int =
        myList.foldLeft(0)((n, _) => n + 1)
}