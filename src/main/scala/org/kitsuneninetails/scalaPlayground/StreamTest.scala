package org.kitsuneninetails.scalaPlayground

import scala.annotation.tailrec

sealed trait Stream[+A] {
    def toList: List[A] = {
        @tailrec
        def loop(currList: List[A], str: Stream[A]): List[A] =
            str match {
                case Empty => currList
                case Cons(h, t) => loop(h() :: currList, t())
        }
        loop(List[A](), this).reverse
    }

    def head: Option[A] = this match {
        case Empty => None
        case Cons(h, _) => Some(h())
    }

    final def take(n: Int): Stream[A] = {
        @tailrec
        def loop(h: Stream[A], t: () => Stream[A], rem: Int): Stream[A] = {
            if (rem == 0) h
            else t match {
                case Cons(hd, tl) => loop(h, tl, rem - 1)
                case _ => _
            }
        }
        loop(Empty, () => this, n)
    }

    override def toString: String =
        this match {
            case Empty => "Empty"
            case Cons(h, _) => s"${h()}, ..."
        }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    def cons[A](hd: => A, t: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = t
        Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}


object StreamTest {
    def foo = {
        println("Running foo!");
        1
    }
    def main(args: Array[String]): Unit = {
        val x: Stream[Int] = Stream.cons(1, Stream.cons(2 + foo, Stream.cons(3 + foo, Stream.cons(4 + foo, Stream.cons(5 + foo, Stream.empty)))))
        println(s"Stream $x")
        println("Converting to List")
        println(s"${x.toList}")
        println(s"${x.take(3).toList}")
    }
}
