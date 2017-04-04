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

    final def take(n: Int): Stream[A] = this match {
        case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
        case _ => Stream.empty
    }

    @annotation.tailrec
    final def drop(n: Int): Stream[A] = this match {
        case Cons(_, t) if n > 0 => t().drop(n - 1)
        case _ => this
    }

    def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    def takeWhile(f: A => Boolean): Stream[A] =
        foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else Stream.empty)

    def map[B](f: A => B): Stream[B] =
        foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

    def filter(f: A => Boolean): Stream[A] =
        foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

    def append[B >: A](n: => Stream[B]): Stream[B] =
        foldRight(n)((a,b) => Stream.cons(a, b))

    def flatMap[B](f: A => Stream[B]): Stream[B] = {
        foldRight(Stream.empty[B])((a, b) => f(a).append(b))
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
    def ones: Stream[Int] = cons(1, ones)
    def cons[A](hd: => A, t: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = t
        Cons(() => head, () => tail)
    }
    def ::[A](hd: => A, t: => Stream[A]): Stream[A] = cons(hd, t)

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))
    def from(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))
    def fibs(): Stream[Int] =
        unfold((0, 1))(s => Some(s._1, (s._1 + s._2, s._1)))

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
        f(z) match {
            case Some((a, s)) => cons(a, unfold(s)(f))
            case None => empty
        }
    }

}


object StreamTest {
    def foo = {
        println("Running foo!");
        1
    }
    def main(args: Array[String]): Unit = {
        import Stream._
        val x: Stream[Int] = cons(1, cons(2 + foo, cons(3 + foo, cons(4 + foo, cons(5 + foo, empty)))))
        println(s"take0: ${x.take(0).toList}")
        println(s"take3: ${x.take(3).toList}")
        println(s"take10: ${x.take(10).toList}")
        println(s"takeWhile (_ <= 3): ${x.takeWhile(_ <= 3).toList}")
        println(s"MAP (_ + 1): ${x.map(_ + 1).toList}")
        println(s"FILTER (_ > 3): ${x.filter(_ > 3).toList}")
        println(s"Append: ${x.append(Stream(1,2,3)).toList}")
        val y: Stream[Stream[Int]] = Stream(Stream(1, 2, 3), Stream(4, 5, 6), Stream(7, 8))
        println(s"flatMap: ${y.flatMap(_.map(_ + 1)).toList}")
        println(s"${ones.take(10).toList}")
        println(s"${constant(3).take(10).toList}")
        println(s"${constant('y').take(10).toList}")
        println(s"${from(10).take(10).toList}")
        println(s"${fibs().take(13).toList}")
        println(s"${unfold(1)(s => Some((s.toString, s + 1))).take(4).toList}")
    }
}
