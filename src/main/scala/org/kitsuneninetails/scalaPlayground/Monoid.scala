package org.kitsuneninetails.scalaPlayground
import scala.language.implicitConversions

trait Monoid[A] {
  def empty: A
  def combine(a: A, b: A): A
}

object MonoidTest {
  final case class MyPair[A, B](a: A, b: B)

  object MyPair {
    implicit def pair2monoid[A, B](implicit A: Monoid[A], B: Monoid[B]): Monoid[MyPair[A, B]] =
      new Monoid[MyPair[A, B]] {
        override def empty: MyPair[A, B] = MyPair(A.empty, B.empty)

        override def combine(a: MyPair[A, B], b:  MyPair[A, B]): MyPair[A, B] =
          MyPair(A.combine(a.a, b.a), B.combine(a.b, b.b))
      }
  }

  def sum[A](list: List[A])(implicit A: Monoid[A]): A = {
    list.foldRight(A.empty)(A.combine)
  }

  def main(args: Array[String]): Unit = {
    implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
      def empty: Int = 0
      def combine(a: Int, b: Int): Int = a + b
    }

    implicit val intMonoid2: Monoid[Int] = new Monoid[Int] {
      def empty: Int = 1
      def combine(a: Int, b: Int): Int = a - b
    }

    implicit val strMonoid: Monoid[String] = new Monoid[String] {
      def empty: String = ""
      def combine(a: String, b: String): String = a ++ b
    }

    val l1 = List(MyPair(1, "test "), MyPair(4, "combine"))
    //println(s"${sum(l1)}")
  }
}
