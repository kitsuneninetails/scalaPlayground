package org.kitsuneninetails.scalaPlayground

import scala.annotation.tailrec

object FoldLeft {
    def init[A](l: List[A]): List[A] = {
        if (l == Nil) Nil
        else
            l.tail match {
                case Nil => Nil
                case _ => l.head :: init(l.tail)
            }
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
        as match {
            case Nil => z
            case _ => f(as.head, foldRight(as.tail, z)(f))
        }
    }

    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
        as.tail match {
            case Nil => f(z, as.head)
            case _ => foldLeft(as.tail, f(z, as.head))(f)
        }
    }

    def map[A,B](as: List[A])(f: A => B): List[B] = {
        as match {
            case Nil => Nil
            case _ => f(as.head) :: map(as.tail)(f)
        }
    }

    def reverse[A](as: List[A]): List[A] = {
        foldLeft(as, List[A]())((y, x) => x +: y)
    }

    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
        foldLeft(as, Nil:List[A]) {(t, h) => if (f(h)) t :+ h else t}
    }

    def main(args: Array[String]): Unit = {
        val x = 1:: 2:: 3:: 4:: 5:: Nil
        def sum(as: List[Int]) = foldLeft[Int, Int](as, 0) {_ + _}
        def product(as: List[Int]) = foldLeft[Int, Int](as, 1) {_ * _}
        def count[A](as: List[A]) = foldLeft[A, Int](as, 0) {(x, _) => x + 1}
        def doubleAll(as: List[Int]) = map(as) {_ * 2}
        println(s"${sum(x)}")
        println(s"${product(x)}")
        println(s"${count(x)}")
        println(s"${reverse(x)}")
        println(s"${doubleAll(x)}")
        println(s"${filter(x){_ % 2 == 0}}")

    }
}
