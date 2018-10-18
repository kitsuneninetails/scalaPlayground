package org.kitsuneninetails.scalaPlayground

import scala.annotation.tailrec

sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] =
        this match {
            case None => None
            case Some(x) => Some(f(x))
        }

    def flatMap[B](f: A => Option[B]): Option[B] =
        this map f getOrElse None

    def getOrElse[B >: A](default: => B): B =
        this match {
            case None => default
            case Some(x) => x
        }

    def orElse[B >: A](ob: => Option[B]): Option[B] =
        this map { Some(_) } getOrElse ob

    def filter(f: A => Boolean): Option[A] =
        this flatMap { x: A => if (f(x)) Some(x) else None }
}

case object None extends Option[Nothing]
case class Some[+A](obj: A) extends Option[A]

object MyOption {

    def lift[A, B](f: A => B): Option[A] => Option[B] = x => x map f

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        a flatMap { x => b map { y => f(x, y) } }

    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
        //a.foldRight(List())((a, b) => a flatMap b)
        a match {
            case Nil => Some(List())
            case h :: t => h flatMap { hh => sequence(t) map (hh :: _)}
        }
    }

    def mean(xs: Seq[Double]): Option[Double] = {
        if (xs.isEmpty) None
        else Some(xs.sum / xs.length)
    }
    def variance(xs: Seq[Double]): Option[Double] = {
        mean(xs) flatMap { rm =>
            mean(xs map { x => math.pow(x - rm, 2) })
        }
    }

    def main(args: Array[String]): Unit = {
        val testList: List[Option[Int]] = List(Some(1), Some(0), None)

        for (x <- testList) {
            println("-----")
            println(s"map: ${x map {_ + 3}}")
            println(s"flatMap: ${x flatMap {
                _ match {
                    case 0 => None
                    case x => Some(1.0 / x)
                }
            }}")
            println(s"getOrElse: ${x getOrElse 4}")
            println(s"orElse: ${x orElse Some(4)}")
            println(s"filter: ${x filter {_ != 1}}")
        }

        val seq = List(1.3, 5.2, 1.2, 0.2, -5.2, -8.2, 2.3, -3.8)
        println(s"mean: ${mean(seq)}")
        println(s"variance: ${variance(seq)}")

        val seq2 = List()
        println(s"mean: ${mean(seq2)}")
        println(s"variance: ${variance(seq2)}")

        println(s"${lift(math.abs)(Some(-1))}")
        println(s"${lift(math.abs)(None)}")
        println(s"${map2(Some(1), Some(2))(_ + _) getOrElse(-1)}")
        println(s"${map2(Some(1), None)(_ + _) getOrElse(-1)}")

        println(s"${sequence(List(Some(1), Some(2), Some(3))) getOrElse(-1)}")
        println(s"${sequence(List()) getOrElse(-1)}")
        println(s"${sequence(List(Some(1), None, Some(3))) getOrElse(-1)}")
    }
}
