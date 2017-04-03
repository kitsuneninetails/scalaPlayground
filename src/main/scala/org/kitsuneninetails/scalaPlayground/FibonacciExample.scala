package org.kitsuneninetails.scalaPlayground

import scala.annotation.tailrec

object FibonacciExample {

    def fib(n: Int): Int = {
        @tailrec
        def fib2(n: Int, max: Int, acc: Int, acc2: Int): Int = {
            if (n >= max) acc + acc2
            else n match {
                case 1 => fib2(n + 1, max, 1, 0)
                case _ => fib2(n + 1, max, acc + acc2, acc)
            }
        }

        if (n == 0) 0
        else fib2(1, n, 1, 0)
    }


    def fib1(n: Int): Int = {
        n match {
            case 0 => 0
            case 1 => 1
            case _ => fib1(n - 2) + fib1(n - 1)
        }
    }

    def main(args: Array[String]): Unit = {
        for (i <- Range(0, 20)) println(s"${fib(i)}")
    }

}
