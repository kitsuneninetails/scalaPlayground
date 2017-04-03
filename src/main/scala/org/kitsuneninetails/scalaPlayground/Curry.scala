package org.kitsuneninetails.scalaPlayground

object Curry {
    def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
        (a: A) => f(a, _)
    }

    def main(args: Array[String]): Unit = {
        def x = curry((a: Int, b: Double) => a + b)
        println(s"${x(2)(3.0)}")
    }
}
