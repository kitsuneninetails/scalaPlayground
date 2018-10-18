package org.kitsuneninetails.scalaPlayground

object Curry {
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a: A => f(a, _)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    def x = curry((a: Int, b: Double) => a + b)
    def y = uncurry(x)

    println(s"${x(2)(3.0)}")
    println(s"${y(2, 3.0)}")

    def z1 = (a: Int) => a * 2
    def z2 = (b: Int) => b + 1
    def z = compose(z1, z2)
    def zb = z1 compose z2
    println(s"${z(3)}")
    println(s"${zb(3)}")

  }
}
