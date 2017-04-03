package org.kitsuneninetails.scalaPlayground

object ListDrop {
    def drop[A](l: List[A], n: Int): List[A] = {
        (l, n) match {
            case (_, 0) => l
            case (Nil, _ )=> Nil
            case _ => drop (l.tail, n - 1)
        }
    }

    def main(args: Array[String]): Unit = {
        val x = 0 :: 1 :: 2 :: 3 :: Nil
        println(s"${drop(x, 2)}")
        println(s"${drop(x, 0)}")
        println(s"${drop(x, 5)}")
    }

}
