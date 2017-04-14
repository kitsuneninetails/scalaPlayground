package org.kitsuneninetails.scalaPlayground

import akka.actor.{Actor, ActorSystem, Props}
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import scala.collection.JavaConversions.`deprecated seqAsJavaList`

//case class Par[A](f: () => A, forking: Boolean = false) {
//    def apply(p: Par[A], fork: Boolean) = new Par[A](p.f, fork)
//}
//
//object Par {
//    def run[A](a: Par[A])(implicit es: ExecutorService): Future[A] = {
//        if (a.forking) es.submit(() => a.f())
//        else CompletableFuture.completedFuture(a.f())
//    }
//
//    def unit[A](a: A): Par[A] = new Par[A](() => a)
//    def forkUnit[A](a: => A): Par[A] = fork(unit(a))
//    def map2[A, B, C](l: Par[A], r: Par[B])(f: (A, B) => C)(implicit es: ExecutorService): Par[C] = {
//        val ls = run(l)
//        val rs = run(r)
//        unit(f(ls.get(), rs.get()))
//    }
//    def fork[A](a: => Par[A]): Par[A] = Par[A](a.f, forking=true)
//}


sealed trait Future[A] {
    private[scalaPlayground] def apply(k: A => Unit): Unit
}

object Par {

    type Par[A] = ExecutorService => Future[A]

    val as: ActorSystem = ActorSystem.create()
    def run[A](es: ExecutorService)(p: Par[A]): A = {
        val ref = new AtomicReference[A]
        val latch = new CountDownLatch(1)
        p(es) { a => ref.set(a); latch.countDown()}
        latch.await()
        ref.get()
    }
    case class ParLeft[A](a: A)
    case class ParRight[B](b: B)

    class ParMap2[A, B, C](f: (A, B) => C, val cb: C => Unit)(val es: ExecutorService) extends Actor {
        var ar: Option[A] = None
        var br: Option[B] = None

        override def receive: Actor.Receive = {
            case ParLeft(a: A) => br match {
                case None => ar = Some(a)
                case Some(b) => eval(es)(cb(f(a, b)))
            }
            case ParRight(b: B) => ar match {
                case None => br = Some(b)
                case Some(a) => eval(es)(cb(f(a, b)))
            }
        }
    }
    def parMap2Props[A, B, C](f: (A, B) => C, cb: C => Unit)(es: ExecutorService): Props =
        Props(new ParMap2[A, B, C](f, cb)(es))

    def unit[A](a: A): Par[A] =
        _ => new Future[A] {
            def apply(cb: A => Unit): Unit =
                cb(a)
        }

    def fork[A](a: => Par[A]): Par[A] =
        es => new Future[A] {
            def apply(cb: A => Unit): Unit =
                eval(es)(a(es)(cb))
        }

    def eval(es: ExecutorService)(r: => Unit): Unit =
        es.submit(new Callable[Unit] { def call = r })

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
        es => new Future[C] {
            def apply(cb: C => Unit): Unit = {
                val combiner = as.actorOf(parMap2Props[A, B, C](f, cb)(es))
                p(es) { a => combiner ! ParLeft(a) }
                p2(es) { b => combiner ! ParRight(b) }
            }
        }


    def map[A, B](o: Par[A])(f: A => B): Par[B] =
        map2(o, unit(()))((a, _) => f(a))

    def asyncF[A,B](f: A => B): A => Par[B] =
        a => fork(unit(f(a)))

    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    {
        def seqInner[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = {
            if (ps.isEmpty) unit(Vector())
            else if (ps.length > 1) {
                val (l, r) = ps.splitAt(ps.length / 2)
                map2(seqInner(l), seqInner(r))(_ ++ _)
            } else map(ps.head)(Vector(_))
        }
        map(seqInner(ps.toIndexedSeq))(_.toList)
    }

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
        val parList = ps.map(asyncF(f)(_))
        sequence(parList)
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
        val pars = as map asyncF((a) => if (f(a)) List(a) else List())
        map(sequence(pars))(_.flatten)
    }

    def chooseN[A](n: Par[Int])(c: Vector[Par[A]]): Par[A] =
        flatMap(n) {a => c(a)}

    def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
        es => new Future[B] {
            override def apply(cb: (B) => Unit) =
                p(es)(a => f(a)(es)(cb))
        }

    def join[A](a: Par[Par[A]]): Par[A] =
        flatMap(a){ a => a }
}

object Parallelism {
    implicit val es = new ScheduledThreadPoolExecutor(1)
    import Par._
    def sum(ints: IndexedSeq[Int]): Par[Int] =
        if (ints.length > 1) {
            val (l, r) = ints.splitAt(ints.length / 2)
            map2(sum(l), sum(r))(_ + _)
        } else fork(unit(ints.headOption getOrElse 0))
    def sortPar(l: Par[Seq[Int]]): Par[Seq[Int]] =
        map(l)(_.sorted)

    def main(args: Array[String]): Unit = {

        val x: IndexedSeq[Int] = Vector(2, 6, 4, 3, 8, 1, 8, 9, 0)
        println(s"SUM = ${x.sum}")
        sum(x)(es) {a => println(s"SUM FUNC = $a") }
        sortPar(unit(x))(es) {a => println(s"SORT FUNC = $a") }

        val y: List[Int] = List(6, 5, 2, 8, 4, 1, 5, 8, 0, 3, 4, 1)
        parMap(y)(_ + 1)(es) {a => println(s"MAP FUNC = $a") }
        println(s"FILTER FUNC = ${run(es)(parFilter(y)(_ % 2 == 0))}")

        val z: Vector[Par[List[Int]]] = Vector(parMap(y)(_ - 2), parMap(y)(_ + 2))
        val c: Par[Int] = unit(0)
        println(s"ChooseN: ${run(es)(chooseN(c)(z))}")
        flatMap(c){a => z(a)}(es) { a => println(s"FLATMAP: $a")}
        val c2: Par[Par[Int]] = unit(unit(3))
        join(c2)(es) { a => println(s"JOIN: $a")}

        es.shutdown()
        es.awaitTermination(3, TimeUnit.SECONDS)
        as.terminate()
    }
}
