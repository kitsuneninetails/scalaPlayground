package org.kitsuneninetails.scalaPlayground

abstract class Lifeform {
    def eat(): Unit
}

abstract class Animal extends Lifeform {
    override def toString: String = "Animal"
    def classification(): String
}
class Cat extends Animal {
    override def toString: String = "Cat"
    def eat(): Unit = {println("Cat eating")}
    def litterBox(): Unit = {println("Cat going potty")}
    def classification(): String = "feline"
}
class Dog extends Animal {
    override def toString: String = "Dog"
    def eat(): Unit = {println("Dog eating")}
    def lampPost(): Unit = {println("Dog going potty")}
    def classification(): String = "canine"
}

class Crate[+T <: Animal](val res: T) {
    def isType[B >: T <: Animal](a: B): Boolean = a.classification == res.classification
    def release(): T = res
}

object Crate {
    def apply[T <: Animal](a: T): Crate[T] = new Crate(a)
}

class Shelter[T <: Animal](res: List[T]) {
    def take(n: Crate[T]): Shelter[T] = new Shelter[T](n.release() :: this.res)
    def take(ns: List[Crate[T]]): Shelter[T] = new Shelter(ns.map(_.release()) ::: this.res)
    override def toString: String = res.toString()
    def feed(): Unit = res.foreach(_.eat())
}

object Variance {
    def main(args: Array[String]): Unit = {
        val crates = new Crate(new Dog) :: new Crate(new Cat) :: Nil
        val crate = Crate(new Dog)
        println(s"${crate.isType(new Dog)}")
        println(s"${crate.isType(new Cat)}")
        var a = crate.release().lampPost()

        val cats: List[Cat] = new Cat :: new Cat :: Nil

        val s = new Shelter[Animal](cats).take(crates).take(crate)
        println(s"$s")
        s.feed()
    }
}
