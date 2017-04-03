package org.kitsuneninetails.scalaPlayground

import scala.annotation.tailrec

trait StateDescription
trait Action {
    def apply(cs: StateDescription): StateDescription
}

case object StateEngine {
    @tailrec
    def action(cs: StateDescription, actions: List[Action]): StateDescription =
        actions match {
            case Nil => cs
            case h :: t =>
                println(s"$cs => ${h(cs)}")
                action(h(cs), t)
        }
}

object CandyMachine {
    case class Machine(locked: Boolean, coins: Int, candies: Int) extends StateDescription {
        override def toString: String = s"Locked=$locked (coins=$coins, candy=$candies)"
    }

    val insertCoin: Action = {
        case Machine(true, x, y) if x > 0 => Machine(locked = false, x + 1, y)
        case m => m
    }

    val turnKnob: Action = {
        case Machine(false, x, y) if x > 0 => Machine(locked = true, x, y - 1)
        case m => m
    }
}

object StateMachine {
    def main(args: Array[String]): Unit = {
        import CandyMachine._
        val actions: List[Action] = insertCoin :: turnKnob ::
                                    insertCoin :: turnKnob ::
                                    insertCoin :: turnKnob ::
                                    insertCoin :: turnKnob ::
                                    turnKnob ::
                                    Nil

        println(s"Final state: ${StateEngine.action(Machine(locked = true, 10, 5), actions)}")
    }
}
