package learning

import scala.language.higherKinds

object AlaCarte extends App {

  sealed trait Expr[A]
  case class Val[A](x: A) extends Expr[A]
  case class Add[A](e0: Expr[A], e1: Expr[A]) extends Expr[A]

  def render[A](exp: Expr[A]): String = exp match {
    case Val(x) => x.toString
    case Add(e0, e1) => "(" + render[A](e0) + " + " + render[A](e1) + ")"
  }
  println(render(Add(Add(Val(1), Val(2)), Val(3))))
}

