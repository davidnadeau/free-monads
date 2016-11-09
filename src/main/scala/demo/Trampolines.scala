package demo

import cats.implicits._
import cats.free.Trampoline
import Trampoline._

object Trampolines extends App {
  def overFlow(option: Option[Int], n: Int): Option[Int] =
    if (n <= 1) option.flatMap(x => Option(x + 1))
    else option.flatMap(x => overFlow(Option(x + 1), n - 1))

  def safe(option: Trampoline[Option[Int]], n: Int): Trampoline[Option[Int]] =
    if (n <= 1)
      option.flatMap(oInt =>
        done(oInt.flatMap(int =>
          Option(int + 1))))
    else
      suspend(safe(option.flatMap(oInt =>
        done(oInt.flatMap(int =>
          Option(int + 1)))), n - 1))

  println(overFlow(Option(0), 100))
//  println(overFlow(Option(1), 1000000))
  println(safe(done(Option(1)), 1000000).run)
// run takes an implicit comonad (cats provides a comonad for option)

  def safeEven[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil => done(true)
      case x :: xs => suspend(odd(xs))
    }
  def odd[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil => done(false)
      case x :: xs => suspend(safeEven(xs))
    }

  def unsafeEven[A](ns: List[A]): Boolean = ns match {
    case Nil => true
    case x :: xs  => unsafeOdd(xs)
  }
  def unsafeOdd[A](ns: List[A]): Boolean = ns match {
    case Nil => false
    case x :: xs  => unsafeEven(xs)
  }

    val range = 0 to 3000
    println(safeEven(range.toList).run)
    println(unsafeEven(range.toList))
}
