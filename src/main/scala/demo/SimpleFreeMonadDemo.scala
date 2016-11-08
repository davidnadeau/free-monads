package demo

import cats._
import cats.free.Free

object SimpleFreeMonadDemo extends App {
  sealed trait InteractA[A]
  case class Ask(prompt: String) extends InteractA[String]
  case class Tell(msg: String) extends InteractA[Unit]

  type Interact[A] = Free[InteractA, A]
  def ask(prompt: String): Interact[String] = Free.liftF[InteractA, String](Ask(prompt))
  def tell(msg: String): Interact[Unit] = Free.liftF[InteractA, Unit](Tell(msg))

  def program: Free[InteractA, Unit] = for {
    first <- ask("What's your first name?")
    last  <- ask("What's your last name?")
  _       <- tell(s"Hello $first $last!")
  } yield ()

  val interpreter: (InteractA ~> Id) = new (InteractA ~> Id) {
    override def apply[A](fa: InteractA[A]): Id[A] = fa match {
      case Ask(prompt) =>
        println(prompt)
        readLine.asInstanceOf[A]
      case Tell(msg) =>
        println(msg).asInstanceOf[A]
    }
  }

  program.foldMap(interpreter)
}