import cats.data.Coproduct
import cats.free.Inject

import scala.language.higherKinds

object Coproducts extends App {
  import cats._
  import cats.free.Free
  import algebra._
  import algebra.lift


  object algebra {
    implicit def lift[F[_], G[_], A](fa: F[A])(implicit I: Inject[F, G]): Free[G, A] =
      Free.liftF(I.inj(fa))

    sealed trait Calc[A]
    object Calc {
      case object Recall extends Calc[Int]
      case class Increment(i: Int) extends Calc[Unit]
      case object Clear extends Calc[Unit]

    }

    sealed trait Interact[A]
    object Interact {
      case class Ask(prompt: String) extends Interact[String]
      case class Tell(msg: String) extends Interact[Unit]
    }

    case class User(id: String, password: String)
    sealed trait Auth[A]
    object Auth {
      case class Login(id: String, password: String) extends Auth[User]
      case class HasPermission(user: User, permission: String) extends Auth[Boolean]
    }

    type M0[A] = Coproduct[Auth, Interact, A]
    type App[A] = Coproduct[Calc, M0, A]

  }

  def calcProgram[F[_]](implicit I: Inject[Interact, F], C: Inject[Calc, F]): Free[F, Int] = {
    import Interact._
    import Calc._

    for {
      m0 <- Recall
      _ <- Tell(s"starting at $m0")
      n0 <- Ask("give a number")
      _ <- Increment(n0.toInt)
      m1 <- Recall
      i <- Ask("continue?")
      m1 <- if (i == "y") calcProgram else Free.pure[F, Int](m1)
    } yield m1
  }

  def guard[F[_]](implicit A: Inject[Auth, F], I: Inject[Interact, F], C: Inject[Calc, F]): Free[F, Unit] = {
    import Interact._
    import Auth._

    for {
      id <- Ask("username?")
      pass <- Ask("password?")
      user <- Login(id, pass)
      b <- HasPermission(user, "")
      m1 <- if (b) calcProgram else Free.pure[F, String]("Can't use this program")
      _ <- Tell(m1.toString)
    } yield ()
  }

  def calcInterpreter: Calc ~> Id =
    new (Calc ~> Id) {
      import Calc._
      var memory = 0
      def apply[A](fa: Calc[A]) =
        fa match {
          case Recall =>
            memory.asInstanceOf[A]
          case Increment(i) =>
            memory = memory + i
            ()
          case Clear =>
            memory = 0
            ()
        }
    }
  def interactInterpreter: Interact ~> Id =
    new (Interact ~> Id) {
      import Interact._
      def apply[A](fa: Interact[A]): Id[A] =
        fa match {
          case Ask(prompt) =>
            println(prompt)
            readLine.asInstanceOf[A]
          case Tell(msg) =>
            println(msg).asInstanceOf[A]
        }
    }
  def authInterpreter: Auth ~> Id =
    new (Auth ~> Id) {
      import Auth._
      def apply[A](fa: Auth[A]): Id[A] =
        fa match {
          case Login(id, password) => User(id, password).asInstanceOf[A]
          case HasPermission(user, permission) =>
            if (user.id == "david") true.asInstanceOf[A] else false.asInstanceOf[A]
        }
    }

  val interpreter0: M0 ~> Id = authInterpreter or interactInterpreter
  val interpreter: App ~> Id = calcInterpreter or interpreter0
  val program: Free[App, Unit] = guard[App]

  println(program.foldMap(interpreter))
}

