import cats.data.Coproduct
import cats.free.Inject

object Coproducts extends App {
  import cats._
  import cats.free.Free
  import algebra._, dsl._


  object algebra {
    sealed trait Calc[A]
    case object Recall extends Calc[Int]
    case class Increment(i: Int) extends Calc[Unit]
    case object Clear extends Calc[Unit]

    sealed trait Interact[A]
    case class Ask(prompt: String) extends Interact[String]
    case class Tell(msg: String) extends Interact[Unit]

    case class User(id: String, password: String)
    sealed trait Auth[A]
    case class Login(id: String, password: String) extends Auth[User]
    case class HasPermission(user: User, permission: String) extends Auth[Boolean]

    type M0[A] = Coproduct[Auth, Interact, A]
    type App[A] = Coproduct[Calc, M0, A]
  }

  object dsl {

    class CalcActions[F[_]](implicit I: Inject[Calc, F]) {
      def recall = Free.inject[Calc, F](Recall)
      def increment(i: Int) = Free.inject[Calc, F](Increment(i))
      def clear = Free.inject[Calc, F](Clear)
    }
    object CalcActions {
      implicit def calcActions[F[_]](implicit I: Inject[Calc, F]): CalcActions[F] = new CalcActions[F]()
    }

    class InteractActions[F[_]](implicit I: Inject[Interact, F]) {
      def ask(prompt: String) = Free.inject[Interact, F](Ask(prompt))
      def tell(msg: String) = Free.inject[Interact, F](Tell(msg))
    }

    object InteractActions {
      implicit def interactActions[F[_]](implicit I: Inject[Interact, F]): InteractActions[F] = new InteractActions[F]()
    }

    class AuthActions[F[_]](implicit I: Inject[Auth, F]) {
      def login(id: String, password: String): Free[F, User] = Free.inject[Auth, F](Login(id, password))
      def hasPermission(user: User, permission: String): Free[F, Boolean] = Free.inject[Auth, F](HasPermission(user, permission))
    }
    object AuthActions {
      implicit def authActions[F[_]](implicit I: Inject[Auth, F]): AuthActions[F] = new AuthActions[F]()
    }

  }
  def calcProgram(implicit I: InteractActions[App], C: CalcActions[App]): Free[App, Int] = {
    import I._, C._

    for {
      m0 <- recall
      _ <- tell(s"starting at $m0")
      n0 <- ask("give a number")
      _ <- increment(n0.toInt)
      m1 <- recall
      i <- ask("continue?")
      m1 <- if (i == "y") calcProgram else recall
    } yield m1

  }
  def guard(implicit A: AuthActions[App], I: InteractActions[App], C: CalcActions[App]): Free[App, Unit] = {
    import I._, A._, C._

    for {
      id <- ask("username?")
      pass <- ask("password?")
      user <- login(id, pass)
      b <- hasPermission(user, "")
      m1 <- if (b) calcProgram else tell("can't use it")
    } yield ()
  }

  def calcInterpreter: Calc ~> Id =
    new (Calc ~> Id) {
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

      def apply[A](fa: Auth[A]): Id[A] =
        fa match {
          case Login(id, password) => User(id, password).asInstanceOf[A]
          case HasPermission(user, permission) =>
            if (user.id == "david") true.asInstanceOf[A] else false.asInstanceOf[A]
        }
    }

  val interpreter0: M0 ~> Id = authInterpreter or interactInterpreter
  val interpreter: App ~> Id = calcInterpreter or interpreter0
  println(guard.foldMap(interpreter))
}

