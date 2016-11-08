package learning

import cats._
import cats.data.Coproduct
import cats.free._

object Runar extends App {

  sealed trait InteractA[A]
  case class Ask(prompt: String) extends InteractA[String]
  case class Tell(msg: String) extends InteractA[Unit]

  type Interact[A] = Free[InteractA, A]

  def ask(prompt: String): Interact[String] =
    Free.liftF[InteractA, String](Ask(prompt))

  def tell(msg: String): Interact[Unit] =
    Free.liftF[InteractA, Unit](Tell(msg))

  def program: Interact[Unit] =
    for {
      first <- ask("What's your first name?")
      last <- ask("What's your last name?")
      _ <- tell(s"hello $first $last")
    } yield ()

  def interactInterpreter: InteractA ~> Id =
    new (InteractA ~> Id) {

      def apply[A](fa: InteractA[A]): Id[A] =
        fa match {
          case Ask(prompt) =>
            println(prompt)
            readLine.asInstanceOf[A]
          case Tell(msg) =>
            println(msg).asInstanceOf[A]
        }
    }

  implicit val testerMonad = new Monad[Tester] {
    override def pure[A](a: A): Tester[A] = _ => (List(), a)
    override def flatMap[A, B](fa: Tester[A])(f: A => Tester[B]): Tester[B] =
      m => {
        val (o1, a) = fa(m)
        val (o2, b) = f(a)(m)
        (o1 ++ o2, b)
      }

    override def tailRecM[A, B](a: A)(f: A => Tester[Either[A, B]]): Tester[B] = m => {
      flatMap(f(a)) {
        case Right(b) => pure(b)
        case Left(nextA) => tailRecM(nextA)(f)
      }.apply(m)
    }
  }

  type Tester[A] = Map[String, String] => (List[String], A)
  def testInteractInterpreter: InteractA ~> Tester =
    new (InteractA ~> Tester) {

      def apply[A](fa: InteractA[A]) =
        fa match {
          case Ask(prompt) => m => (List(), m(prompt).asInstanceOf[A])
          case Tell(msg) => _ => (List(msg), ().asInstanceOf[A])
        }
    }
//  val result = program.foldMap(interactInterpreter)
  val testResult = program.foldMap(testInteractInterpreter)
    .apply(Map(
      "What's your first name?" -> "david",
      "What's your last name?" -> "nadeau"))
//  println(result)
  println(testResult)
}

object TheMoney extends App {

  sealed trait Interact[A]
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String) extends Interact[Unit]

  case class User(id: String, password: String)
  sealed trait Auth[A]
  case class Login(id: String, password: String) extends Auth[User]
  case class HasPermission(user: User, permission: String) extends Auth[Boolean]

  type App[A] = Coproduct[Interact, Auth, A]

  object dsl {
    class InteractActions[F[_]](implicit I: Inject[Interact, F]) {
      def ask(prompt: String): Free[F, String] = Free.inject[Interact, F](Ask(prompt))
      def tell(msg: String): Free[F, Unit] = Free.inject[Interact, F](Tell(msg))
    }
    object InteractActions {
      implicit def interactActions[F[_]](implicit I: Inject[Interact, F]): InteractActions[F] = new InteractActions[F]
    }
    class AuthActions[F[_]](implicit I: Inject[Auth, F]) {
      def login(id: String, password: String): Free[F, User] = Free.inject[Auth, F](Login(id, password))
      def hasPermission(user: User, permission: String): Free[F, Boolean] = Free.inject[Auth, F](HasPermission(user, permission))
    }
    object AuthActions {
      implicit def authActions[F[_]](implicit I: Inject[Auth, F]): AuthActions[F] = new AuthActions[F]
    }

  }
  import dsl.InteractActions._
  import dsl._
  def program(implicit I: InteractActions[App], A: AuthActions[App]) = {
    import A._
    import I._
    for {
      id <- ask("What's your userID?")
      password <- ask("What's your password?")
      user <- login(id, password)
      b <- hasPermission(user, "KnowSecret")
      _ <- if (b) tell("secret code") else tell("go away")
    } yield ()
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

  val appInterpreter = interactInterpreter or authInterpreter

  program.foldMap(appInterpreter)
}