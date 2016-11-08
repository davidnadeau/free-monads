package demo

import cats._
import cats.data.Coproduct
import cats.free.Free

object SimpleCoproductDemo extends App {

  sealed trait Interact[A]
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String) extends Interact[Unit]

  case class User(id: String, password: String)

  sealed trait Auth[A]
  case class Login(id: String, password: String) extends Auth[User]
  case class HasPermission(user: User, permission: String) extends Auth[Boolean]

  type App[A] = Coproduct[Interact, Auth, A]

  def ask(prompt: String) = Free.inject[Interact, App](Ask(prompt))
  def tell(msg: String) = Free.inject[Interact, App](Tell(msg))
  def login(id: String, password: String) = Free.inject[Auth, App](Login(id, password))
  def hasPermission(user: User, permission: String) = Free.inject[Auth, App](HasPermission(user, permission))

  val program =
    for {
      id    <- ask("What's your userID?")
      pass  <- ask("What's your password?")
      user  <- login(id, pass)
      b     <- hasPermission(user, "KnowSecret")
      _     <- if (b) tell("secret code") else tell("go away")
    } yield ()

  val interactInterpreter: (Interact ~> Id) = new (Interact ~> Id) {
    override def apply[A](fa: Interact[A]): Id[A] = fa match {
      case Ask(prompt) =>
        println(prompt)
        readLine.asInstanceOf[A]
      case Tell(msg) =>
        println(msg).asInstanceOf[A]
    }
  }

  val authInterpreter: Auth ~> Id =
    new (Auth ~> Id) {
      def apply[A](fa: Auth[A]): Id[A] =
        fa match {
          case Login(id, password) => User(id, password).asInstanceOf[A]
          case HasPermission(user, permission) =>
            if (user.id == "david") true.asInstanceOf[A] else false.asInstanceOf[A]
        }
    }

  val interpreter = interactInterpreter or authInterpreter
  program.foldMap(interpreter)
}