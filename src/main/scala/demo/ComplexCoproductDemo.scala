package demo

import cats._
import cats.free._
import cats.data.Coproduct

import scala.language.higherKinds


object DSL {
  implicit def lift[F[_], G[_], A](fa: F[A])(implicit I: F :<: G): Free[G, A] =
    Free.inject[F, G](fa)(Inject[F, G])

  case class User(id: String, hash: String)

  sealed trait ProgramState
  case object Continue extends ProgramState
  case object End extends ProgramState

  sealed trait Store[A]
  object Store {
    case class SaveUser(user: User) extends Store[Boolean]
    case class GetUser(id: String) extends Store[Option[User]]
    case object GetUsers extends Store[List[User]]
  }

  sealed trait Interact[A]
  object Interact {
    case class Ask(prompt: String) extends Interact[String]
    case class Tell(msg: String) extends Interact[Unit]
  }

  sealed trait Auth[A]
  object Auth {
    case class Hash(password: String) extends Auth[String]
    case class CheckHash(password: String, hash: String) extends Auth[Boolean]
  }

  type M0[A] = Coproduct[Store, Interact, A]
  type PRG[A] = Coproduct[Auth, M0, A]
}

object Programs {
  import DSL._
  import Store._
  import Interact._
  import Auth._

  def createUser[F[_]](implicit I: Interact :<: F, A: Auth :<: F, S: Store :<: F): Free[F, ProgramState] = {
    for {
      id    <- Ask("What is your Id?")
      pass  <- Ask("What is your password?")
      hash  <- Hash(pass)
      user  =  User(id, hash)
      b     <- SaveUser(user)
      _     <- if (b) Tell("User created succesffuly") else Tell("User creation failed")
    } yield Continue
  }

  def isAuthenticated[F[_]](implicit I: Interact :<: F, A: Auth :<: F, S: Store :<: F): Free[F, ProgramState] = {
    for {
      id    <- Ask("What is your Id?")
      pass  <- Ask("What is your password?")
      oUser <- GetUser(id)
      b     <- oUser match {
        case Some(user) => CheckHash(pass, user.hash)
        case None => CheckHash("", "")
      }
      _     <- if (b) Tell(s"User $id is authenticated") else Tell(s"Invalid!")
    } yield Continue
  }

  def printUserTable[F[_]](implicit I: Interact :<: F, S: Store :<: F): Free[F, ProgramState] = {
    for {
      users <- GetUsers
      _     <- Tell("id\thash")
      _     <- Tell(users.map(u => s"${u.id}\t${u.hash}").mkString("\n"))
    } yield Continue
  }

  def quit[F[_]](implicit I: Interact :<: F, S: Store :<: F): Free[F, ProgramState] = {
    for {
      _     <- printUserTable
      _     <- Tell("Cya!")
    } yield End
  }

  def invalidSelection[F[_]](implicit I: Interact :<: F): Free[F, ProgramState] = {
    for {
      _     <- Tell("Not a valid selection")
    } yield Continue
  }

  def userProgram[F[_]](implicit I: Interact :<: F, A: Auth :<: F, S: Store :<: F): Free[F, Unit] = {
    for {
      res   <- Ask("Select an option from below\n1) Create a user\n2) Auth a user\n3) See existing users\n4) Quit")
      s     <- res match {
        case "1" => createUser
        case "2" => isAuthenticated
        case "3" => printUserTable
        case "4" => quit
        case _   => invalidSelection
      }
      _     <- s match {
        case Continue => userProgram
        case End => Free.pure[F, String]("end")
      }
    } yield ()
  }
}

object Interpreters {
  import DSL._
  import Store._
  import Interact._
  import Auth._

  def storeInterpreter: (Store ~> Id) = new (Store ~> Id) {
    import scala.collection.mutable
    val store = mutable.Map.empty[String, User]

    override def apply[A](fa: Store[A]): Id[A] = fa match {
      case SaveUser(user) =>
        store.put(user.id, user)
        true.asInstanceOf[A]
      case GetUser(id) =>
        store.get(id).asInstanceOf[A]
      case GetUsers =>
        store.values.toList.asInstanceOf[A]
    }
  }

  def interactInterpreter: (Interact ~> Id) = new (Interact ~> Id) {
    override def apply[A](fa: Interact[A]): Id[A] = fa match {
      case Ask(prompt) =>
        println(prompt)
        readLine.asInstanceOf[A]
      case Tell(msg) =>
        println(msg).asInstanceOf[A]
    }
  }

  def authInterpreter: (Auth ~> Id) = new (Auth ~> Id) {
    import java.security.MessageDigest
    def md5(s: String): String = MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02x".format(_)).mkString

    override def apply[A](fa: Auth[A]): Id[A] = fa match {
      case Hash(password) => md5(password).asInstanceOf[A]
      case CheckHash(password, hash) => (md5(password) == hash).asInstanceOf[A]
    }
  }
}

object Main extends App {
  import DSL._
  import Programs._
  import Interpreters._

  val interpreter0: M0 ~> Id = storeInterpreter or interactInterpreter
  val interpreter: PRG ~> Id = authInterpreter or interpreter0
  val program: Free[PRG, Unit] = userProgram[PRG]

  program.foldMap(interpreter)
}
