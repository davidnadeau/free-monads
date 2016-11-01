import cats._
import cats.data.Coproduct
import cats.free._
import cats.free.Free.liftF

import scala.collection.mutable

object freer extends App {

  sealed trait KVStoreA[A]
  case class Put[T](key: String, value: T) extends KVStoreA[Unit]
  case class Get[T](key: String) extends KVStoreA[Option[T]]
  case class Delete(key: String) extends KVStoreA[Unit]

  type KVStore[A] = Free[KVStoreA, A]

  // Put returns nothing (i.e. Unit).
  def put[T](key: String, value: T): KVStore[Unit] =
    liftF[KVStoreA, Unit](Put[T](key, value))

  // Get returns a T value.
  def get[T](key: String): KVStore[Option[T]] =
    liftF[KVStoreA, Option[T]](Get[T](key))

  // Delete returns nothing (i.e. Unit).
  def delete(key: String): KVStore[Unit] =
    liftF(Delete(key))

  // Update composes get and set, and returns nothing.
  def update[T](key: String, f: T => T): KVStore[Unit] =
  for {
    vMaybe <- get[T](key)
    _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
  } yield ()

  def program: KVStore[Option[Int]] =
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", (_ + 12))
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n

  // the program will crash if a key is not found,
  // or if a type is incorrectly specified.
  def impureCompiler: KVStoreA ~> Id =
  new (KVStoreA ~> Id) {

    // a very simple (and imprecise) key-value store
    val kvs = mutable.Map.empty[String, Any]

    def apply[A](fa: KVStoreA[A]): Id[A] =
      fa match {
        case Put(key, value) =>
          println(s"put($key, $value)")
          kvs(key) = value
          ()
        case Get(key) =>
          println(s"get($key)")
          kvs.get(key).map(_.asInstanceOf[A])
        case Delete(key) =>
          println(s"delete($key)")
          kvs.remove(key)
          ()
      }
  }

  val result: Option[Int] = program.foldMap(impureCompiler)
  println(result)
}

object Runar extends App {

  sealed trait InteractA[A]
  case class Ask(prompt: String) extends InteractA[String]
  case class Tell(msg: String) extends InteractA[Unit]

  type Interact[A] = Free[InteractA, A]

  def ask(prompt: String): Interact[String] =
    liftF[InteractA, String](Ask(prompt))

  def tell(msg: String): Interact[Unit] =
    liftF[InteractA, Unit](Tell(msg))

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
  import dsl._
  import dsl.AuthActions._
  import dsl.InteractActions._
  def program(implicit I: InteractActions[App], A: AuthActions[App]) = {
    import I._, A._
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