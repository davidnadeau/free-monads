package demo

import scala.language.higherKinds

object Evolution0 {
  sealed trait Interact[A]
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String) extends Interact[Unit]

  val prg = List(
    Ask("what's your first name"),
    Ask("what's your last name"),
    Tell("Hello ???")
  )
}

object Evolution1 extends App {
  sealed trait Interact[A]
  case class Ask[A](prompt: A) extends Interact[A]
  case class Tell[A](msg: A) extends Interact[A]

  trait Functor[F[_]] {
    def map[A, B](a: F[A])(f: A => B): F[B]
  }

  val interactFunctor: Functor[Interact] = new Functor[Interact] {
    override def map[A, B](a: Interact[A])(f: A => B): Interact[B] = a match {
      case Ask(prompt) => Ask(f(prompt))
      case Tell(msg) => Tell(f(msg))
    }
  }
  println(interactFunctor.map(Ask("what's your first name"))(_.length))
  println(interactFunctor.map(Tell("Hello"))(_.length))

//  val optionFunctor: Functor[Option] = new Functor[Option] {
//    override def map[A, B](a: Option[A])(f: A => B): Option[B] = a match {
//      case Some(x) => Some(f(x))
//      case None => None
//    }
//  }
//  val a: Option[Int] = Option(1)
//  println(optionFunctor.map[Int, Int](a)(_ * 100))

//  val b: Option[Int] = Option(1)
//  val c: Option[Int] = Option(2)
//  println(optionFunctor.map[Int, Int](b)(x => optionFunctor.map[Int, Int](c)(y => x + y)))
}

object SimpleFreeMonadDemo extends App {
//
//  trait Monad[M[_]] {
//    def pure[A](a: A): M[A]
//    def flatMap[A,B](a: M[A])(f: A => M[B]): M[B]
//  }
//
//  val optionMonad: Monad[Option] = new Monad[Option] {
//    override def pure[A](a: A): Option[A] = Some(a)
//    override def flatMap[A, B](a: Option[A])(f: A => Option[B]): Option[B] = a match {
//      case Some(x) => f(x)
//      case None => None
//    }
//  }
//  val a: Option[Int] = Option(1)
//  println(optionMonad.flatMap[Int, Int](a)(x => Option(x * 100)))
//
//  val b: Option[Int] = Option(1)
//  val c: Option[Int] = Option(2)
//  println(optionMonad.flatMap[Int, Int](b)(x => optionMonad.flatMap[Int, Int](c)(y => Option(x + y))))


  import cats._
  import cats.free.Free

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