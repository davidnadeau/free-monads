package demo

import cats._
import cats.implicits
import cats.free.Free

object Testing extends App {

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

