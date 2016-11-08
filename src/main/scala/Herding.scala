import cats._
import cats.instances.all._

import scala.language.higherKinds

object Herding extends App {

  sealed trait CharToy[+Next]

  object CharToy {
    case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]
    case class CharBell[Next](next: Next) extends CharToy[Next]
    case class CharDone() extends CharToy[Nothing]
    def output[Next](a: Char, next: Next): CharToy[Next] = CharOutput(a, next)
    def bell[Next](next: Next): CharToy[Next] = CharBell(next)
    def done: CharToy[Nothing] = CharDone()
  }

  import CharToy._

  println(bell(output('A', done)))


  case class Fix[F[_]](f: F[Fix[F]])

  object Fix {
    def fix(toy: CharToy[Fix[CharToy]]) = Fix[CharToy](toy)
  }

  import Fix._, CharToy._

  println(output('A', fix(done)))
  println(fix(bell(fix(output('A', fix(done))))))
}

object MoreCats extends App {

  sealed trait CharToy[+Next]

  object CharToy {
    case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]
    case class CharBell[Next](next: Next) extends CharToy[Next]
    case class CharDone() extends CharToy[Nothing]
    def output[Next](a: Char, next: Next): CharToy[Next] = CharOutput(a, next)
    def bell[Next](next: Next): CharToy[Next] = CharBell(next)
    def done: CharToy[Nothing] = CharDone()
  }

  import CharToy._

  sealed trait FixE[F[_], E]
  object FixE {
    case class Fix[F[_], E](f: F[FixE[F, E]]) extends FixE[F, E]
    case class Throwy[F[_], E](e: E) extends FixE[F, E]

    def fix[E](toy: CharToy[FixE[CharToy, E]]): FixE[CharToy, E] =
      Fix[CharToy, E](toy)
    def throwy[F[_], E](e: E): FixE[F, E] = Throwy(e)
    def catchy[F[_]: Functor, E1, E2](ex: => FixE[F, E1])
                                     (f: E1 => FixE[F, E2]): FixE[F, E2] = ex match {
      case Fix(x)    => Fix[F, E2](Functor[F].map(x) {catchy(_)(f)})
      case Throwy(e) => f(e)
    }
  }
  implicit val charToyFunctor: Functor[CharToy] = new Functor[CharToy] {
    def map[A, B](fa: CharToy[A])(f: A => B): CharToy[B] = fa match {
      case o: CharToy.CharOutput[A] => CharToy.CharOutput(o.a, f(o.next))
      case b: CharToy.CharBell[A]   => CharToy.CharBell(f(b.next))
      case CharToy.CharDone()       => CharToy.CharDone()
    }
  }
  import FixE._
  case class IncompleteException()
  def subroutine = fix[IncompleteException](
    output('A',
      throwy[CharToy, IncompleteException](IncompleteException())))
  def program = catchy[CharToy, IncompleteException, Nothing](subroutine) { _ =>
    fix[Nothing](bell(fix[Nothing](done)))
  }
  println(subroutine)

}

object MostCats extends App {
  import cats.free.Free
  sealed trait CharToy[+Next]
  object CharToy {
    case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]
    case class CharBell[Next](next: Next) extends CharToy[Next]
    case class CharDone() extends CharToy[Nothing]

    implicit val charToyFunctor: Functor[CharToy] = new Functor[CharToy] {
      def map[A, B](fa: CharToy[A])(f: A => B): CharToy[B] = fa match {
        case o: CharOutput[A] => CharOutput(o.a, f(o.next))
        case b: CharBell[A]   => CharBell(f(b.next))
        case CharDone()       => CharDone()
      }
    }
    def output(a: Char): Free[CharToy, Unit] =
      Free.liftF[CharToy, Unit](CharOutput(a, ()))
    def bell: Free[CharToy, Unit] = Free.liftF[CharToy, Unit](CharBell(()))
    def done: Free[CharToy, Unit] = Free.liftF[CharToy, Unit](CharDone())
    def pure[A](a: A): Free[CharToy, A] = Free.pure[CharToy, A](a)
  }


  import CharToy._
  val subroutine = output('A')
  val program = for {
    _ <- subroutine
    _ <- bell
    _ <- done
  } yield ()

  def showProgram[R: Show](p: Free[CharToy, R]): String =
    p.fold({ r: R => "return " + Show[R].show(r) + "\n" },
      {
        case CharOutput(a, next) =>
          "output " + Show[Char].show(a) + "\n" + showProgram(next)
        case CharBell(next) =>
          "bell " + "\n" + showProgram(next)
        case CharDone() =>
          "done\n"
      })

  println(showProgram(program))

}