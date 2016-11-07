import cats._
import cats.implicits._
import cats.free.Free

object FreeList extends App {

  sealed trait ListPrimeA[A]
  case class Cons[A](h: A, t: List[A]) extends ListPrimeA[List[A]]
  case class Rm[A](h: A, t: List[A]) extends ListPrimeA[List[A]]
  type ListPrime[A] = Free[ListPrimeA, A]

  def cons[A](h: A, t: List[A]) =
    Free.liftF[ListPrimeA, List[A]](Cons(h, t))

  def rm[A](h: A, t: List[A]) =
    Free.liftF[ListPrimeA, List[A]](Rm(h, t))


  def program =
    for {
      n <- cons(1, Nil)
      n <- cons(2, n)
      n <- rm(1, n)
    } yield {
      println(n)
      ()
    }

  def intr = new (ListPrimeA ~> Id) {
    def apply[A](fa: ListPrimeA[A]): Id[A] = fa match {
      case Cons(h, t) => (h :: t).asInstanceOf[A]
      case Rm(h, t) => t.filter(_ != h).asInstanceOf[A]
    }
  }

  program.foldMap(intr)

}

