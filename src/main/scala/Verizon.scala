import cats.Functor
import cats.free.{Coyoneda, Free}

import scala.language.{higherKinds, reflectiveCalls}

object Verizon {

//  sealed trait Free[F[_], A]
//  case class Return[F[_], A](a: A) extends Free[F, A]
//  case class Suspend[F[_], A](s: F[Free[F, A]]) extends Free[F, A]
//
//  abstract class Yoneda[F[_], A] {
//    def apply[B](f: A => B): F[B]
//
//    def toYoneda[F[_] : Functor](fa: F[A]) =
//      new Yoneda[F, A] {
//        def apply[B](f: A => B) = Functor[F].map(fa)(f)
//      }
//  }
//
//  def fromYoneda[F[_], A](yo: Yoneda[F, A]) =
//    yo.apply(identity)
//
//  sealed abstract class Coyoneda[F[_], A] {
//    type I
//    val fi: F[I]
//    val k: I => A
//  }
//
//  type FreeC[S[_], A] = Free[({type f[x] = Coyoneda[S, x]})#f, A]

  sealed abstract class SchedulerOp[A] extends Product with Serializable
  final case class Delete(s: String) extends SchedulerOp[Unit]
  final case class Launch(s: String) extends SchedulerOp[Unit]

  type SchedulerF[A] = Free[SchedulerOp, A]
  def delete(s: String): SchedulerF[Unit] =
    Free.liftF(Delete(s))



}
