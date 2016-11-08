package learning

import scala.collection.mutable
import scala.language.higherKinds

object TrivialRunar extends App {

  trait Functor[F[_]] {
    def map[A, B](a: F[A])(f: A => B): F[B]
  }

  implicit val kvsFunctor: Functor[KVS] = new Functor[KVS] {
    def map[A,B](a: KVS[A])(f: A => B) = a match {
      case Put(key, value, aa) => Put(key, value, f(aa))
      case Get(key, h) => Get(key, x => f(h(x)))
      case Delete(key, aa) => Delete(key, f(aa))
    }
  }
  class Free[F[_], A](implicit F: Functor[F]) {
    def flatMap[B](f: A => Free[F,B]): Free[F,B] = this match {
      case Done(a) => f(a)
      case More(k) => More[F, B](F.map(k)(_ flatMap f))
    }
    def map[B](f: A => B): Free[F, B] = flatMap(x => Done(f(x)))
  }

  case class Done[F[_]: Functor, A](a: A) extends Free[F, A]
  case class More[F[_]: Functor, A](k: F[Free[F,A]]) extends Free[F, A]


  sealed trait KVS[A]
  case class Put[A](key: String, value: String, a: A) extends KVS[A]
  case class Get[A](key: String, h: String => A) extends KVS[A]
  case class Delete[A](key: String, a: A) extends KVS[A]

  def put(k: String, v: String): Free[KVS, Unit] = More(Put(k, v, Done(())))
  def get(k: String): Free[KVS, String] = More(Get(k, v => Done(v)))
  def delete(k: String): Free[KVS, Unit] = More(Delete(k, Done(())))

  def runKVS[A](kvs: Free[KVS, A], table: Map[String, String]): Map[String, String] =
    kvs match {
      case More(Put(k, v, a)) => runKVS(a, table + (k -> v))
      case More(Get(k, f)) => runKVS(f(table(k)), table)
      case More(Delete(k, a)) => runKVS(a, table - k)
      case Done(a) =>  table
    }

  val program = for {
    _ <- put("1", "1")
    _ <- put("2", "2")
    _ <- put("3", "3")
    second <- get("2")
    _ <- delete("3")
  } yield ()

  println(runKVS(program, Map.empty[String, String]))
}

object UsingCats extends App {
  import cats._
  import cats.free.Free

  sealed trait KVSA[A]
  case class Put[A](key: String, value: String) extends KVSA[A]
  case class Get[A](key: String) extends KVSA[A]
  case class Delete[A](key: String) extends KVSA[A]

  type KVS[A] = Free[KVSA, A]

  def put[A](key: String, value: String) =
    Free.liftF[KVSA, Unit](Put(key, value))
  def get[A](key: String) =
    Free.liftF[KVSA, String](Get(key))
  def delete[A](key: String) =
    Free.liftF[KVSA, Unit](Delete(key))

  val program = for {
    _ <- put("1", "1")
    _ <- put("2", "2")
    _ <- put("3", "3")
    second <- get("2")
    _ <- delete("3")
  } yield ()

  def interp: KVSA ~> Id =
    new (KVSA ~> Id) {
    val table = mutable.Map.empty[String, Any]

    def apply[A](fa: KVSA[A]): Id[A] = fa match {
      case Put(k, v) =>
        table.put(k, v)
        ().asInstanceOf[A]
      case Get(k) =>
        table(k).asInstanceOf[A]
      case Delete(k) =>
        table.remove(k)
        ().asInstanceOf[A]
    }
  }
  println(program.foldMap(interp))
}