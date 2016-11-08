package demo

import scala.language.higherKinds

object MathExprDemo extends App {
  sealed trait Expr
  case class Val(x: Int) extends Expr
  case class Add(e0: Expr, e1: Expr) extends Expr
  case class Sub(e0: Expr, e1: Expr) extends Expr

  def render(exp: Expr): String = exp match {
    case Val(x) => x.toString
    case Add(e0, e1) => "(" + render(e0) + " + " + render(e1) + ")"
    case Sub(e0, e1) => "(" + render(e0) + " - " + render(e1) + ")"
  }
  def eval(exp: Expr): Int = exp match {
    case Val(x) => x
    case Add(e0, e1) => eval(e0) + eval(e1)
    case Sub(e0, e1) => eval(e0) - eval(e1)
  }

  val simpleExpr = Add(Val(1), Val(1))
  println(render(simpleExpr))
  println(eval(simpleExpr))

  val complexExpr = Sub(Add(Val(1), Val(1)), Val(1))
  println(render(complexExpr))
  println(eval(complexExpr))
}

object MathExprEarlier extends App {
  trait Functor[F[_]] {
    def map[A, B](a: F[A])(f: A => B): F[B]
  }

  sealed trait Expr[A]
  case class Val[A](x: Int, a: A) extends Expr[A]
  case class Add[A](e0: Int, e1: Int, a: Int => A) extends Expr[A]
  case class Sub[A](e0: Int, e1: Int, a: Int => A) extends Expr[A]

  implicit val kvsFunctor: Functor[Expr] = new Functor[Expr] {
    def map[A,B](a: Expr[A])(f: A => B) = a match {
      case Val(x, aa) => Val(x, f(aa))
      case Add(e0, e1, aa) => Add(e0, e1, v => f(aa(v)))
      case Sub(e0, e1, aa) => Sub(e0, e1, v => f(aa(v)))
    }
  }

  case class Done[F[_]: Functor, A](a: A) extends Free[F, A]
  case class More[F[_]: Functor, A](k: F[Free[F,A]]) extends Free[F, A]
  class Free[F[_], A](implicit F: Functor[F]) {
    def flatMap[B](f: A => Free[F,B]): Free[F,B] = this match {
      case Done(a) => f(a)
      case More(k) => More[F, B](F.map(k)(_ flatMap f))
    }
    def map[B](f: A => B): Free[F, B] = flatMap(x => Done(f(x)))
  }

  def vall(a: Int): Free[Expr, Int] = More(Val(a, Done(a)))
  def add(a0: Int, a1: Int): Free[Expr, Int] = More(Add(a0, a1, a => Done(a)))
  def sub(a0: Int, a1: Int): Free[Expr, Int] = More(Sub(a0, a1, a => Done(a)))

  def runEvalExpr[A](expr: Free[Expr, A]): A = expr match {
    case More(Add(a0,a1,c)) => runEvalExpr(c(a0 + a1))
    case More(Sub(a0,a1,c)) => runEvalExpr(c(a0 - a1))
    case More(Val(a0,c)) => runEvalExpr(c)
    case Done(a) => a
  }

  val simpleProgram = for {
    n <- vall(1)
    m <- add(n,n)
  } yield m

  val complexProgram = for {
    n <- vall(1)
    m <- add(n,n)
    o <- sub(m, n)
  } yield o

  println(runEvalExpr(simpleProgram))
  println(runEvalExpr(complexProgram))
}

object MathExprFree extends App {
  import cats._
  import cats.free.Free

  sealed trait Expr[A]
  case class Val[A](x: A) extends Expr[A]
  case class Add[A](e0: A, e1: A) extends Expr[A]
  case class Sub[A](e0: A, e1: A) extends Expr[A]

  def vall[A](x: A) = Free.liftF[Expr, A](Val(x))
  def add[A](x: A, y: A) = Free.liftF[Expr, A](Add(x, y))
  def sub[A](x: A, y: A) = Free.liftF[Expr, A](Sub(x, y))

  def simpleProgram[A] = for {
    n <- vall(1.asInstanceOf[A])
    m <- add(n, n)
  } yield m

  def complexProgram[A] = for {
    n <- vall(1.asInstanceOf[A])
    m <- add(n, n)
    o <- sub(m, n)
  } yield o

  val renderInterpreter = new (Expr ~> Id) {
    override def apply[A](fa: Expr[A]): Id[A] = fa match {
      case Val(x) => x.asInstanceOf[A]
      case Add(e0, e1) => ("(" + e0 + " + " + e1 + ")").asInstanceOf[A]
      case Sub(e0, e1) => ("(" + e0 + " - " + e1 + ")").asInstanceOf[A]
    }
  }

  val evalInterpreter = new (Expr ~> Id) {
    override def apply[A](fa: Expr[A]): Id[A] = fa match {
      case Val(x) => x.asInstanceOf[A]
      case Add(e0, e1) => (e0.asInstanceOf[Int] + e1.asInstanceOf[Int]).asInstanceOf[A]
      case Sub(e0, e1) => (e0.asInstanceOf[Int] - e1.asInstanceOf[Int]).asInstanceOf[A]
    }
  }

  println(simpleProgram.foldMap(renderInterpreter))
  println(simpleProgram.foldMap(evalInterpreter))
  println(complexProgram.foldMap(renderInterpreter))
  println(complexProgram.foldMap(evalInterpreter))

}