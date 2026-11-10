// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Monads for functional programming by Phil Wadler (Sections 1-2)

package adpro.eval

import adpro.eval.ExceptionEvaluator.M.Raise
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.*

// Equivalence is type checked stronger than Equality
// and its definition does not have issues with Matchable in Scala 3
import org.scalactic.TypeCheckedTripleEquals.*
import org.scalactic.Equivalence

// We first include implementation of Functor and Monad type classes for
// convenience (these are taken from the textbook repo, but should  be the same
// or very similar to ours from the monad week)
// You can scroll down until "end Monad"

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

  extension [A, B](fab: F[(A, B)])
    def distribute: (F[A], F[B]) =
      (fab.map(_(0)), fab.map(_(1)))

  extension [A, B](e: Either[F[A], F[B]])
    def codistribute: F[Either[A, B]] = e match
      case Left(fa) => fa.map(Left(_))
      case Right(fb) => fb.map(Right(_))

end Functor


trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: => A): F[A]

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join

    def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List[A]()))((fa, acc) => fa.map2(acc)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, acc) => f(a).map2(acc)(_ :: _))

  extension [A](ffa: F[F[A]]) def join: F[A] =
    ffa.flatMap(identity)

  object laws:

    /* The triple equality below (===) uses the Equality instance to check
     * equality (by default the same as == but we can override it). */

    def associative[A, B, C](using Arbitrary[F[A]], Arbitrary[A => F[B]],
      Arbitrary[B => F[C]], Equivalence[F[C]]): Prop =
      forAll { (x: F[A], f: A => F[B], g: B => F[C]) =>
        val left = x.flatMap[B](f).flatMap[C](g)
        val right = x.flatMap[C] { a => f(a).flatMap[C](g) }
        (left === right) :| s"left:$left right:$right"
      }

    def identityRight[A](using Arbitrary[F[A]], Arbitrary[A => F[A]], Equivalence[F[A]]) =
      forAll { (x: F[A], f: A => F[A]) =>
        val result = x.flatMap[A](unit[A])
        (result === x) :| s"got:$result expected:$x" }

    def identityLeft[A: Arbitrary](using Arbitrary[A => F[A]], Equivalence[F[A]]) =
      forAll { (y: A, f: A => F[A]) =>
        val left = unit[A](y).flatMap[A](f)
        val right = f(y)
        (left === right) :| s"left:$left right:$right" }

    def identity[A: Arbitrary](using Arbitrary[F[A]], Arbitrary[A => F[A]],
      Equivalence[F[A]]): Prop =
      { "identity left: " |: identityLeft[A]  } &&
      { "identity right:" |: identityRight[A] }

    def monad[A: Arbitrary, B, C] (using Arbitrary[F[A]], Arbitrary[A => F[A]],
      Arbitrary[A => F[B]], Arbitrary[B => F[C]]): Prop =
      { "associative:" |: this.associative[A,B,C] } &&
      { "identity:   " |: this.identity[A] }

  end laws

end Monad

// Section 2.1 [Wadler]
// This is Wadler's Term language in Scala:

enum Term:
  case Cons(value: Int)
  case Div(left: Term, right: Term)

import Term.*

object BasicEvaluator:
  def eval(term: Term): Int = term match
    case Cons(a) => a
    case Div(t,u) => eval(t) / eval(u)

// Section 2.2: Exceptions

object ExceptionEvaluator:

  // Exercise 1

  type Exception = String
  enum M[+A]:
    case Raise (e: Exception) extends M[Nothing]
    case Return (a: A)

  import M.*

  def eval(term: Term): M[Int] = term match {
    case Term.Cons(value) => Return(value)
    case Term.Div(left, right) =>
      eval(left) match {
        case Raise(e) => Raise(e)
        case Return(lv) =>
          eval(right) match {
            case Raise(e) => Raise(e)
            case Return(rv) =>
              if rv == 0
              then Raise("Division by zero")
              else Return (lv / rv)
          }
      }
  }

  // Exercise 2
  given mIsMonad: Monad[M] = new:
    def unit[A] (a: => A): M[A] = Return(a)
    extension [A](ma: M[A])
      override def flatMap[B](f: A => M[B]): M[B] = ma match
        case Raise(e) => Raise(e)
        case Return(a) => f(a)

  // Exercise 3
  def evalMonad(term: Term): M[Int] =
    term match
      case Term.Cons(value) => Return(value)
      case Term.Div(left, right) =>
        evalMonad(left).flatMap(lv =>
          evalMonad(right).flatMap(rv =>
            if rv == 0
            then Raise("Division by zero.")
            else Return(lv / rv)))

  // Exercise 4
  def evalForYield(term: Term): M[Int] =
    term match
      case Term.Cons(value) => Return(value)
      case Term.Div(left, right) =>
        val numbers = for
          lv <- evalForYield(left)
          rv <- evalForYield(right)
        yield (lv, rv)
        numbers.flatMap((lv, rv) =>
          if rv == 0
          then Raise("Division by zero.")
          else Return(lv / rv))


end ExceptionEvaluator


// Section 2.3: State

object StateEvaluator:

  type State = Int
  case class M[+A] (step: State => (A,State))

  // This is a week equivalence test that we need because
  // we cannot compare functions in a programming language
  given equiv: [A] => Equivalence[M[A]] = new:
    def areEquivalent(a1: M[A], a2: M[A]): Boolean =
      (-500 to 500).forall { n => a1.step(n) == a2.step(n) }


  // Exercise 5
  def eval (term: Term): M[Int] = term match
    case Term.Cons(value) => StateEvaluator.M(s => (value, s))
    case Term.Div(left, right) =>
      eval(left).flatMap(lv => eval(right).flatMap(rv =>
        StateEvaluator.M(s =>
          if rv == 0
          then throw new ArithmeticException("Division by zero.")
          else (lv / rv, s + 1))))

  // Exercise 6
  given mIsMonad: Monad[M] = new:
    def unit[A] (a: => A): M[A] = M[A](s => (a, s))
    extension [A](ma: M[A])
      override def flatMap[B](f: A => M[B]): M[B] =
        M[B](s =>
          val (a, s1) = ma.step(s)
          f(a).step(s1))

  // Exercise 7
  def tick: M[Unit] = M { (x: State) => ((), x+1) }

  def evalMonad (term: Term): M[Int] = term match
    case Term.Cons(value) => mIsMonad.unit(value)
    case Term.Div(left, right) =>
      evalMonad(left).flatMap(lv =>
        evalMonad(right).flatMap(rv =>
          if rv == 0
          then M(_ => throw new ArithmeticException("Division by zero."))
          else tick.flatMap(_ => mIsMonad.unit(lv/rv))
        )
      )

  // Exercise 8
  def evalForYield (term: Term): M[Int] = term match {
    case Term.Cons(value) => mIsMonad.unit(value)
    case Term.Div(left, right) =>
      val pair = for
        lv <- evalForYield(left)
        rv <- evalForYield(right)
      yield (lv, rv)
      pair.flatMap((l, r) =>
        if r == 0
        then M(_ => throw new ArithmeticException("Division by zero."))
        else tick.flatMap(_ => mIsMonad.unit(l/r))
      )
  }

end StateEvaluator

// Section 2.4 [Wadler] Variation three: Output

object OutputEvaluator:

  // We now define the state of an evaluator that outputs the results of
  // reductions
  type Output = String
  case class M[+A] (o: Output, a: A)

  // A helper function from the paper
  def line (a: Term) (v: Int): Output =
    "eval(" + a.toString + ") <= " + v.toString + "\n"

  // Exercise 9
  def eval (term: Term): M[Int] = ???

  // Exercise 10
  given mIsMonad: Monad[M] = ???

  // Exercise 11
  def out (o: Output): M[Unit] = M (o, ())

  def evalMonad (term: Term): M[Int] = ???

  // Exercise 12
  def evalForYield (term: Term): M[Int] = ???

  // Exercise 13
  // your notes below


// vim:cc=72:tw=72
