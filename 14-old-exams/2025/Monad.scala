package adpro
package monad

import parsing.*
import testing.*
import par.*
import state.*
import par.Par.*

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.*
import org.scalacheck.Prop.given
import org.scalactic.Equality
import org.scalactic.TripleEquals.*


trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

  extension [A, B](fab: F[(A, B)])
    def distribute: (F[A], F[B])=
      (fab.map(_._1), fab.map(_._2))

  extension [A, B](e: Either[F[A], F[B]])
    def codistribute: F[Either[A, B]] = e match
      case Left(fa) => fa.map { Left(_) }
      case Right(fb) => fb.map { Right(_) }

  object functorLaws:

    /* The triple equality below (===) uses the Equality instance to check
     * equality (by default the same as == but we can override it). */

    def map[A](using Arbitrary[F[A]], Equality[F[A]]): Prop =
      forAll { (fa: F[A]) => fa.map[A](identity[A]) === fa }

  end functorLaws

end Functor


val listFunctor: Functor[List] = new:
  extension [A] (as: List[A])
    def map[B](f: A => B): List[B] = as.map(f)

trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: => A): F[A]

  extension [A](fa: F[A])
    def flatMap[B] (f: A => F[B]): F[B]

    def map[B](f: A => B): F[B] =
      fa.flatMap[B] { a => unit(f(a)) }

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap { a => fb.map { b => f(a,b) } }

  object monadLaws:

    /* The triple equality below (===) uses the Equality instance to check
     * equality (by default the same as == but we can override it). */

    def associative[A, B, C](using Arbitrary[F[A]], Arbitrary[A => F[B]],
      Arbitrary[B => F[C]], Equality[F[C]]): Prop =
      forAll { (x: F[A], f: A => F[B], g: B => F[C]) =>
        val left = x.flatMap[B](f).flatMap[C](g)
        val right = x.flatMap[C] { a => f(a).flatMap[C](g) }
        (left === right) :| s"left:$left right:$right"
      }

    def identityRight[A](using Arbitrary[F[A]], Arbitrary[A => F[A]], Equality[F[A]]) =
      forAll { (x: F[A], f: A => F[A]) =>
        val result = x.flatMap[A](unit[A])
        (result === x) :| s"got:$result expected:$x" }

    def identityLeft[A: Arbitrary](using Arbitrary[A => F[A]], Equality[F[A]]) =
      forAll { (y: A, f: A => F[A]) =>
        val left = unit[A](y).flatMap[A](f)
        val right = f(y)
        (left === right) :| s"left:$left right:$right"
      }

    def identity[A: Arbitrary](using Arbitrary[F[A]], Arbitrary[A => F[A]],
      Equality[F[A]]): Prop =
      { "identity left: " |: identityLeft[A]  } &&
      { "identity right:" |: identityRight[A] }

    def monad[A: Arbitrary, B, C] (using Arbitrary[F[A]], Arbitrary[A => F[A]],
      Arbitrary[A => F[B]], Arbitrary[B => F[C]]): Prop =
      { "associative:" |: this.associative[A,B,C] } &&
      { "identity:   " |: this.identity[A] }

  end monadLaws

end Monad

lazy val optionMonad: Monad[Option] = new:
  def unit[A](a: => A): Option[A] = Some(a)
  extension [A](oa: Option[A])
    def flatMap[B](f: A => Option[B]): Option[B] = oa match
      case Some(a) => f(a)
      case None => None


lazy val listMonad: Monad[List] = new:
  def unit[A](a: => A): List[A] = List(a)
  extension [A](as: List[A])
    def flatMap[B](f: A => List[B]): List[B] = as.flatMap(f)

extension [F[_]](m: Monad[F])
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    import m.map2
    val fz: F[List[A]] = m.unit(List[A]())
    val fcons = (h: F[A], tl: F[List[A]]) => h.map2(tl) { _::_ }
    fas.foldRight(fz)(fcons)

extension [F[_]](m: Monad[F])
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    m.sequence(List.fill(n)(ma))

extension [F[_]](m: Monad[F])
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    import m.flatMap
    a => f(a).flatMap(g)

lazy val genMonad: Monad[Gen] = new:
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    extension [A](fa: Gen[A])
      override def flatMap[B](f: A => Gen[B]): Gen[B] =
        Gen.flatMap(fa)(f)

lazy val parMonad: Monad[Par] = new:
    def unit[A](a: => A) = Par.unit(a)
    extension [A](fa: Par[A])
      override def flatMap[B](f: A => Par[B]): Par[B] =
        Par.flatMap(fa)(f)


lazy val lazyListMonad: Monad[LazyList] = new:
    def unit[A](a: => A) = LazyList(a)
    extension [A](fa: LazyList[A])
      override def flatMap[B](f: A => LazyList[B]) =
        fa.flatMap(f)


  // Since `State` is a binary type constructor, we need to partially apply it
  // with the `S` type argument. Thus, it is not just one monad, but an entire
  // family of monads, one for each type `S`. One solution is to create a class
  // `StateMonads` that accepts the `S` type argument and then has a _type member_
  // for the fully applied `State[S, A]` type inside:
  trait StateMonads[S]:
    type StateS[A] = State[S, A]

    // We can then declare the monad for the `StateS` type constructor:
    lazy val stateMonad: Monad[StateS] = new:
      def unit[A](a: => A): State[S, A] = State(s => (a, s))
      extension [A](fa: State[S, A])
        override def flatMap[B](f: A => State[S, B]) =
          State.flatMap(fa)(f)

