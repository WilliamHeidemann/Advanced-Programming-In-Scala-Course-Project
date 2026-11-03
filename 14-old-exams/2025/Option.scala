// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
package adpro.option

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*

enum Option[+A]:
  case Some(get: A)
  case None

  // Exercise 6

  def map[B](f: A => B): Option[B] = this match
    case None => None
    case Some(v) => Some(f(v))

  def getOrElse[B >: A] (default: => B): B = this match
    case None => default
    case Some(v) => v

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some.apply).getOrElse(ob)

  def filter(p: A => Boolean): Option[A] =
    this.flatMap(v => if p(v) then this else None)

  def forAll(p: A => Boolean): Boolean = this match
    case None => true
    case Some(a) => p(a)

  def isEmpty: Boolean = this match
    case None => true
    case _ => false

end Option

import Option.*

def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A,B) => C): Option[C] =
  ao.flatMap { a => bo.map { f(a, _) } }

def sequence[A](aos: List[Option[A]]): Option[List[A]] =
  def f(o: Option[A],z: Option[List[A]]): Option[List[A]] =
    z.flatMap { l => o.map(a => a::l) }
  aos.foldRight(Some(Nil))(f)

def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
  def g(a: A, z: Option[List[B]]): Option[List[B]] =
    z.flatMap { l => f(a).map { _::l } }
  as.foldRight(Some(Nil))(g)

given arbOption: [A: Arbitrary] => Arbitrary[Option[A]] =
  val gen = for
    ao  <- arbitrary[scala.Option[A]]
    ao1 <- ao match
      case scala.Some (a) => Some (a)
      case scala.None => None
  yield (ao1)
  Arbitrary (gen)
