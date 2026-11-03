package adpro.monad
// we follow the sama packaging as in the exercises (even though adpro.monoid would be more natural)

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.given
import org.scalactic.Equality
import org.scalactic.TripleEquals.*

trait Monoid[A]:

  def combine(a1: A, a2: A): A
  def empty: A

  // Some Laws for monoids (We place them here like in Parsers)

  object laws:

    /* The instance of Eqaulity type class will be used for equality
     * comparisons.  Normally the default equality is fine, but we need to
     * parameterize to handle endoMonoid (comparisons of functions).  See more
     * comments about that in the test of Ex03.01. */

    def associative(using Arbitrary[A], Equality[A]): Prop =
      forAll { (a1: A, a2: A, a3: A) =>
        combine(combine(a1, a2), a3) ===
          combine(a1, combine(a2, a3))
      } :| "monoid-associative"

    /* The triple equality below (===) uses the eqA instance (by default the
     * same as == but we can override it, which is exploited in the test of
     * Exercise 3) */

    def unit(using Arbitrary[A], Equality[A]): Prop =
      forAll { (a: A) =>
        (combine(a, empty) === a) :| "right-empty" &&
        (combine(empty, a) === a) :| "left-empty"
      } :| "monoid-unit"

    def monoid (using Arbitrary[A], Equality[A]): Prop =
      (associative && unit) :| "monoid-laws"

end Monoid

// AW: A helper function to find a monoid instance easily
def monoid[A: Monoid]: Monoid[A] = summon[Monoid[A]]

val stringMonoid = new Monoid[String]:
  def combine(a1: String, a2: String) = a1 + a2
  val empty = ""

def listMonoid[A] = new Monoid[List[A]]:
  def combine(a1: List[A], a2: List[A]) = a1 ++ a2
  val empty = Nil

lazy val intAddition: Monoid[Int] = new Monoid[Int]:
  def combine(a1: Int, a2: Int) = a1 + a2
  val empty = 0

lazy val intMultiplication: Monoid[Int] = new Monoid[Int]:
  def combine(a1: Int, a2: Int) = a1 * a2
  val empty = 1

lazy val booleanOr: Monoid[Boolean] = new Monoid[Boolean]:
  def combine(a1: Boolean, a2: Boolean) = a1 || a2
  val empty = false

lazy val booleanAnd: Monoid[Boolean] = new Monoid[Boolean]:
  def combine(a1: Boolean, a2: Boolean) = a1 && a2
  val empty = true

def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]]:
  def combine(a1: Option[A], a2: Option[A]): Option[A]= a1.orElse(a2)
  val empty = None

// We can get the dual of any monoid just by flipping the `combine`.
def dual[A](m: Monoid[A]): Monoid[A] = new:
  def combine(x: A, y: A): A = m.combine(y, x)
  val empty = m.empty

def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A]:
  def combine(f: A => A, g: A => A) = f.compose(g)
  val empty = identity[A]

extension [B](mb: Monoid[B])
  def foldMap[A](as: List[A])(f: A => B): B =
    as.foldLeft(mb.empty) { (b, a) => mb.combine(b, f(a)) }

def productMonoid[A, B](ma: Monoid[A])(mb: Monoid[B]) = new Monoid[(A, B)]:
  def combine(ab1: (A, B), ab2: (A, B)) =
    ma.combine(ab1._1, ab2._1) -> mb.combine(ab1._2, ab2._2)
  val empty = ma.empty -> mb.empty
