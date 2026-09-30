// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.lazyList

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary.arbitrary

import lazyList00.* // uncomment to test the book laziness solution implementation
// import lazyList01.* // uncomment to test the broken headOption implementation
// import lazyList02.* // uncomment to test another version

import LazyList.*

object LazyListSpec
  extends org.scalacheck.Properties("testing"):

  /* Generators and helper functions */

  /** Convert a strict list to a lazy-list */
  def list2lazyList[A](la: List[A]): LazyList[A] =
    LazyList(la *)

  /** Generate finite non-empty lazy lists */
  def genNonEmptyLazyList[A](using Arbitrary[A]): Gen[LazyList[A]] =
    for la <- arbitrary[List[A]].suchThat {
      _.nonEmpty
    }
    yield list2lazyList(la)

  /** Generate an infinite lazy list of A values.
   *
   * This lazy list is infinite if the implicit generator for A never fails. The
   * code is ugly-imperative, but it avoids stack overflow (as Gen.flatMap is
   * not tail recursive)
   */
  def infiniteLazyList[A: Arbitrary]: Gen[LazyList[A]] =
    def loop: LazyList[A] =
      summon[Arbitrary[A]].arbitrary.sample match
        case Some(a) => cons(a, loop)
        case None => empty

    Gen.const(loop)

  def genThrowingLazyListTail[A](using Arbitrary[A]): Gen[LazyList[A]] = {
    for {
      head <- Arbitrary.arbitrary[A]
    } yield cons(head, throw new RuntimeException("Tail was evaluated."))
  }

  def genThrowingLazyList[A](using Arbitrary[A]): Gen[LazyList[A]] = {
    cons(
      throw new RuntimeException("Head was evaluated."),
      throw new RuntimeException("Tail was evaluated."))
  }

  def genPositiveInt(using Arbitrary[Int]): Gen[Int] = {
    for {
      int <- Arbitrary.arbInt.arbitrary
    } yield int.abs
  }

  def genNandM(min: Int, max: Int): Gen[(Int, Int)] = {
    for {
      n <- Gen.choose(min, max)
      m <- Gen.choose(min, max)
    } yield (n, m)
  }

  def genShortLazyList(maxSize: Int): Gen[LazyList[Int]] =
    for
      size <- Gen.choose(0, maxSize)
      list <- Gen.listOfN(size, arbitrary[Int])
    yield list2lazyList(list)

  def genThrowingLazyList[A](size: Int): Gen[LazyList[A]] = {
    def loop(n: Int): LazyList[A] = n match {
      case i if i > 0 => cons(throw new RuntimeException(), loop(n - 1))
      case _ => empty
    }
    loop(size)
  }

  /* The test suite */

  // Exercise 1

  property("Ex01.01: headOption returns None on an empty LazyList") =
    empty.headOption == None

  property("Ex01.02: headOption returns the head of the stream packaged in Some") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (n: Int) => cons(n,empty).headOption == Some(n) } :| "singleton" &&
    forAll { (s: LazyList[Int]) => s.headOption != None }      :| "random"

  // Exercise 2

  property("Ex02: headOption does not force the tail of a lazy list") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genThrowingLazyListTail[Int])

    forAll { (s: LazyList[Int]) =>
      s.headOption
      true
    }

  // Exercise 3

  property("Ex03: take does not force any heads nor any tails of the lazy list it manipulates") =

    given Arbitrary[LazyList[Int]] = Arbitrary(cons(
      throw new RuntimeException("Head evaluated."),
      throw new RuntimeException("Tail evaluated.")))

    forAll { (s: LazyList[Int]) =>
      s.take(10)
      true
    }

  // Exercise 4

  property("Ex04: take(n) does not force the (n+1)st head ever (even if we force all elements of take(n))") =
    given Arbitrary[Int] = Arbitrary(Gen.choose(0, 10))
    given Arbitrary[LazyList[Int]] = Arbitrary(infiniteLazyList)

    forAll { (s: LazyList[Int], n: Int) =>
      s.append(throw new RuntimeException("n+1st element evaluated")).take(n).toList
      true
    }

  // Exercise 5

  property("Ex05: l.take(n).take(n) == l.take(n) for any lazy list s and any n") =
    given Arbitrary[Int] = Arbitrary(Gen.choose(0, 10))
    given Arbitrary[LazyList[Int]] = Arbitrary(infiniteLazyList)

    forAll { (s: LazyList[Int], n: Int) => s.take(n).take(n).toList == s.take(n).toList }

    // Exercise 6

  property("Ex06: l.drop(n).drop(m) == l.drop(n+m) for any n, m") =
    given Arbitrary[Int] = Arbitrary(Gen.choose(0, 10))
    given Arbitrary[LazyList[Int]] = Arbitrary(infiniteLazyList)

    forAll { (s: LazyList[Int], n: Int, m: Int) =>
      s.drop(n).drop(m).take(10).toList == s.drop(n + m).take(10).toList
    }

// Exercise 7

  property("Ex07: l.drop(n) does not force any of the dropped elements (heads). " +
    "This should hold even if we force some element in the tail.") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genThrowingLazyList[Int](100))
    given Arbitrary[Int] = Arbitrary(Gen.choose(0, 10))

    forAll { (n: Int, m: Int) =>
      def throwingPrefix(i: Int): LazyList[Int] =
        if i < n then cons(throw new RuntimeException("Prefix was forced"), throwingPrefix(i + 1))
        else cons(i, throwingPrefix(i + 1))

      throwingPrefix(0).drop(n).take(m).toList
      true
    }

// Exercise 8

  property("Ex08: l.map(identity) == l for any lazy list l") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genShortLazyList(100))

    forAll { (l: LazyList[Int]) => l.map(identity).toList == l.toList }

// Exercise 9

  property("Ex09: map terminates on infinite lazy lists") =
    given Arbitrary[LazyList[Int]] = Arbitrary(infiniteLazyList)

    forAll { (l: LazyList[Int]) =>
      l.map(identity)
      true
    }

// Exercise 10
  property("Ex10: correctness of append") =

    val maxIndividualSize = 10
    given Arbitrary[LazyList[Int]] = Arbitrary(genShortLazyList(maxIndividualSize))
    forAll { (l1: LazyList[Int], l2: LazyList[Int]) =>
      l1.append(l2).toList == l1.toList ++ l2.toList
    } :| "append is equivalent to concatenation"