package adpro.solution

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink}
import org.scalacheck.Prop
import org.scalactic.TripleEquals.*

// Used by samplers below
given rng: spire.random.rng.SecureJava
  = spire.random.rng.SecureJava.apply

object ExamSpecFull
  extends org.scalacheck.Properties("exam-2024-autumn"):

  property("A test that always passes (a sanity check)") =
    forAll { (n: Int) => n == n }

  property("Ex01.01: goodPairs should fail") =
    forAll { (l: List[Int]) =>
      val l1 = 31::42::l
      !Good.goodPairs (l1, (_ == _))
    }

  property("Ex01.02: goodPairs should succeed") =
    forAll { (n: Int) =>
      val l = List.fill (n.abs % 500) (n)
      Good.goodPairs (l, (_ == _))
    }

  property("Ex01.03: goodPairs on empty list") =
    forAll { (p: ((Int,Int) => Boolean)) =>
      Good.goodPairs (Nil, p) }

  property("Ex01.04: goodPairs on singleton list") =
    forAll { (n: Int, p: ((Int,Int) => Boolean)) =>
      Good.goodPairs (n:: Nil, p) }

  property("Ex02.01: goodPairsCurried should fail") =
    forAll { (l: List[Int]) =>
      val l1 = 31::42::l
      !Good.goodPairsCurried (l1) (_ == _)
    }

  property("Ex02.02: goodPairsCurried should succeed") =
    forAll { (n: Int) =>
      val l = List.fill (n.abs % 500) (n)
      Good.goodPairsCurried (l) (_ == _)
    }

  property("Ex02.03: goodPairsCurried on empty list") =
    forAll { (p: ((Int,Int) => Boolean)) =>
      Good.goodPairsCurried (Nil) (p) }

  property("Ex02.04: goodPairsCurried on singleton list") =
    forAll { (n: Int, p: ((Int,Int) => Boolean)) =>
      Good.goodPairsCurried (n:: Nil) (p) }

  property("Ex03.01: curriedNested doesn't affect fn value") =
    forAll { (f: ((Int, Int) => Int) => Int) =>
      forAll { (p: (Int, Int) => Int) =>
        f (p) === Good.curriedNested(f) (p.curried)
      }
    }

  property("Ex04.01: goodPairsHotCurry should fail") =
    forAll { (l: List[Int]) =>
      val l1 = 31::42::l
      !Good.goodPairsHotCurry (l1) { (a: Int) => (b: Int) => a == b }
    }

  property("Ex04.02: goodPairsHotCurry should succeed") =
    forAll { (n: Int) =>
      val l = List.fill (n.abs % 500) (n)
      Good.goodPairsHotCurry (l) { (a: Int) => (b: Int) => a == b }
    }

  property("Ex04.03: goodPairsHotCurry on empty list") =
    forAll { (p: ((Int,Int) => Boolean)) =>
      Good.goodPairsHotCurry (Nil) (p.curried) }

  property("Ex04.03: goodPairsHotCurry on singleton list") =
    forAll { (n: Int, p: ((Int,Int) => Boolean)) =>
      Good.goodPairsHotCurry (n:: Nil) (p.curried) }

  property("Ex05.01: A success case (we only check if it compiles)") =
    scala.compiletime.testing.typeChecks {
      """
      {
        import adpro.monad.*
        import pigaro.Dist
        def f (using Monad[Dist]) =
          summon[Monad[Dist]].unit[Int](42)
        import MultivariateUniform.given
        f
      }
      """
    }

  property("Ex06.01: Check if the distribution describes the lists of the right length") =
    import MultivariateUniform.given
    forAllNoShrink (Gen.choose(2,100)) { (n: Int) =>
      MultivariateUniform.multUni(n, 1,2,3,4,5)
        .sample(1)
        .chain
        .head
        .size === n }
    //
  // A test that merely detects that the original types have not been deleted.
  // This will not fly as we cannot be sure in which order the students will add
  // type parameters
  // trait CReservationSystem extends FullyAbstractTrains.ReservationSystem
  //   [Set, Int, String, String, String, String, String, String]:
  //   override def law1: Prop = false
  //   override def law2: Prop = false

  // Let's use some types the user does not know about so
  // that they cannot guess what we are testing on (like Int),
  // and pass tests because Gen[Option[Int]] can always be summoned.
  object Hide:
    opaque type MA = Int
    opaque type MB = Double

    given arbMA: Arbitrary[Option[MA]] = summon
    given arbMB: Arbitrary[Option[MB]] = summon

  import Hide.{MA, MB}
  import Hide.given

  property("Ex07.01: Gen[Either[A,B]] can generate Some[A]") =
    val l = LazyList.unfold[Either[MA,MB],Unit](()) { _ =>
        Gens.genEither[MA,MB].sample.map { eab => (eab,()) } }
    .take(1000)
    assert (l.size == 1000, s"Generated ${l.size} elements")
    l.exists { _.isLeft }

  property("Ex07.02: Gen[Either[A,B]] can generate Some[B]") =
    LazyList.unfold(())(_ => (Gens.genEither[Int,Double].sample.map ((_,()))))
    .take(10000)
    .exists { _.isRight }

  property("Ex08.01: Parse a success case") =
    import adpro.parsing.Sliceable.*
    forAllNoShrink (Gen.choose(-42,42)) { (n: Int) =>
      IntervalParser1.intBetween(-42, +42).run (n.toString) === Right (Some (n))  }

  property("Ex08.02: Parse a failure case") =
    import adpro.parsing.Sliceable.*
    forAllNoShrink (Gen.choose(43,200)) { (n: Int) =>
      IntervalParser1.intBetween(-42, +42).run (n.toString) === Right (None)  }

  // had some race conditions with this test. Seem to need to do scala-cli clean
  // after every change to the solution file (bloop somehow gets confused if metals
  // is using it simultanously, I think)
  property("Ex09.01: Parse a success case (only check if it compiles)") =
    scala.compiletime.testing.typeChecks {
      """
        import adpro.parsing.*
        import IntervalParser2.*
        def f [P[+_]] (p: Parsers[ParseError, P]): Unit =
          val _: P[Option[Int]] = p.intBetween(0,0)
      """
    }

  property("Ex10.01: NO ATOMATED TESTS FOR Q10 (API not standardized)") =
    true

  property("Ex11.01: NO ATOMATED TESTS FOR Q11 (API not standardized)") =
    true

  property("Ex12.01: NO ATOMATED TESTS FOR Q12 (API not standardized)") =
    true


// vim:cc=66
