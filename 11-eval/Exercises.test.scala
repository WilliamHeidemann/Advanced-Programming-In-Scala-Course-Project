// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Monads for functional programming by Phil Wadler
// change package to adpro.eval.solution to test teacher's solutions

package adpro.eval

import org.scalacheck.{Arbitrary, Prop, Test, Gen}
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalactic.TripleEquals.*

object EvalSpec
  extends org.scalacheck.Properties("eval"):

  import Term.*

  // test cases, also from Section 2.1 [Wadler]:
  val answer = Div (
                  Div (Cons(1972), Cons(2)),
                  Cons(23)
               )
  val error = Div( Cons(1), Cons(0) )
  val const = Cons(42)
  val zero = Cons(0)


  property("base.01: test case from Section 2.1 (BasicEvaluator.eval)") =
    BasicEvaluator.eval (answer) === 42

  property("base.02: test case from Section 2.1 (BasicEvaluator.eval)") =
     BasicEvaluator.eval (const) === 42

  property(s"base.03: should throw a scala exception on division by 0") =
     throws(classOf[scala.Exception]) (BasicEvaluator.eval (error))

  property(s"Ex01.01: the evaluators from Section 2.1-2.2 agree between each other") =
    import ExceptionEvaluator.M.*
    forAll { (t: Int, u: Int) =>
      val u1 = if u == 0 then 1 else u
      val e = Div(Cons(t), Cons(u1))
      ExceptionEvaluator.eval(e) === Return(BasicEvaluator.eval(e)) }

  property(s"Ex01.02: the ExceptionEvaluator agrees with scala evaluator") =
    import ExceptionEvaluator.eval
    import ExceptionEvaluator.M.*
    forAll { (t: Int, u: Int) =>
      val u1 = if u == 0 then 1 else u
      eval(Div(Cons(t), Cons(u1))) === Return(t/u1) }

  property(s"Ex01.03: ExceptionEvaluator Wadler-raise on division-by-zero") =
    import ExceptionEvaluator.eval
    import ExceptionEvaluator.M.*
    forAll { (t: Int) => Prop.iff (
      eval(Div(Cons(t), zero)),
      { case Raise(_) => true }
    ) }

  // generator of random divisions by not-a-zero
  val genSafeTerm: Gen[Term] =
    given Arbitrary[Int] = Arbitrary(Gen.choose[Int](0, 999))
    arbitrary[List[Int]]
      .suchThat { l => l.nonEmpty && l.forall { _!=0 } }
      .flatMap[List[Term]] { li => li.sorted.map { n => Cons(n.abs) } }
      .flatMap[Term] ( lc => lc.tail.foldLeft[Term] (lc.head) (Div.apply) )

  property(s"Ex01.04: ExceptionEvaluator returns a non-neg result on randome safe terms") =
    import ExceptionEvaluator.M.*
    forAll (genSafeTerm) { (t: Term) => Prop.iff(
      ExceptionEvaluator.eval(t),
      { case Return(n)  => n >=0 }
    ) }

  // A generator of random monadic values so that we can execute monad laws
  def genM[A: Arbitrary]: Gen[ExceptionEvaluator.M[A]] =
    import ExceptionEvaluator.M
    Gen.oneOf (
      arbitrary[A].map { a => M.Return[A](a)},
      Gen.const(M.Raise("divide by zero")
    ))

  given arbExceptionEvaluatorM: Arbitrary[ExceptionEvaluator.M[Int]] =
    Arbitrary(genM[Int])

  property(s"Ex02.01: mIsMonad satisfies propertye laws") =
    ExceptionEvaluator.mIsMonad.laws.monad[Int, Int, Int]

  property(s"Ex03.01: the evaluators from Section 2.1-2.2 agree (evalMonad)") =
    import ExceptionEvaluator.M.*
    forAll { (t: Int, u: Int) =>
      val u1 = if u == 0 then 1 else u
      val e = Div(Cons(t), Cons(u1))
      ExceptionEvaluator.evalMonad(e) === Return(BasicEvaluator.eval(e)) }

  property(s"Ex03.02: evalMonad agrees with scala evaluator") =
    import ExceptionEvaluator.M.*
    forAll { (t: Int, u: Int) =>
      val u1 = if u == 0 then 1 else u
      ExceptionEvaluator.evalMonad(Div(Cons(t), Cons(u1))) === Return(t/u1) }

  property(s"Ex03.03: evalMonad Wadler-raises on division-by-zero") =
    import ExceptionEvaluator.M.*
    forAll { (t: Int) => Prop.iff (
      ExceptionEvaluator.evalMonad(Div(Cons(t), zero)),
      { case Raise(_) => true }
    ) }

  property(s"Ex04.01: the evaluators from Section 2.1-2.2 agree (evalForYield)") =
    import ExceptionEvaluator.M.*
    forAll { (t: Int, u: Int) =>
      val u1 = if u == 0 then 1 else u
      val e = Div(Cons(t), Cons(u1))
      ExceptionEvaluator.evalForYield(e) === Return(BasicEvaluator.eval(e)) }

  property(s"Ex04.02: evalForYield agrees with scala evaluator") =
    import ExceptionEvaluator.M.*
    forAll { (t: Int, u: Int) =>
      val u1 = if u == 0 then 1 else u
      ExceptionEvaluator.evalForYield(Div(Cons(t), Cons(u1))) === Return(t/u1) }

  property(s"Ex04.03: evalForYield Wadler-raises on division-by-zero") =
    import ExceptionEvaluator.M.*
    forAll { (t: Int) => Prop.iff (
      ExceptionEvaluator.evalForYield(Div(Cons(t), zero)),
      { case Raise(_) => true }
    ) }

  property(s"Ex05.01: should count two divisions") =
    forAll { (n: Int) =>
      StateEvaluator.eval (answer).step (n) == (42, n+2) }

  property(s"Ex05.02: should count no divisions") =
    forAll { (n: Int) =>
      StateEvaluator.eval (const).step (n) == (42,n) }

  property(s"Ex05.03: should raise a scala exception on division by 0") =
     throws(classOf[scala.Exception]) (StateEvaluator.eval (error).step (0))


  property(s"Ex06.01: StateEvaluator.mIsMonad satisfies monad laws") =
    import StateEvaluator.M

    // A generator of random monadic values so that we can execute monad laws
    def genM[A: Arbitrary]: Gen[M[A]] =
      for step <- arbitrary[Int => Int]
             a <- arbitrary[A]
      yield M { x => (a, step(x)) }

    given Arbitrary[M[Int]] = Arbitrary(genM[Int])

    // TODO: it is very suspicious that this fails, but the thing below does not
    // StateEvaluator.mIsMonad.laws.monad[Int, Int, Int]
    StateEvaluator.mIsMonad.laws.associative[Int,Int,Int]
      && StateEvaluator.mIsMonad.laws.identity[Int]

  property(s"Ex07.01: evalMonad should count two divisions") =
    forAll { (n: Int) =>
      StateEvaluator.evalMonad (answer).step (n) == (42, n+2) }

  property(s"Ex07.02: evalMonad should count no divisions") =
    forAll { (n: Int) =>
      StateEvaluator.evalMonad (const).step (n) == (42,n) }

  property(s"Ex07.03: evalMonad should raise a scala exception on division by 0") =
     throws(classOf[scala.Exception]) (StateEvaluator.evalMonad (error).step (0))

  property(s"Ex08.01: evalMonad should count two divisions") =
    forAll { (n: Int) =>
      StateEvaluator.evalMonad (answer).step (n) == (42, n+2) }

  property(s"Ex08.02: evalMonad should count no divisions") =
    forAll { (n: Int) =>
      StateEvaluator.evalMonad (const).step (n) == (42,n) }

  property(s"Ex08.03: evalMonad should raise a scala exception on division by 0") =
     throws(classOf[scala.Exception]) (StateEvaluator.evalMonad (error).step (0))

  val result = "eval(Cons(1972)) <= 1972\n" +
               "eval(Cons(2)) <= 2\n" +
               "eval(Div(Cons(1972),Cons(2))) <= 986\n" +
               "eval(Cons(23)) <= 23\n" +
               "eval(Div(Div(Cons(1972),Cons(2)),Cons(23))) <= 42\n"

  property("Ex09.01: OutputEvaluator gives good 'result' and string output") =
    val r = OutputEvaluator.eval(answer)
    r.a == 42 && r.o == result

  property("Ex09.02: OutputEvaluator returns simple result for a constant") =
    val r = OutputEvaluator.eval(const)
    r.a === 42 && r.o === "eval(Cons(42)) <= 42\n"

  property("Ex09.03: OutputEvaluator throws an exception on division by 0") =
     throws(classOf[scala.Exception]) (OutputEvaluator.eval (error))

  def genOutputMonad[A: Arbitrary]: Gen[OutputEvaluator.M[A]] =
    for m1 <- Gen.choose(0, 1000)
        m2 <- Gen.choose(0, 1000)
        msg = s"msg{m1}\nmsg{m2}\n"
        a  <- arbitrary[A]
    yield OutputEvaluator.M[A](msg, a)

  given arbOutputEvalutorMonad: Arbitrary[OutputEvaluator.M[Int]] =
    Arbitrary(genOutputMonad[Int])

  property("Ex10.01: OutputEvaluator.mIsMonad satisfies monad laws") =
    OutputEvaluator.mIsMonad.laws.monad[Int, Int, Int]

  property("Ex11.01: OutputEvaluator.evalMonad gives good 'result' and string output") =
    val r = OutputEvaluator.evalMonad(answer)
    r.a == 42 && r.o == result

  property("Ex11.02: OutputEvaluator.evalMonad returns simple result for a constant") =
    val r = OutputEvaluator.evalMonad(const)
    r.a === 42 && r.o === "eval(Cons(42)) <= 42\n"

  property("Ex11.03: OutputEvaluator.evalMonad throws an exception on division by 0") =
     throws(classOf[scala.Exception]) (OutputEvaluator.evalMonad (error))

  property(s"Ex11.04: output evaluators without and with flatMap agree") =
    forAll (genSafeTerm) { (t: Term) =>
      OutputEvaluator.eval(t) === OutputEvaluator.evalMonad(t)  }

  property("Ex12.01: OutputEvaluator.evalForYield gives good 'result' and string output") =
    val r = OutputEvaluator.evalForYield(answer)
    r.a == 42 && r.o == result

  property("Ex12.02: OutputEvaluator.evalForYield returns simple result for a constant") =
    val r = OutputEvaluator.evalForYield(const)
    r.a === 42 && r.o === "eval(Cons(42)) <= 42\n"

  property("Ex12.03: OutputEvaluator.evalForYield throws an exception on division by 0") =
     throws(classOf[scala.Exception]) (OutputEvaluator.evalForYield (error))

  property(s"Ex12.04: output evaluators without and with for-yield agree") =
    forAll (genSafeTerm) { (t: Term) =>
      OutputEvaluator.eval(t) === OutputEvaluator.evalForYield(t)  }
