/* Final Exam: Advanced Programming, by Andrzej Wąsowski
 * IT University of Copenhagen, Autumn 2024: 06 January 2025
 *
 * The exam consists of 12 questions to be solved within 4 hours.
 * Solve the tasks in the file 'Exam.scala' (this file).
 *
 * You can use all functions provided in the included files,  as well
 * as functions we implemented in the course (if the source is missing
 * in this folder, you can add it to this file, so that things
 * compile)
 *
 * You can access any static written aids, also online, but you are
 * not allowed to communicate with anybody or with anything (bots).
 * Using GitHub copilot ChatGPT and similar large language models
 * during the exam is not allowed. By submitting you legally declare
 * to have solved the problems alone, without communicating with
 * anybody.
 *
 * Do not modify this file in other ways than answering the questions
 * (adding imports is allowed). Do not reorder the answers, and do not
 * remove question numbers or comments from the file.
 *
 * Submit this file and only this file to LearnIT. Do not convert to
 * any other format than .scala. Do not submit the entire zip archive.
 * The only accepted file format is '.scala'.
 *
 * Keep the solutions within 80 character columns to make grading
 * easier.
 *
 * The answers will be graded manually. We focus on the correctness of
 * ideas, the use of concepts, clarity, and style. We will use
 * undisclosed automatic tests during grading, but not to compute the
 * final grade, but to help us debug your code.
 *
 * We do require that your hand in compiles.  The directory has a
 * project setup so compilation with scala-cli shall work
 * out-of-the-box. If you cannot make a fragment compile, put your
 * solution in a comment, next to the three question marks. We will
 * grade the solutions in comments as well.
 *
 * We will check whether the file compiles by running
 *
 *    scala-cli compile .
 *
 * Hand-ins that do not compile will automatically fail the exam.
 *
 * We do not recommend to write and run tests if you are pressed for
 * time. It is a good idea to run and test, if you have time.
 *
 * Good luck!
 **/

package adpro.solution

import org.scalacheck.{Arbitrary, Gen, Prop}
import Arbitrary.*, Prop.*
import org.scalactic.TripleEquals.*

import adpro.lazyList.LazyList
import adpro.state.*

object Good:

  /* QUESTION 1 ######################################################
   *
   * Implement a function `goodPairs` that checks whether all pairs of
   * consecutive elements in a list satisfy a predicate. Choose the
   * right higher order function for the task. If you can't solve this
   * with higher order functions, using recursion still makes sense,
   * even if for less points.
   */

  // def goodPairs [A] (l: List[A], good: (A,A) => Boolean): Boolean = ???

  // An excellent solution contributed by Bjarke
  def goodPairs [A] (l: List[A], good: (A,A) => Boolean): Boolean =
    l.zip(l.drop(1))
     .forall { good.tupled }

  // Probably as fast as a direct recursion
  def goodPairs1 [A] (l: List[A], good: (A,A) => Boolean): Boolean = l match
    case Nil => true
    case h::t =>
      t.foldLeft (true, h) { case ((res,prev), next) =>
        (res && good(prev, next), next)  }
        ._1

  def goodPairs2 [A] (l: List[A], good: (A,A) => Boolean): Boolean =
    l match
    case Nil => true
    case _::t => l.zip (t)
                  .map (good.tupled)
                  .forall (identity)

  // Concise but slow due to init, also corresponds was not implemented in
  // the course. (Init I don't remember)
  def goodPairs3 [A] (l: List[A], good: (A,A) => Boolean): Boolean =
    l match
    case Nil => true
    case _::t => l.init.corresponds(t)(good)

  // NB. copilot proposes: `l.sliding(2).forall { case List(a,b) => good(a,b) }` (WRONG)
  // NB. with a bit more clicking, after a hint l match, copilot proposes a completion which is a correct recursive solution:
  //   case Nil => true
  //   case h::t => t match
  //     case Nil => true
  //     case h1::t1 => good(h,h1) && goodPairs(t, good)

  /* QUESTION 2 #####################################################
   *
   * Recall the functions  curry and uncurry from the course (week 1).
   * In this exercise we use the standard library counterparts,
   * `curried` and `uncurried` see these docs (if you don't recall
   * them):
   *
   * https://scala-lang.org/api/3.4.2/scala/Function$.html#uncurried-d4
   * https://scala-lang.org/api/3.4.2/scala/Function2.html#curried-0
   *
   * Use the above functions to produce a function goodPairsCurried by
   * transforming goodPairs programmatically, without writing it from
   * scratch.
   *
   * This question can be solved even if you did not answer Q1. Just
   * assume you have the solution for Q1.
   */

  // def goodPairsCurried[A]: List[A] => ((A,A) => Boolean) => Boolean = ???

  def goodPairsCurried[A]: List[A] => ((A,A) => Boolean) => Boolean =
    goodPairs[A].curried

  // A weaker solution as this does not use the function "curried"
  def goodPairsCurried1[A]: List[A] => ((A,A) => Boolean) => Boolean =
    l => good => goodPairs(l, good)

  // NB. co-pilot does not propose anything useful to me



  /* QUESTION 3. #####################################################
   *
   * Now Implement function curriedNested that takes a higher order
   * function with the first argument being an uncurried binary
   * function and curries the first argument. See the type
   * specification below.
   *
   * This question can be solved even if you did not answer the
   * previous questions.
   */

  // def curriedNested [A, B, C, D] (f: ((A,B) => C) => D): (A => B => C) => D = ???

  def curriedNested [A, B, C, D] (f: ((A,B) => C) => D): (A => B => C) => D =
    f compose Function.uncurried

  def curriedNested1 [A, B, C, D] (f: ((A,B) => C) => D): (A => B => C) => D =
    f compose Function.uncurried

  def curriedNested2 [A, B, C, D] (f: ((A,B) => C) => D): (A => B => C) => D =
    Function.uncurried[A,B,C] andThen f

  def curriedNested3 [A, B, C, D] (f: ((A,B) => C) => D): (A => B => C) => D =
    p => f(Function.uncurried (p))

  def curriedNested4 [A, B, C, D] (f: ((A,B) => C) => D): (A => B => C) => D =
    p => f (Function.uncurried (p))

  //  Does not use curried/uncurried, but this was not asked for
  def curriedNested5 [A, B, C, D] (f: ((A,B) => C) => D): (A => B => C) => D =
      (g: (A => B => C)) => f((a,b) => g(a)(b))

  // NB. copilot proposes `f.curried` (WRONG)



  /* QUESTION 4 ######################################################
   *
   * Create a function goodPairsHotCurry where both the top-level
   * function and the first argument are curried. Do not implement the
   * function from scratch but use curriedNested and standard library
   * functions to achieve the transformation.
   *
   * This question can be solved even if you did not answer the
   * previous questions.
   */

  def goodPairsHotCurry[A]: List[A] => (A => A => Boolean) => Boolean =
    l => curriedNested (goodPairs[A].curried (l))

  def goodPairsHotCurry1[A]: List[A] => (A => A => Boolean) => Boolean =
    l => p => curriedNested (goodPairs[A].curried (l)) (p)

  // very elegant, found by one of the students in exam conditions
  def goodPairsHotCurry2[A]: List[A] => (A => A => Boolean) => Boolean =
    goodPairs[A].curried andThen curriedNested

  // NB. copilot proposes `curriedNested (goodPairs[A])` (WRONG)
  // NB. When given a prefix l => it completes with `l => curriedNested
  // (goodPairs[A]) (l)` (WRONG)

end Good



object MultivariateUniform:

  import pigaro.*
  import adpro.monad.*

  /* QUESTION 5 #####################################################
   *
   * Recall our probabilistic programming library Pigaro.  We want to
   * show that Pigaor's `Dist` is a monad. Provide evidence (a given,
   * an instance) of Monad for Dist.
   */

  // given ... (add answer here)

  // My solution:
  given distMonad: Monad[Dist] with
    def unit[A](a: => A): Dist[A] = Dirac(a)
    extension [A] (fa: Dist[A])
      override def flatMap[B](f: A => Dist[B]): Dist[B] =
        fa.flatMap(f)

  // slightly different, equivalent syntax
  // I put in an object to avoid given type clashes with the above
  object Alternative:
    given distIsMonad: Monad[Dist] = new Monad[Dist]:
      def unit[A](a: => A): Dist[A] = pigaro.Dirac(a)
      extension [A](da: Dist[A])
        override def flatMap[B](f: A => Dist[B]): Dist[B] =
          da.flatMap(f)


  // This is a copilot answer, it lacks an 'override' in the extension
  // unfortunately an error message from the compiler immmediately suggests
  // this
  // given Monad[Dist] with
  //   def unit[A] (a: => A): Dist[A] = Pigaro.uniform("unit")(a)
  //   extension [A] (fa: Dist[A])
  //     def flatMap[B] (f: A => Dist[B]): Dist[B] = fa.flatMap(f)


 /* QUESTION 6 #####################################################
  *
  * Implement a function `multUni`  that represents a product of
  * n identical uniform distributions, where n is its first argument.
  * A single sample from this distribution is a list of size n.
  *
  * def multUni (n: Int, values: T*): Dist[List[T]]
  *
  * You likely need to use the fact that Dist is a monad. If you do so
  * you should ensure that the function signature enforces this
  * requirement on the caller. Questions 5 and 6 are conceptually
  * related, but this one can be answered without answering Q5.
  */

  def multUni [T] (dim: Int, values: T*)(using Monad[Dist])
    : Dist[List[T]] =
    summon[Monad[Dist]].sequence
      (List.fill(dim)(Pigaro.uniform(values*)))

  // A less elegant solution, but still correct (a direct recursion)
  def multUni1[T] (dim: Int, values: T*): Dist[List[T]] = dim match
    case 0 => pigaro.Dirac(Nil)
    case x => multUni(x-1, values*)
              .flatMap(tail => Pigaro.uniform[T](values*)
              .map(head => head::tail))

end MultivariateUniform



object Gens:

  /* QUESTION 7 ######################################################
   *
   * Let's assume that we are writing some tests for a function that
   * takes Either[A,B] as an input, for some unknown types A and B
   * (type parameters).  We do not have Arbitrary[A] and Arbitrary[B]
   * instances.  Thanks to some imports we instead have access to
   * Arbitrary[Option[A]] and Arbitrary[Option[B]] instances instead.
   *
   * Write a function genEither that creates Gen[Either[A,B]] using
   * the Arbitrary[Option[A]] and Arbitrary[Option[B]]. Your
   * implementatation needs to ensure that the arbitraries are
   * available in the scope of the function (the type checker must
   * check for their existance).
   *
   * We are working with the scalacheck library here, so we use
   * org.scalacheck.Gen and org.scalacheck.Arbitrary, not the book's
   * Gen.
   *
   * A direct recursion is allowed and will award maximum points in
   * this exercise. Some no directly recusive solutions are also
   * possible.
   */

  // def genEither[A,B] = ???

  def genEither [A,B] (using Arbitrary[Option[A]], Arbitrary[Option[B]])
    : Gen[Either[A,B]] = for
        oa <- arbitrary[Option[A]]
        ob <- arbitrary[Option[B]]
        result <- oa.flatMap { a =>
                  ob.map { b => Gen.oneOf(Left(a), Right(b)) } }
                    .getOrElse (genEither[A,B])
    yield result

  // This is a recursive solution, that should still give full points
  def genEither1 [A,B] (using Arbitrary[Option[A]], Arbitrary[Option[B]])
    : Gen[Either[A,B]] = for
        oa <- arbitrary[Option[A]]
        ob <- arbitrary[Option[B]]
        result <-
          if oa.isDefined && ob.isDefined
          then Gen.oneOf (Left(oa.get), Right(ob.get))
          else genEither[A,B]
    yield result

  // This solution is not nice because it uses retryUntil, which is a
  // function we did not use in the course. It avoids direct recursion
  def genEither2 [A,B] (using Arbitrary[Option[A]], Arbitrary[Option[B]])
    : Gen[Either[A,B]] =
    for
        oa <- arbitrary[Option[A]].retryUntil { _.isDefined }
        ob <- arbitrary[Option[B]].retryUntil { _.isDefined }
        result <- Gen.oneOf (Left(oa.get), Right(ob.get))
    yield result

  // An example with flatMap explicit (and slightly different logics)
  def genEither3 [A,B]
  (using aoa: Arbitrary[Option[A]])
  (using aob: Arbitrary[Option[B]])
  : Gen[Either[A,B]] =
    aoa.arbitrary.flatMap( a =>
      aob.arbitrary.flatMap( b =>
          if a.isDefined || b.isDefined
            then Gen.const(a.toLeft(b.get))
            else genEither[A,B]
        ))

  // Copilot answer uses implicit, and is not correct
  // It can crash when both a and b are None
  // def genEither[A,B] (implicit arbA: Arbitrary[Option[A]], arbB: Arbitrary[Option[B]]): Gen[Either[A,B]] =
  //   for
  //     a <- arbA.arbitrary
  //     b <- arbB.arbitrary
  //   yield if a.isDefined then Left(a.get) else Right(b.get)
  //
  // Another copilot variant that does not seem to compile even:
  //
  // def genEither[A,B]: Gen[Either[A,B]] = for
  //   a <- Arbitrary.arbitrary[Option[A]]
  //   b <- Arbitrary.arbitrary[Option[B]]
  // yield if a.isDefined then Left(a.get) else Right(b.get)

end Gens


object IntervalParser1:

  import adpro.parsing.*
  import adpro.parsing.Sliceable.*

  /* QUESTION 8 ######################################################
   *
   * Implement a parser that accepts a single integer from a closed
   * interval between low and high.
   *
   *    intBetween(low: Int, high: Int): Parser[Option[Int]]*
   *
   * The parser always succeeds. It returns Some(n) if it parses an
   * integer n. It returns None, if the integer is not in the
   * interval.
   *
   * Use the parser combinator library developed in the course. You
   * may want to use a concrete parser implemetnation. The parser
   * `Sliceable` is included in the exam project.
   */

  // def intBetween (low: Int, high: Int): Parser[Int] = ???

  // NB. I tried to get a solution from copilot, but it did not propose
  // anything useful.

  // my solution
  val INT: Parser[Int] =
    regex("""(\+|-)?[0-9]+""".r)
      .map { _.toInt }

  def intBetween (low: Int, high: Int): Parser[Option[Int]] =
    INT.map { n => Some(n).filter { n => n >= low && n <= high } }

  def intBetween1 (low: Int, high: Int): Parser[Option[Int]] =
    INT.map (n =>
        if n < low || n > high then None else Some(n))

  // almost identical with for-yield
  def intBetween2 (low: Int, high: Int): Parser[Option[Int]] = for
    n <- INT
  yield { Some(n).filter (n => n >= low && n <= high) }

  // isn't this more cool?
  def intBetween3 (low: Int, high: Int): Parser[Option[Int]] =
    INT.map (Some.apply)
       .map { _.filter { n => n >= low && n <= high } }

end IntervalParser1



object IntervalParser2:

  import adpro.parsing.*

  /* QUESTION 9 #####################################################
   *
   * Notice that `intBetween` is independent of the concrete parser
   * implementation.  We can abstract over the parser type. Implement
   * it as an extension that works for any implementation of the
   * `Parsing` structure
   *
   * This question depends on the previous one. You need to copy your
   * answer to Q8 and generalize it to an extension of instances of
   * Parsers. Since now our parser implementation is abstract  you may
   * need to build the INT token lexer differently than in Q8 (it
   * depends a bit on which solution you proposed in Q8---you can no
   * longer use methods from Sliceable here).
   *
   * The goal is to have something like this code compile:
   *
   *  import IntervalParser2.*
   *  def f [P[+_]] (p: Parsers[ParseError, P]) =
   *    p.intBetween(0,0) ...
   *
   * HINT: the extension will be for p: Parsers[ParseError, P] for
   * some implementation of Parsing represented by type constructor
   * variable P[+_].
   */

  // Write your solution here(below)
  // ...

  // It seems that all the solutions are basically translating Q8
  // directly to place it in the extension.
  extension [P[+_]] (p: Parsers[ParseError, P])

    def INT: P[Int] =
      import p.*
      many1(char('0')|char('1')|char('2')|char('3')|char('4')|
            char('5')|char('6')|char('7')|char('8')|char('9'))
        .map { _.toString.toInt }

    def intBetween (low: Int, high: Int): P[Option[Int]] =
      import p.*
      INT.map { n => Some(n).filter { n => n >= low && n <= high } }
         .flatMap { succeed }

    // almost identical with for-yield
    def intBetween1 (low: Int, high: Int): P[Option[Int]] =
      import p.*
      for n <- INT
      yield { Some(n).filter (n => n >= low && n <= high) }

    // isn't this more cool
    def intBetween2 (low: Int, high: Int): P[Option[Int]] =
      import p.*
      INT.map (Some.apply)
         .map { _.filter { n => n >= low && n <= high } }

    // other solutions from the previous question adapt as well.

end IntervalParser2


/* QUESTION 10 ########################################################
 *
 * Implement a type class `Member[F[+_]]` that ensures that its
 * instances provide a method `contains`:
 *
 *   def contains[A] (fa: F[A], a: A): Bolean
 *
 * The intuition is that this method can be used to check whether fa
 * contains the element a (although this is relevant for the task at
 * hand).  The type class should be implemented as a trait.
 */

// Add your answer here (bnlow)
trait Member[F[+_]]:
  def contains[A] (fa: F[A], a: A): Boolean



/* QUESTION 11 ########################################################
 *
 * Read the following interface extracted from a railway ticketing system.
 * The question is formulated underneath.
 *
 * The train reservation system accepts payments and creates
 * reservations. Each of the four methods is commented below.  We
 * assume this interface is imperative, so most of the functions have
 * side effects. But this does not matter for the questions below.
 **/

object Trains:
  trait ReservationSystem:
    // Return paymentId if successfully charged the amount;
    // otherwise error message
    def pay (CreditCard: String, amount: Int): Either[String, String]
    // Create a reservation, returns a ticket number if successful,
    // otherwise error message
    def reserve (passenger: String, train: String, paymentId: String)
      : Either[String, String]
    // Confirms the validity of the payment with a broker.
    // True if the paymentId is valid
    def validate (paymentId: String): Boolean
    // Returns a set of passengers on the train (a manifest)
    def paxOnTrain (train: String): Set[String]


object FullyAbstractTrains:

  /* Design a fully abstract version of the ReservationSystem
   * interface shown above. In particular abstract away from the
   * details of representation of credit cards, amounts, error
   * messages,passanger names, train numbers, ticket numbers, and
   * payment ids.
   *
   * Because we may be using a distributed data store, we want to
   * abstract away from the representation of sets as query results.
   * We want to however assume that whatever representation we use is
   * a Monad, so that map and flatMap are available, and that we can
   * check whether query results contain an element. The latter
   * requires using the solution of Q10.
   */

  // trait ReservationSystem ... // your solution here


  // The two type class constraints in the QueryResults are a bit
  // tricky. Not sure that we have seen this syntax in the course. But
  // the editor can help (and the internet)
  trait ReservationSystem[
    QueryResult[+_]: adpro.monad.Monad: Member,
    Amount: Arbitrary, Pax: Arbitrary, TrainNo: Arbitrary, TicketNo,
    PaymentId: Arbitrary, CC: Arbitrary, Err
  ]:
    def pay (CreditCard: CC, amount: Amount): Either[Err,PaymentId]
    def reserve (p: Pax, t: TrainNo, pmnt: PaymentId): Either[Err,TicketNo]
    def validate (pmnt: PaymentId): Boolean
    def paxOnTrain (train: TrainNo): QueryResult[Pax]

    // NB. This is the (nonsense) answer from copilot
    // trait ReservationSystem[F[_]]:
    //   def pay (CreditCard: F[String], amount: F[Int]): F[Either[F[String], F[String]]]
    //   def reserve (passenger: F[String], train: F[String], paymentId: F[String]): F[Either[F[String], F[String]]]
    //   def validate (paymentId: F[String]): F[Boolean]
    //   def paxOnTrain (train: F[String]): F[F[Set[F[String]]]]

    /* QUESTION 12 ######################################################
     *
     * We want to write some property laws for the fully abstract
     * version of the train reservation system. These tests we cannot
     * run before the implementation is concrete. But they should
     * compile, to support test-first development.
     *
     * Note this question depends on Q5-6. There are two laws to be
     * written below.
     */

    /* Law 1. A succesful Payment produces a valid PaymentId */

    // def law1: Prop = ???

    def law1: Prop =
      forAll { (cc: CC, amount: Amount) =>
        pay (cc, amount).map (validate).isRight }

    /* Law 2. A succesful reservation puts the passenger on the
     * requested train (relates `reserve` with `paxOnTrain`). If
     * `reserve` succeeds then paxOnTrain returns a result containing
     * the passenger.)
     */

    // def law2: Prop = ???

    def law2: Prop =
      forAll { (p: Pax, t: TrainNo, pmnt: PaymentId) =>
        reserve (p, t, pmnt)
          .map { tn =>
            summon[Member[QueryResult]].contains (paxOnTrain(t), p) }
          .getOrElse (true)
      }

end FullyAbstractTrains

// vim:tw=76:cc=70
