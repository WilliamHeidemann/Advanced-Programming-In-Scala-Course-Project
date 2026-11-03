// Advanced Programming, A. Wąsowski, IT University of Copenhagen
package adpro.prob

import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean, forAllNoShrink}
import org.scalacheck.Arbitrary.arbitrary


import probula.*

object ProbSpec
  extends org.scalacheck.Properties("prob"):

  // Used by samplers below
  given rng: spire.random.rng.SecureJava
    = spire.random.rng.SecureJava.apply

  val Err: Prob = 0.05.pr

  /** Sample size for tests */
  val N = 4000.sampleSize

  // n is the number of black balls (not the total number of balls)
  // returns the probability of the player making a move winning with
  // n black balls in the urn
  def analyticalP1(n: Int): Prob =
    if n % 2 == 0
    then ((1 + n/2) / (n + 1.0)).pr // see notes below
    else 0.5.pr

  // n = 3 (#black)
  // 1/4 + 1/2 * (3/4*2/3) = 1/4 + 1/4 = 1/2
  // 1/3 * 3/4 + (3/4*2/3*1/2) = 1/4 + 1/4 = 1/2

  // n = 4 (#black)
  // 1/5 + (4/5 * 3/4) * 1/3 + (4/5 * 3/4 * 1/3 * 1/2) = 3/5
  // 1/4 * 4/5 + (4/5 * 3/4 * 2/3) * 1/2 = 1/5 + 1/5 = 2/5
  //
  //  (1 + n div 2) / (n+1) for the first player
  //  (n div 2) / (n+1)     for the second player

  // Exercise 6

  property("Ex06.01: posterior of an odd urn after Player1 wins > 1/2") =
    println (posteriorOdd)
    s"Prob. that we had an odd-sized urn: $posteriorOdd" |:
      posteriorOdd > 0.5.pr

  // Exercise 5

  property(s"Ex05.01: Pr. Paula started given she won, w/ ${BallsNo} balls") =
     if BallsNo % 2 == 0
     then probPaulaStarted.about(0.5.pr, Err)
     else probPaulaStarted > 0.5.pr

  property("Ex05.02: gameWonByPaula conditioned on Paula's win = 1.0") =
    gameWonByPaula._2.sample(N).pr(Player.Paula) == 1.0.pr

  property(s"Ex05.03: gameResult with $BallsNo balls & uniform prior is 1/2") =
    val p = gameResult._2.sample(N).pr(Player.Paula)
    p.about(0.5.pr, Err)

  // Exercise 3

  property(s"Ex03.01: probPaulaStarts with $BallsNo balls in the urn") =
    val expected = analyticalP1(BallsNo - 1)
    probPaulaStarts.about(expected, Err)

  property(s"Ex03.02: probPeterStarts with $BallsNo balls in the urn") =
    val expected = 1.0.pr - analyticalP1(BallsNo - 1)
    probPeterStarts.about(expected, Err)


  // Exercise 2

  property("Ex02.01: move") =
    forAllNoShrink (Gen.oneOf(0 to 7)) { (n: Int) =>
      val p1 = move(Player.Peter, n).sample(N).pr(Player.Paula)
      val p2 = move(Player.Paula, n).sample(N).pr(Player.Paula)
      { p1.about(1.0.pr - analyticalP1(n), Err) }:| s"p1=$p1 is incorrect."
        && { p2.about(analyticalP1(n), Err) } :| s"p2=$p2 is incorrect."
    }

  // Exercise 1

  property("Ex01.01: pick") =
    forAll(Gen.choose(0, 10)){ (nBalls: Int) =>
      val expected = 1.0 / (nBalls + 1.0)
      val p = pick(nBalls).sample(N).pr(Ball.Red)
      p.about(expected.pr, Err)
    }
