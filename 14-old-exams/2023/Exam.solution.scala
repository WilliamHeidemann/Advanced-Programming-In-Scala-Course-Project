/*********************************************************************
 * Final Exam: Advanced Programming, by Andrzej Wąsowski
 * IT University of Copenhagen, Autumn 2023: 05 January 2024
 *
 * The exam consists of eight questions to be solved within 4 hours.
 *
 * You can use all functions provided in the included implementation
 * files (textbook, exercises) as well as functions we implemented in
 * the course (if the source is missing, you may need to copy it in,
 * so that things compile). 
 *
 * You can access any static written material, also online, but you
 * are not allowed to communicate with anybody or with anything
 * (bots). Using GitHub copilot during exam is not allowed. By
 * submitting you legally declare to have solved the problems alone,
 * without communicating with anybody.
 *
 * Solve the tasks in the file 'Exam.scala' (this file) found in zip
 * archive made available on LearnIt.
 *
 * Do not modify the questions file in any other places than marked
 * for answers with three question marks.
 *
 * Submit this file and only this file to learnIT. Do not convert to
 * any other format than .scala. Do not submit the entire zip archive.
 * Do not reorder the answers, and do not remove question numbers from
 * the file. The only accepted file format is '.scala'.
 *
 * Keep the solutions within 80 columns width to make grading easier.
 *
 * The answers will be graded manually. We focus on the correctness of
 * ideas, the use of concepts, clarity, and style.
 *
 * We do require that the file compiles.  The directory has a project
 * setup so compilation with scala-cli shall work out-of-the-box.
 *
 * If you cannot make a fragment compile, put your solution in a
 * comment, next to the three question marks. We will grade the
 * solutions in comments as well.
 *
 * Files that do not compile will automatically fail the exam. 
 *
 * The size of the exam has been adjusted from previous years so that
 * you have time to work with the compiler. We do not recommend to run
 * and test the code if you are pressed for time. Only compile. On the
 * other hand, it is a good idea to run and test, if you have time. 
 *
 * We will use undisclosed automatic tests during grading, but not to
 * compute the final grade, but to help us debug your code.
 *
 * Good luck!
 ********************************************************************/
package adpro.solution

import org.scalacheck.{Arbitrary, Gen, Prop}
import Arbitrary.*, Prop.*
import org.scalactic.TripleEquals.*

import adpro.laziness.LazyList
import adpro.state.*
import adpro.monoids.Foldable
import adpro.parallelism.Par
import adpro.monads.Monad

object Streaming: 

  
  /* QUESTION 1 ######################################################
   *
   * Study the following recursive function. Then implement exactly
   * the same semantics using a fold.
   */

  def fViaRec (l: LazyList[Int]): Int = 
    def doRec (l: LazyList[Int], z: Int): Int = 
      l match 
        case LazyList.Cons(hd, tl) => 
          if hd() % 2 == 1 
          then doRec(tl(), z+1) 
          else doRec(tl(), z)
        case LazyList.Empty => z

    doRec (l,0)


  // This is closer to fViaRec
  def fViaFold (l: LazyList[Int]): Int = 
    l.foldLeft(0) { (z,n) => if n % 2 == 1 then z+1 else z }

  // This is more elegant, unfortunately does not work 
  // for negative numbers in the list (it has a different 
  // semantics than fViaRec)
  def fViaFold_ (l: LazyList[Int]): Int = 
    l.foldLeft(0) { (z,n) => z + (n % 2) }

end Streaming



object Parsing:

  import adpro.parsing.*
  import adpro.parsing.Sliceable.*

  /* QUESTION 2 ######################################################
   *
   * The folowing parser parses CSV (comma separated values) of
   * integer numbers (and  no other types). The type of values
   * produced by a succesful parse is List[List[Int]].
   *
   * Example input:
   *
   * 1,2,3 , 5,4
   * 42,42
   *
   * Example output: 
   * List(List(1,2,3,5,4), List(42,42))
   *
   * Each line in the file contains integers separated with commas
   * (',') and possibly white space (tab or space character).  Lines
   * are separated by '\n'. The last line must not be terminated by
   * '\n'.  There can be no empty lines.   *
   *
   * Write a parser `longestLine` of type Parser[Int] that returns the
   * maximum number of columns in the longest line in the file,
   * measured by the number of integers in the line.
   */

  val WS: Parser[String] = regex("""[ \t]+""".r)

  val NL: Parser[String] = string("\n")

  val INT: Parser[Int] = 
    regex("""(\+|-)?[0-9]+""".r).map { _.toInt }

  val commaSeparatedInts: Parser[List[Int]] =
    { WS.? |* INT *| WS.? ** (string(",") |* WS.? |* INT *| WS.?).* }
      .map { (h,t) => h::t }

  lazy val parser: Parser[List[List[Int]]] =
    { commaSeparatedInts ** { ( NL |* commaSeparatedInts ) }.* }
      .map { (h,t) => h::t }

  lazy val longestLine: Parser[Int] = 
    parser.map { l => l.map {_.size}.max }


  
  /* QUESTION 3 ######################################################
   *
   * Write a parser of type Parser[Boolean] that parses a
   * CSV input and returns true if all lines in the input have the
   * same number of elements. 
   *
   * NB. This question does not require that you answered QUESTION 2.
   */

  val allLinesTheSame: Parser[Boolean] = 
    parser.map { l => l.map {_.size}.toSet.size == 1 }

end Parsing



object Game:

  import pigaro.*

  /* QUESTION 4 ######################################################
   *
   * Consider the old Chinese game of rock, paper, and scissors. Two
   * players independently  and simultaneously  select a  move.  The
   * possible moves are  Rock, Paper, and Scissors.   The winning is
   * established using the following rules:
   *
   * Rock wins against Scissors
   * Paper wins against Rock
   * Scissors win against Paper
   * Otherwise there is a draw.
   *
   * The function `winner` encodes the rules o the game, i.e. which
   * player wins in each pair of moves. `None` represents a draw.
   *
   * Continue reading below.
   */

  enum Move:
    case Rock, Paper, Scissors

  enum Player: 
    case P1, P2

  import Move.*, Player.*

  type Result = Option[Player]

  def winner(player1: Move, player2: Move): Result = (player1, player2) match
    case (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) => Some(P1)
    case (Scissors, Rock) | (Rock, Paper) | (Paper, Scissors) => Some (P2)
    case _ => None


  /* A player's strategy is a distribution indicating what is the
   * probability distribution over their next move (Dist[Move]).
   *
   * Create two strategies: Alice and Bob:
   *
   * a) Alice picks Rock, Paper, and Scissors with uniform
   * probability.
   *
   * b) Bob Picks Rock and Paper with probability 0.5 each, and never
   * picks Scissors.
   *
   * Links to relevant documentation 
   * https://www.itu.dk/people/wasowski/ula/scala-doc/index.html
   */

  type Strategy = Dist[Move]

  lazy val Alice: Strategy = Pigaro.uniform("alice")(Rock, Paper, Scissors)

  lazy val Bob: Strategy = Pigaro.uniform("bob")(Rock, Paper)



  /* QUESTION 5 ######################################################
   *
   * Implement a function that computes a result of a game given two
   * probabilistic strategies. This function will give a probability
   * of winning for each player, and a probability of a draw as a
   * Dist[Result].
   *
   * You can use the function 'winner' from above. Note that answering
   * the previous question is not required to answer this one.
   */

  def game (player1: Strategy, player2: Strategy): Dist[Result] = 
    (player1 -> player2)
      .map { case (p1, p2) => winner(p1, p2) }

  // even nicer:
  def game_ (player1: Strategy, player2: Strategy): Dist[Result] = 
    player1.map2(player2)(winner)




  /* QUESTION 6 ######################################################
   *
   * Obtain a sample of 10000 outcomes and  estimate the  probability
   * that Alice is winning when playing against  Bob.  You do not have
   * to report the number,  just show the Scala code  that computes
   * the probability.
   *
   * The correct answer will compile even if you leave the two above
   * questions unanswered.
   */
  
  given rng: spire.random.rng.SecureJava 
    = spire.random.rng.SecureJava.apply

  lazy val aliceFraction: Double = 
    game(Alice, Bob).sample(10000).pr(Some(P1)) 

end Game



object RL: 

  import monocle.*
  import monocle.syntax.all.*
  import monocle.function.all.*
  import monocle.function.Index
  import monocle.PLens.lensChoice
 
  /* QUESTION 7 ######################################################
   * The type Q below represents Q-Tables in reinforcement learning,
   * mapping states to actions to expected rewards. The function
   * update below produces a new q-table which has been modified for
   * the given state and action, using a new value of rewards and
   * estimate.  Note that the details of how this update happens are
   * unimportant for the question.
   *
   * Assume that we only perform an update for a state and action for
   * which the Q-table is defined.
   *
   * We consider the case where both the State and Action types are
   * both Int. The type of the specialized update is written below as
   * `IntUpdate` for convenience.  Our goal will to test the update
   * function, and we will be testing it on the Int,Int case.
   *
   * Continue reading below.
   * */

  type Q[State, Action] = Map[State, Map[Action, Double]]

  val α = 0.5
  val γ = 1.0

  def update[State, Action](q: Q[State, Action], state: State, action: Action) 
    (reward: Double, estimate: Double): Q[State, Action] = 
    val qsa   = q(state)(action)
    val value = (1.0 - α) * qsa + α * (reward + γ * estimate) /* MARK */
    val av    = q(state) + (action -> value)
    q + (state -> av)

  type IntUpdate = 
    (Q[Int, Int], Int, Int) => (Double, Double) => Q[Int, Int]

  
  /* We will test on q-tables initialized with zero reward values for
   * all actions and states.  The function qZero below, is a
   * convenince device filling in a Q-table of size nStates by
   * nActions with zero'es.  States are counted from 0 to nStates-1
   * inclusively, and actions are counted from 0 to nActions-1
   * inclusively.
   *
   * Continue reading below.
   */

  def qZero(nStates: Int, nActions: Int): Q[Int, Int] = 
    val actionMap: Map[Int, Double] = 
      List.fill(nActions)(0.0).zipWithIndex.map(_.swap).toMap
    List.fill(nStates)(actionMap).zipWithIndex.map(_.swap).toMap

  /* We will also test on randomly initialized qTables, which are
   * created using the Scalacheck generator below.
   *
   * Continue reading below.
   */

  val genRewards = Gen.choose(-100, 100).map(_.toDouble)
  def qGen(nStates: Int, nActions: Int): Gen[Q[Int, Int]] = for 
    av <- Gen.listOfN[Double](nActions, genRewards)
    av1 = av.zipWithIndex
            .map(_.swap)
            .toMap
    sav = List.fill(nStates)(av1)
              .zipWithIndex
              .map(_.swap)
              .toMap
  yield sav


  abstract class NullUpdatesSpec(update: IntUpdate, name: String) 
    extends org.scalacheck.Properties(name): 

    /* Write a **scenario** test that updates a zero-initialized 2 x 3
     * q-table (States in 0,1; Actions in 0,1,2) on the position (0),(0)
     * with values (0.0, 0.0) for the `reward` and `estimate` arguments.
     * The test should check that the obtained q-table is still the same
     * as the input table (such operation does nothing interesting).
     *
     * Continue reading below.
     */

    property("00 Null update on null table 2x3") = 
      update(qZero(2, 3), 0, 0)(0.0, 0.0) == qZero(2, 3)

    /* QUESTION 8 *********************************************************
     * Write a **property** test that checks a similar property on
     * **randomly initialized** Q-Tables of the same size 2 x 3. The
     * update should be performed on any position (for any state, and
     * any action within the range), not only at (0,0) as above. Also
     * for the `reward` argument use the value stored in the q-table
     * on this position (instead of zero). The estimate is sitll zero.
     *
     * Check that the resulting table (under these conditions) is the
     * same as the input.
     */

    property("01 Null update on null table 2x3") = 
      forAllNoShrink(qGen(2,3)) { q => 
        forAll(Gen.choose(0, 1), Gen.choose(0, 2)) { (s, a) =>
          val qU = update(q,s,a)(q(s)(a), 0.0)
          qU ?= q
        }
      }

  end NullUpdatesSpec


  /* QUESTION 9 ######################################################
   * We want to rewrite the update function using lenses.  The lens
   * below, implemented using the monocle library, projects a q-table
   * at its value at position (s,a) (i.e, state and action). 
   *
   * Reimplement the function 'update' to use this lens. Note that the
   * lens is used to obtain and replace the reward value. The actual
   * update calculation (marked MARK in the update function above)
   * will not change in this implementation.
   *
   * Assume that we only perform an update for a state and action for
   * which the Q-table is defined.
   *
   * This question can be answered independently of the previous one.
   */

  def lens[S, A] (s: S, a: A): Optional[Q[S,A], Double] = 
    val l1 = summon[Index[Q[S, A], S, Map[A, Double]]].index(s)
    val l2 = summon[Index[Map[A, Double], A, Double]].index(a)
    l1.andThen(l2)
  
  def updateWithLens[State, Action] (q: Q[State, Action], s: State, a: Action)
    (reward: Double, estimate: Double): Q[State, Action] =
    val qsa   = lens(s, a).getOption(q).get
    val value = (1 - α) * qsa + α * (reward + γ * estimate)
    lens(s, a).replace(value)(q)

end RL
