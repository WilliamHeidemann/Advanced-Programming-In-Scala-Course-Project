/* These tests have not been available for the students writing the exam. */
package adpro.solution

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.*
import org.scalactic.TripleEquals.*


import adpro.laziness.LazyList

object StreamingSpec
  extends org.scalacheck.Properties("list_____"):

  property("00 fViaFold compiles, doesn't crash (fixed list)") = 
    Streaming.fViaFold(LazyList(1, 2, 4, 3, 5, 6, 7, 8, 7, 7, 42)) === 6

  property("01 fViaFold compiles, doesn't crash (empty list)") = 
    Streaming.fViaFold(LazyList.Empty) === 0

  given arb [A: Arbitrary]: Arbitrary[LazyList[A]] = 
    val genLA = summon[Arbitrary[List[A]]].arbitrary
    val genLLA = genLA.map { l => LazyList(l*) }
    Arbitrary(genLLA)

  property("02 fViaFold compiles, doesn't crash (list of evens)") = 
    forAll { (l: LazyList[Int]) => Streaming.fViaFold(l.map { _*2 }) ?= 0 }

  property("03 fViaFold compiles, doesn't crash (random list)") = 
    forAll { (l: LazyList[Int]) => Streaming.fViaFold(l); true }

  // A copy to prevent injection of broken spec
  def fViaRec (l: LazyList[Int]): Int = 
    def doRec (l: LazyList[Int], z: Int): Int = 
      l match 
        case LazyList.Cons(hd, tl) => 
          if hd() % 2 == 1 
          then doRec(tl(), z+1) 
          else doRec(tl(), z)
        case LazyList.Empty => z

    doRec (l,0)

  property("04 fViaFold is same as fViaRec") = 
    forAll { (l: LazyList[Int]) => Streaming.fViaFold(l) ?= fViaRec(l) }

end StreamingSpec



object ParsingSpec
  extends org.scalacheck.Properties("parsing__"):

  import adpro.parsing.*, adpro.parsing.Sliceable.*

  val linesDifferentInp = "1,2, 3 ,5 , 4\n 42,42"
  val linesDifferentExp = List(List(1,2,3,5,4), List(42,42))

  val linesSameInp  = " 1,2,3 ,5 \n4,42,42,42"
  val linesSameExp  = List(List(1,2,3,5), List(4,42,42,42))

  // This tests me, but also if the student made any changes to the parser
  property("00 parser behaves as spec'ed on the example #1") =
    val expected = Right(linesDifferentExp)
    val received = Parsing.parser.run(linesDifferentInp)
    received ?= expected

  property("01 parser behaves as spec'ed on the example #2") =
    val expected = Right(linesSameExp)
    val received = Parsing.parser.run(linesSameInp)
    received ?= expected

  property("03 longestLine on example #1") =
    Parsing.longestLine.run(linesDifferentInp) ?= Right(5)

  property("04 longestLine on example #2") =
    Parsing.longestLine.run(linesSameInp) ?= Right(4)

  property("05 allLinesTheSame on example #1") =
    Parsing.allLinesTheSame.run(linesDifferentInp) ?= Right(false)

  property("06 allLinesTheSame on example #2") =
    Parsing.allLinesTheSame.run(linesSameInp) ?= Right(true)

end ParsingSpec



object GameSpec
  extends org.scalacheck.Properties("probabil_"):

  val N = 10000
  val ε = 0.05

  import Game.{Alice, Bob}, Game.Player.*, Game.Move.* 
  
  // Used by samplers
  given rng: spire.random.rng.SecureJava 
    = spire.random.rng.SecureJava.apply

  property("00 Alice is uniform (Rock)") = 
    (Alice.sample(N).pr(Game.Move.Rock) - 0.333333).abs <= ε

  property("01 Alice is uniform (Paper)") = 
    (Alice.sample(N).pr(Game.Move.Paper) - 0.333333).abs <= ε

  property("02 Alice is uniform (Scissors)") = 
    (Alice.sample(N).pr(Game.Move.Scissors) - 0.333333).abs <= ε

  property("03 Bob takes Rock with half") = 
    (Bob.sample(N).pr(Game.Move.Rock) - 0.5).abs <= ε

  property("04 Bob takes Paper with half") = 
    (Bob.sample(N).pr(Game.Move.Paper) - 0.5).abs <= ε

  property("05 Bob never takes Scissors") = 
    Bob.sample(N).pr(Game.Move.Scissors) ?= 0

  property("06 Alice wins against Alice #1") =
    (Game.game(Alice, Alice).sample(N).pr(Some(P1)) - 0.333333).abs <= ε

  property("07 Alice wins against Alice #2") =
    (Game.game(Alice, Alice).sample(N).pr(Some(P2)) - 0.333333).abs <= ε

  property("08 Alice draws against Alice #3") =
    (Game.game(Alice, Alice).sample(N).pr(None) - 0.333333).abs <= ε

  property("09 Bob wins against Bob #1") =
    (Game.game(Bob, Bob).sample(N).pr(Some(P1)) - 0.25).abs <= ε

  property("10 Bob wins against Bob #2") =
    (Game.game(Bob, Bob).sample(N).pr(Some(P2)) - 0.25).abs <= ε

  property("11 Bob draws against Bob #3") =
    (Game.game(Bob, Bob).sample(N).pr(None) - 0.5).abs <= ε

  property("12 aliceFraction still 1/3") =
    (Game.aliceFraction - 0.33333).abs <= ε

  property("13 aliceFraction same as obtained from game") =
    (Game.aliceFraction - Game.game(Alice, Bob).sample(N).pr(Some(P1))).abs <= ε

end GameSpec



object LearningSpec
  extends org.scalacheck.Properties("learning_"):

  // Q RL TODO

  import monocle.*
  import monocle.syntax.applied.*
  import monocle.{Lens, Optional}

  // violates property 00 and 01
  def updateMutant00(q: RL.Q[Int, Int], state: Int, action: Int) 
    (reward: Double, estimate: Double): RL.Q[Int, Int] = 
    val qsa   = q(state)(action)
    val value = (1.0 - RL.α) * qsa + RL.α * (reward + RL.γ * estimate) + 0.01
    val av    = q(state) + (action -> value)
    q + (state -> av)

  // violates property 01 without violating property 00
  def updateMutant01(q: RL.Q[Int, Int], state: Int, action: Int) 
    (reward: Double, estimate: Double): RL.Q[Int, Int] = 
    val err   = if reward != 0.0 || state + action > 0 then 0.01 else 0.0
    val qsa   = q(state)(action)
    val value = (1.0 - RL.α) * qsa + RL.α * (reward + RL.γ * estimate) + err
    val av    = q(state) + (action -> value)
    q + (state -> av)

  // Run the student tests on the correct implementation
  private val solutionSpec = 
    new RL.NullUpdatesSpec(RL.update, "\b#correct#") {}
  for ((n,p) <- solutionSpec.properties) 
    property(n.replace("#correct#", "") + " [positive]") = p


  def negate (r: Result): Prop = 
    (r.status !== True) && (r.status !== Proof)

  // Run the student tests on a mutated implementation (fault injection)
  private val (n00, p00) = new RL.NullUpdatesSpec(updateMutant00, "\b#IGNORE#") {}
      .properties(0)
  property(n00.replace("#IGNORE#", "") + " [NEGATIVE]") = p00.flatMap(negate) 

  private val (n01, p01) = new RL.NullUpdatesSpec(updateMutant01, "\b#IGNORE#") {}
      .properties(1)
  property(n01.replace("#IGNORE#", "") + " [NEGATIVE]") = p01.flatMap(negate)



  type State = Int
  type Action = Int

  val NoOfActions: Action = 2
  val NoOfStates: State = 3

  def actG(nActions: Int = NoOfActions) = 
    Gen.choose(0, nActions-1)

  def staG(nStates: Int = NoOfStates) = 
    Gen.choose(0, nStates-1)

  val q0 = RL.qZero(NoOfStates, NoOfActions)

  property("02 new update doesn't crash on q0") = 
    RL.updateWithLens[State, Action](q0, 0, 0)(0.0, 0.0)
    true

  property("03 null update on qZero") = 
    val qL = RL.updateWithLens[State, Action](q0, 0, 0)(0.0, 0.0) 
    qL =? q0

  property("04 random new update doesn't crash on q0") = 
    forAll { (reward: Double, estimate: Double) => 
      forAll (staG(), actG()) { (state, action) => 
        RL.updateWithLens(q0, state, action)(reward, estimate)
        true
      }
    }

  property("05 new update is same as old on q0") =
    forAll { (reward: Double, estimate: Double) => 
      forAll(staG(), actG()) { (state, action) =>
        val qL = RL.updateWithLens(q0, state, action)(reward, estimate)
        val qU = RL.update(q0, state, action)(reward, estimate)
        qL ?= qU
      }
    }


  given qG: Arbitrary[RL.Q[State, Action]] = 
    Arbitrary(RL.qGen(2*NoOfActions, 2*NoOfStates))

  property("06 random update doesn't crash on qGen") = 
    forAll { (q: RL.Q[State, Action], reward: Double, estimate: Double) => 
      val m = q(0).size
      val n = q.size
      forAllNoShrink (staG(n), actG(m)) { (state, action) =>
        RL.updateWithLens[State, Action](q, state, action)(reward, estimate)
        true
      }
    }

  property("07 random update doesn't change size, qGen") = 
    forAll { (q: RL.Q[State, Action], reward: Double, estimate: Double) => 
      val m = q(0).size
      val n = q.size
      forAllNoShrink (staG(n), actG(m)) { (state, action) =>
        val qU = RL.updateWithLens[State, Action](q, state, action)(reward, estimate)
        (qU.size ?= n) && (qU(0).size ?= m)
      }
    }

  property("08 new update same as old, on random Q") = 
    forAll { (q: RL.Q[State, Action], reward: Double, estimate: Double) => 
      val m = q(0).size
      val n = q.size
      forAll (staG(n), actG(m)) { (state, action) =>
        val qL = RL.updateWithLens[Int, Int](q, state, action)(reward, estimate)
        val qU = RL.update[Int, Int](q, state, action)(reward, estimate)
        qL(state) =? qU(state)
      }
    }

end LearningSpec
