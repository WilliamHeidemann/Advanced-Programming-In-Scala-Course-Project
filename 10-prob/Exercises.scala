// Advanced Programming, Andrzej Wasowski
// Probabilistic Programming (AKA Probability is also a monad)
package adpro.prob

import probula.*

// Used by samplers
given rng: spire.random.rng.SecureJava
= spire.random.rng.SecureJava.apply

val M = 4000

enum Player:
  case Peter, Paula

import Player.*

def next(player: Player): Player = player match
  case Peter => Paula
  case Paula => Peter

val BallsNo: Int = 8

// Exercise 1.

enum Ball:
  case Red, Black

import Ball.*

def pick(n: Int): Dist[Ball] =
  Probula.bernoulli[Ball]("Pick")((1.0 / (1 + n)).pr, Red, Black)

// Exercise 2.

def move(player: Player, n: Int): Dist[Player] = {
  if n <= 0 then Dirac(player)
  else
    pick(n).flatMap(ball => ball match {
      case Ball.Red => Dirac(player)
      case Ball.Black => move(next(player), n - 1)
    })
}

// Exercise 3.

// Probability that Paula wins given Paula starts (with BallsNo balls)
def probPaulaStarts: Prob =
  move(Paula, BallsNo - 1).sample(M.sampleSize).pr(Paula)

// Probability that Paula wins given Peter starts (with BallsNo balls)
def probPeterStarts: Prob =
  move(Peter, BallsNo - 1).sample(M.sampleSize).pr(Paula)

// Exercise 4.

// no code, solve on paper

// Exercise 5.

def gameResult: Dist[(Player, Player)] =
  for
    starter <- Uniform("starter")(Paula, Peter)
    winner <- move(starter, BallsNo - 1)
  yield (starter, winner)

// probability that Paula won, not knowing who started; first the model
// conditioned that Paula wins
lazy val gameWonByPaula: Dist[(Player, Player)] =
  gameResult
    .condition((starter: Player, winner: Player) => winner == Paula)

// now the probability value
lazy val paulaWinning: Prob =
//  gameResult.sample(M.sampleSize).pr(Player, Paula)
  gameResult.sample(M.sampleSize).pr((_: Player, winner: Player) => winner == Paula)

// probability that Paula started given she won
lazy val probPaulaStarted: Prob =
  gameWonByPaula.sample(M.sampleSize).pr((starter: Player, _: Player) => starter == Paula)

// Exercise 6.

val UpperBound = 6

val model: Dist[(Int, Player)] =
  for
    n <- Uniform("n")(1 to UpperBound *)
    winner <- move(Paula, n - 1)
  yield (n, winner)

// The distribution over the number of balls given that Paula has won
lazy val paulaWon: Dist[Int] =
  model.condition((n: Int, winner: Player) => winner == Paula)
    .map((n: Int, winner: Player) => n)

// The probability of the number of balls being odd, giventhat Paula has won
lazy val posteriorOdd: Prob =
  paulaWon.sample(M.sampleSize).pr((n: Int) => n % 2 == 1)
