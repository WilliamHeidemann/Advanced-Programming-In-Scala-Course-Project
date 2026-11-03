// Advanced Programming, Andrzej Wasowski
// Probabilistic Programming (AKA Probability is also a monad)
package adpro.prob

import probula.*

// Used by samplers
given rng: spire.random.rng.SecureJava
  = spire.random.rng.SecureJava.apply

val M = 42

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

def pick (n: Int): Dist[Ball] =
  ???

// Exercise 2.

def move(player: Player, n: Int): Dist[Player] =
  ???

// Exercise 3.

// Probability that Paula wins given Paula starts (with BallsNo balls)
def probPaulaStarts: Prob =
  ???

// Probability that Paula wins given Peter starts (with BallsNo balls)
def probPeterStarts: Prob =
  ???


// Exercise 4.

// no code, solve on paper

// Exercise 5.

def gameResult: Dist[(Player, Player)] =
  ???

// probability that Paula won, not knowing who started; first the model
// conditioned that Paula wins
lazy val gameWonByPaula: Dist[(Player, Player)] =
  ???

// now the probability value
lazy val paulaWinning: Prob =
  ???

// probability that Paula started given she won
lazy val probPaulaStarted: Prob =
  ???

// Exercise 6.

val UpperBound = 6

// The distribution over the number of balls given that Paula has won
lazy val paulaWon: Dist[Int] =
  ???

// The probability of the number of balls being odd, giventhat Paula has won
lazy val posteriorOdd: Prob =
  ???
