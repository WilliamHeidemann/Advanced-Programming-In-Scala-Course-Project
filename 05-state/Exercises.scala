// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.state

import adpro.lazyList.LazyList
import adpro.lazyList.LazyList.*
import adpro.state.RNG.SimpleRNG


trait RNG:
  /** Generate a random `Int`. We define other functions using `nextInt`. */
  def nextInt: (Int, RNG)

object RNG:

  case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      // The next state, which is an `RNG` instance created from the new seed. 
      val nextRNG = SimpleRNG(newSeed)
      // `>>>` is right binary shift with zero fill. 
      // The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG)


  // Exercise 1

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r1) = rng.nextInt
    val iPositive = if i == Int.MinValue then 0 else i.abs
    (iPositive, r1)
  }

  // Exercise 2

  def double(rng: RNG): (Double, RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue.toDouble, r1)
  }

  // Exercise 3

  // The return type is broken and needs to be fixed
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  // The return type is broken and needs to be fixed
  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (d, r1) = double(rng)
    val (i, r2) = nonNegativeInt(r1)
    ((d, i), r2)

  // Exercise 4

  // The return type is broken and needs to be fixed
  def ints(size: Int)(rng: RNG): (List[Int], RNG) = {
    def recurse(acc: List[Int], x: Int, rng: RNG): (List[Int], RNG) =
      if x <= 0 then (acc.reverse, rng)
      else
        val (i, r1) = rng.nextInt
        recurse(i :: acc, x - 1, r1)

    recurse(List.empty, size, rng)
  }


  type Rand[+A] = RNG => (A, RNG)

  lazy val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)


  def nonNegativeEven: Rand[Int] = map(nonNegativeInt) { i => i - i % 2 }

  // Exercise 5

  lazy val double2: Rand[Double] =
    map[Int, Double](nonNegativeInt)(i => i.toDouble / Int.MaxValue.toDouble)

  // Exercise 6

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      val c = f(a, b)
      (c, r2)

  // Exercise 7

  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
    ras.foldRight(unit(List.empty[A]))((ra, acc) => map2(ra, acc)((a, as) => a :: as))

  def ints2(size: Int): Rand[List[Int]] = {
    val initial: List[Rand[Int]] = List.fill[Rand[Int]](size)(int)
    sequence(initial)
  }

  // Exercise 8

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng =>
      val (a, r1) = f(rng)
      g(a)(r1)

  def nonNegativeLessThan(bound: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => unit(i % bound))

end RNG

import State.*

case class State[S, +A](run: S => (A, S)):

  // Exercise 9 (methods in class State)
  // Search for the second part (sequence) below

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State {
      s =>
        val (a, s1) = run(s)
        f(a).run(s1)
    }
  }

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  //    State {
  //      s =>
  //        val (a, s1) = run(s)
  //        val b = f(a)
  //        (b, s1)
  //    }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

//    State {
//      s =>
//        val (a, s1) = run(s)
//        val (b, s2) = sb.run(s1)
//        (f(a, b), s2)
//    }


object State:

  def unit[S, A](a: A): State[S, A] =
    State { s => (a, s) }

  def modify[S](f: S => S): State[S, Unit] = for
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // Now Rand can be redefined like this (we keep it here in the State object,
  // to avoid conflict with the other Rand in RNG).
  type Rand[A] = State[RNG, A]

  // Exercise 9 (sequence, continued)

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight[State[S, List[A]]](unit(List.empty))((s, acc) => s.map2(acc)((a, b) => a :: b))

  import adpro.lazyList.LazyList

  // Exercise 10 (stateToLazyList)

  def stateToLazyList[S, A](s: State[S, A])(initial: S): LazyList[A] = {
    val (a, s1) = s.run(initial)
    cons(a, stateToLazyList(s)(s1))
  }

  // Exercise 11 (lazyInts out of stateToLazyList)

  def lazyInts(rng: RNG): LazyList[Int] =
    stateToLazyList[RNG, Int](State { rng => rng.nextInt })(rng)


  lazy val tenStrictInts: List[Int] =
    lazyInts(SimpleRNG(42)).take(10).toList

end State
