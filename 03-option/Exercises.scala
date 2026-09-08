// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.option

// Exercise 1

trait OrderedPoint
  extends scala.math.Ordered[java.awt.Point]:

  this: java.awt.Point =>

  override def compare(that: java.awt.Point): Int = {
    if this.x < that.x then -1
    else if this.x == that.x then
      if this.y < that.y then -1
      else if this.y == that.y then 0
      else 1
    else 1
  }

// Try the following (and similar) tests in the repl (sbt console):
//
// import adpro.option.*
// val p = new java.awt.Point(0, 1) with OrderedPoint
// val q = new java.awt.Point(0, 2) with OrderedPoint
// assert(p < q)


// Chapter 3 Exercises

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

object Tree:

  // Exercise 2

  def size[A](t: Tree[A]): Int = t match
    case Leaf(value: A) => 1
    case Branch(left: Tree[A], right: Tree[A]) => size(left) + size(right) + 1

  // Exercise 3

  def maximum(t: Tree[Int]): Int = t match
    case Leaf(value: Int) => value
    case Branch(left: Tree[Int], right: Tree[Int]) => maximum(left) max maximum(right)


  // Exercise 4

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match
    case Leaf(value: A) => Leaf(f(value))
    case Branch(left: Tree[A], right: Tree[A]) => Branch(map(left)(f), map(right)(f))


  // Exercise 5

  def fold[A, B](t: Tree[A])(f: (B, B) => B)(g: A => B): B = t match
    case Leaf(value: A) => g(value)
    case Branch(left: Tree[A], right: Tree[A]) => f(fold(left)(f)(g), fold(right)(f)(g))

  def size1[A](t: Tree[A]): Int = fold(t)((left: Int, right: Int) => left + right + 1)(_ => 1)

  def maximum1(t: Tree[Int]): Int = fold(t)((left: Int, right: Int) => left max right)(x => x)

  def map1[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]]
      (t: Tree[A])
      ((left: Tree[B], right: Tree[B]) => Branch(left, right))
      ((value: A) => Leaf(f(value)))


enum Option[+A]:
  case Some(get: A)
  case None

  // Exercise 6

  def map[B](f: A => B): Option[B] = this match
    case Some(get) => Some(f(get))
    case None => None

  def getOrElse[B >: A](default: => B): B = this match
    case Option.Some(get) => get
    case Option.None => default

  def flatMap[B](f: A => Option[B]): Option[B] = this match
    case Some(get) => f(get)
    case None => None

  def filter(p: A => Boolean): Option[A] = this match
    case Some(get) => if p(get) then Some(get) else None
    case None => None

  // Scroll down for Exercise 7, in the bottom of the file, outside Option

  def forAll(p: A => Boolean): Boolean = this match
    case None => true
    case Some(a) => p(a)

end Option

// Exercise 9

def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A, B) => C): Option[C] =
  ao.flatMap(a => bo.map(b => f(a, b)))

def map2for[A, B, C](ao: Option[A], bo: Option[B])(f: (A, B) => C): Option[C] =
  for
    a <- ao
    b <- bo
  yield f(a, b)

// Exercise 10

def sequence[A](aos: List[Option[A]]): Option[List[A]] =
  aos.foldRight[Option[List[A]]](Some(Nil))((ao: Option[A], accOption: Option[List[A]]) =>
    ao.flatMap(a => accOption.map(acc => a :: acc))
  )

// Exercise 11

def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
  as.foldRight[Option[List[B]]](Some(Nil))((a: A, accOption: Option[List[B]]) =>
    accOption.flatMap(acc => f(a).map(b => b :: acc))
  )




// Exercise that are outside the Option companion object

import Option.{Some, None}

def headOption[A](lst: List[A]): Option[A] = lst match
  case Nil => None
  case h :: t => Some(h)

// Exercise 7

def headGrade(lst: List[(String, Int)]): Option[Int] =
  headOption(lst).map(tuple => tuple._2)

def headGrade1(lst: List[(String, Int)]): Option[Int] =
  for
    tuple <- headOption(lst)
  yield tuple._2

// Implemented in the text book

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

// Exercise 8

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs)
    .map(m => xs.map(x => (m - x) * (m - x)))
    .flatMap(diffs => mean(diffs))

def variance1(xs: Seq[Double]): Option[Double] =
  for
    m <- mean(xs)
    v <- mean(xs.map(x => (m - x) * (m - x)))
  yield v

// Scroll up, to the Option object for Exercise 9
