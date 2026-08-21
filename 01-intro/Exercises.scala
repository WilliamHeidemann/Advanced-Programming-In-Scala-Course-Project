// Advanced Programming, A. Wąsowski, IT University of Copenhagen Based on Functional Programming in Scala, 2nd Edition

package adpro.intro

object MyModule:

  def abs(n: Int): Int =
    if n < 0 then -n else n

  // Exercise 1

  def square(n: Int): Int =
    n * n

  private def formatAbs(x: Int): String =
    s"The absolute value of ${x} is ${abs(x)}"

  private def formatSquare(x: Int): String =
    s"The square of ${x} is ${square(x)}"

  val magic: Int = 42
  var result: Option[Int] = None

  @main def printAbs: Unit =
    assert(magic - 84 == magic.-(84))
    println(formatAbs(magic - 100))
    println(formatSquare(8))

end MyModule

// Exercise 2 requires no programming

// Exercise 3

def fib(n: Int): Int = {
  @annotation.tailrec
  def fibRecursive(c: Int, prev: Int, current: Int): Int = {
    if c == n then current
    else fibRecursive(c + 1, current, current + prev)
  }

  if n <= 1 then 0
  else if n == 2 then 1
  else fibRecursive(2, 0, 1)
}


// Exercise 4

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  def isSortedRecursive(i: Int): Boolean = {
    if i >= as.length then true
    else ordered(as(i - 1), as(i)) && isSortedRecursive(i + 1)
  }

  if as.length < 2 then true
  else isSortedRecursive(1)
}

// Exercise 5

def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
  def g(a: A): B => C = {
    def h(b: B): C = {
      f(a,b)
    }
    h
  }
  g
}

def curry2[A, B, C](f: (A, B) => C): A => (B => C) = {
  val g = (a: A) => {
    val h = (b: B) => f(a,b)
    h
  }
  g
}

def curry3[A, B, C](f: (A, B) => C): A => (B => C) = {
  (a: A) => (b: B) => f(a,b)
}

def curry4[A, B, C](f: (A, B) => C): A => (B => C) = {
  a => b => f(a,b)
}

def isSortedCurried[A]: Array[A] => ((A, A) => Boolean) => Boolean =
  curry(isSorted)

// Exercise 6

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a, b) => f(a)(b)

def isSortedCurriedUncurried[A]: (Array[A], (A, A) => Boolean) => Boolean =
  uncurry(isSortedCurried)

// Exercise 7

def compose[A, B, C](f: B => C, g: A => B): A => C =
  a => f(g(a))
