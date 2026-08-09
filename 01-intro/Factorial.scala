// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.intro

object Imperative:
  def factorial(n: Int): Int =
    var result = 1
    for i <- 2 to n do
      result *= i
    result

object Functional:
  def factorial(n: Int): Int =
    if n <= 1 then 1
    else n * factorial(n - 1)

object TailRecursive:
  def factorial(n: Int) =
    def f(n: Int, r: Int): Int =
      if n <= 1 then r
      else f(n - 1, n * r)
    f(n, 1)

@main def main =
  println(f"Imperative: ${Imperative.factorial(5)}")
  println(f"Applicative: ${Functional.factorial(5)}")
  println(f"Tail recursive: ${TailRecursive.factorial(5)}")
