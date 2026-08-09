// Advanced Programming, A. Wąsowski, IT University of Copenhagen

package adpro

import scala.annotation.targetName

// A short text explaining how to write typical imperative computations in a
// pure functional manner for novice functional programmers.  Skip if you had a
// course on that before.

def factorial_imperative(n: Int): Int =
  var result = 1
  var x = n
  while x > 1 do
    result = result * x
    x = x - 1
  return result

// We extract the state we operate on: x, result and turn it into
// parameters, which are additional to the one we already head

// The return type stays the same

def doWork(n: Int) (result: Int) (x: Int): Int = ???

// Turn the loop into an if-condition that will decide to
// loop or recurse

object doWork1: // I put things in objects so that conflicting defs compile
  def doWork(n: Int) (result: Int) (x: Int): Int =
    if x > 1
    then ??? /* translate loopbody here */
    else ??? /* translate the rest here */

// Translate the loop body
object doWork2:
  def doWork(n: Int)(result: Int)(x: Int): Int =
    if x > 1
    then doWork (n) (result * x) (x - 1)
    else result

// Let us to hide the internal (technical) arguments
// and implement the initialization from the original function
object factorial1:
  def factorial(n: Int): Int =
    def doWork(n: Int)(result: Int)(x: Int): Int =
      if x > 1
      then doWork (n) (result*x) (x-1)
      else result
    doWork (n) (1) (n)

//  n is invariant so we can eliminate it from doWork's explicit
//  interface
object factorial2:
  def factorial(n: Int): Int =
    def doWork (result: Int) (x: Int): Int =
      if x > 1
      then doWork (result*x) (x-1)
      else result
    doWork (1) (n)
