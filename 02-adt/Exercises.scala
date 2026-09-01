// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.adt

import java.util.NoSuchElementException

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])


object List: 

  def head[A] (l: List[A]): A = l match
    case Nil => throw NoSuchElementException() 
    case Cons(h, _) => h                                                                                                                                                                                                                                       
  
  def apply[A] (as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def append[A] (l1: List[A], l2: List[A]): List[A] =
    l1 match
      case Nil => l2
      case Cons(h, t) => Cons(h, append(t, l2)) 

  def foldRight[A, B] (l: List[A], z: B, f: (A, B) => B): B = l match
    case Nil => z
    case Cons(a, as) => f(a, foldRight(as, z, f))
    
  def map[A, B] (l: List[A], f: A => B): List[B] =
    foldRight[A, List[B]] (l, Nil, (a, z) => Cons(f(a), z))

  // Exercise 1 (is to be solved without programming)

  // Answer: 1. The list will match with head and a tail and the expression evaluates to the head.

  // Exercise 2

  def tail[A] (l: List[A]): List[A] = l match
    case Nil => throw NoSuchElementException()
    case Cons(head, tail) => tail

  // Exercise 3
  
  def drop[A] (l: List[A], n: Int): List[A] =
    if n <= 0 then l
    else l match
      case Nil => throw NoSuchElementException()
      case Cons(head, tail) => drop(tail, n-1)

  // Exercise 4

  def dropWhile[A] (l: List[A], p: A => Boolean): List[A] = l match
    case Nil => Nil
    case Cons(head, tail) => if p(head) then dropWhile(tail, p) else l

  // Exercise 5
  def init[A] (l: List[A]): List[A] = l match
    case Nil => throw NoSuchElementException()
    case Cons(head, Nil) => Nil
    case Cons(head, Cons(head2, Nil)) => Cons(head, Nil)
    case Cons(head, tail) => Cons(head, init(tail))

  // Exercise 6

  def length[A] (l: List[A]): Int =
    foldRight[A, Int] (l, 0, (element, acc) => acc + 1)

  // Exercise 7

  @annotation.tailrec
  def foldLeft[A, B] (l: List[A], z: B, f: (B, A) => B): B = l match
    case Nil => z
    case Cons(head, tail) => foldLeft[A, B] (tail, f(z, head), f)

  // Exercise 8

  def product (as: List[Int]): Int =
    foldLeft[Int, Int] (as, 1, (acc, element) => acc * element)

  def length1[A] (as: List[A]): Int =
    foldLeft[A, Int] (as, 0, (acc, element) => acc + 1)

  // Exercise 9

  def reverse[A] (l: List[A]): List[A] =
    foldLeft[A, List[A]] (l, Nil, (acc, element) => Cons(element, acc))
 
  // Exercise 10

  def foldRight1[A, B] (l: List[A], z: B, f: (A, B) => B): B =
    foldLeft[A, B] (reverse(l), z, (acc, element) => f(element, acc))

  // Exercise 11

  def foldLeft1[A, B] (l: List[A], z: B, f: (B, A) => B): B = ???
    // foldRight[A, B => B]
 
  // Exercise 12

  def concat[A] (l: List[List[A]]): List[A] =
    foldLeft[List[A], List[A]] (l, Nil, (acc, element) => append(acc, element))

  // Exercise 13

  def filter[A] (l: List[A], p: A => Boolean): List[A] =
    foldRight[A, List[A]] (l, Nil, (element, acc) => if p(element) then Cons(element, acc) else acc)

  // Exercise 14

  def flatMap[A,B] (l: List[A], f: A => List[B]): List[B] =
    foldLeft[A, List[B]] (l, Nil, (acc, element) => append(acc, f(element)))

  // Exercise 15

  def filter1[A] (l: List[A], p: A => Boolean): List[A] =
    flatMap[A, A] (l, element => if p(element) then Cons(element, Nil) else Nil)

  // Exercise 16

  def addPairwise (l: List[Int], r: List[Int]): List[Int] = l match
    case Nil => Nil
    case Cons(lHead, lTail) => r match
      case Nil => Nil
      case Cons(rHead, rTail) => Cons(lHead + rHead, addPairwise(lTail, rTail))

  // Exercise 17

  def zipWith[A, B, C] (l: List[A], r: List[B], f: (A,B) => C): List[C] = l match
    case Nil => Nil
    case Cons(lHead, lTail) => r match
      case Nil => Nil
      case Cons(rHead, rTail) => Cons(f(lHead, rHead), zipWith(lTail, rTail, f))
  // Exercise 18

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = ???
