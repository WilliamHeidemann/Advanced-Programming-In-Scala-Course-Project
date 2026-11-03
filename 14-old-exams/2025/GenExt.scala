// (c) 2025 Andrzej Wasowski
// An extension of scalacheck Gen's to support LazyList sequencing
// Added to support some solutions for exam 2025.

package org.scalacheck

import Gen.gen, Gen.Parameters
import org.scalacheck.rng.Seed

import adpro.lazyList.*
import LazyList.{empty, cons}

object GenExt:

  def map2[A, B, C](ga: Gen[A], gb: => Gen[B])(f: (A, => B) => C): Gen[C] =
    gen { (p: Parameters, seed: Seed) =>
      val seed2 = seed.slide
      val a = ga.pureApply(p, seed)
      lazy val b = gb.pureApply(p, seed2)
      Gen.r(Some(f(a, b)), seed2)
    }

  extension [A](lga: LazyList[Gen[A]])
    def sequence: Gen[LazyList[A]] =
      lga.foldRight (Gen.const(empty[A])) { (h, t) => map2 (h, t) (cons) }

end GenExt
