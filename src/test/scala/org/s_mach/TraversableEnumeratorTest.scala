/*
    Copyright 2012 Georgia Tech Research Institute

    Author: lance.gatlin@gmail.com

    This file is part of org.s_mach library.

    org.s_mach library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    org.s_mach library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with org.s_mach library. If not, see <http://www.gnu.org/licenses/>.

*/
package org.s_mach

import org.scalatest.FunSpec
import org.s_mach._
import org.s_mach.Enumerator.STD_CHUNK_SIZE
import scala.util.Random
import test._
import scala.collection.immutable.Seq

class TraversableEnumeratorTest extends FunSpec {
  val rnd = Stream.continually(Random.nextInt(100))

  describe("A TraversableEnumerator[O]") {
    it("should be be constructable from any traversable") {
      val e : Enumerator[Int] = rnd.toEnumerator
    }
    it("should end with the Success state once input is exhausted") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val e : Enumerator[Int] = l.toEnumerator
      val state0 : Enumerator.State[Int] = e.s0
      val result = state0.run()
      val isSuccess =
        result.state match {
          case q : Enumerator.State.Continuation[Int] => false
          case q : Enumerator.State.Success[Int] => true
          case q : Enumerator.State.Halted[Int] => false
        }
      // Just demo'ing a second way of doing this
      val isSuccess2 =
        result.state.fold(
          ifContinuation = { q => false },
          ifSuccess = { q => true },
          ifHalted = { q => false }
        )
      assert(isSuccess == true && isSuccess2 == true)
    }
    it("should return Success state when endOfInput is applied to a continue state") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val e : Enumerator[Int] = l.toEnumerator
      val state0 = e.s0
      val result = utility.forceDoneTransition(Enumerator.Transition(state0))
      val isSuccess =
        result.state.fold(
          ifContinuation = { q => false },
          ifSuccess = { q => true },
          ifHalted = { q => false }
        )
      assert(isSuccess == true)
    }
    it("should return Progress metadata if traversable has definite size") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val e : Enumerator[Int] = l.toEnumerator
      val result = e.run()
      assert(result.metadata == Seq(Enumerator.Progress(0,n),Enumerator.Progress(STD_CHUNK_SIZE,n),Enumerator.Progress(n,n)))
    }
  }
}
