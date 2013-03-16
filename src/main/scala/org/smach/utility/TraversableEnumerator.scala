/*
    Copyright 2013 Lance Gatlin

    Author: lance.gatlin@gmail.com

    This file is part of org.smach library.

    org.smach library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    org.smach library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with org.smach library. If not, see <http://www.gnu.org/licenses/>.

*/
package org.smach.utility

import org.smach._
import scala.collection.immutable.Seq

case class TraversableEnumerator[A](
  t         :     Traversable[A],
  chunkSize :     Int               =   Enumerator.STD_CHUNK_SIZE
) extends Enumerator[A] {
  import Enumerator._
  require(chunkSize > 0)

  case class ContWithoutProgress(current : Traversable[A]) extends State.Continuation[A] {

    def apply(x : Unit) : Transition[A] = {
      val (nextChunk, remaining) = current.splitAt(chunkSize)
      if(remaining.isEmpty) {
        Succeed(nextChunk.toVector)
      } else {
        Continue(ContWithoutProgress(remaining), nextChunk.toVector)
      }
    }
    def apply(x : EndOfInput) = Succeed()
  }

  case class ContWithProgress(maxN : Int, current : Traversable[A]) extends State.Continuation[A] {

    def currentProgress = Progress(maxN - current.size, maxN)

    def apply(x : Unit) : Transition[A] = {
      val (nextChunk, remaining) = current.splitAt(chunkSize)
      if(remaining.isEmpty) {
        Succeed(
          output = nextChunk.toVector,
          metadata = Seq(currentProgress, Progress(maxN, maxN))
        )
      } else {
        val output = nextChunk.toVector
        Continue(
          state = ContWithProgress(maxN,remaining),
          output = output,
          metadata = Seq(currentProgress)
        )
      }
    }
    def apply(x : EndOfInput) = Succeed(metadata = Seq(currentProgress))
  }

  def s0 = {
    if(t.hasDefiniteSize) {
      ContWithProgress(t.size,t)
    } else {
      ContWithoutProgress(t)
    }
  }
}