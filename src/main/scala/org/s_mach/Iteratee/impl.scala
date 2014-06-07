/*
    Copyright 2013 Lance Gatlin

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
package org.s_mach.Iteratee

import org.s_mach._
import org.s_mach.Iteratee._
import org.s_mach.utility
import scala.collection.immutable.Seq
import org.s_mach.StateMachine.State.{Halted, Success, Continuation}

object impl {

  /**
   * Implementation of Iteratee.State.map
   * @param s
   * @param f
   * @tparam I
   * @tparam A
   * @tparam B
   * @return
   */
  def mapIterateeState[I,A,B](s : State[I,A], f: A => B) : State[I,B] = {
    flatMapIterateeState(s, { (a : A) => bindIterateeState(f(a)) } )
  }

  /**
   * Implementation of Iteratee.State.bind
   * @param value
   * @tparam I
   * @tparam A
   * @return
   */
  def bindIterateeState[I,A](value : A) : State[I,A] = State.Success(value)

  private[impl] def flatMapIterateeTransition[I,A,B](t0 : Transition[I,A], f: A => State[I,B]) : Transition[I,B] = {
    t0.fold(
      ifContinue = { t0 =>
        Transition(
          state = FlatMapIterateeContinuation(t0.state,f),
          metadata = t0.metadata
        )
      },
      ifSucceed = { t0 =>
        val t1 = utility.applySeqToState(t0.overflow,f(t0.state.value))
        t1.fold(
          ifSucceed = t1 => t1.copy(metadata = t1.metadata ++ t0.metadata),
          ifHalt = t1 => t1.copy(metadata = t1.metadata ++ t0.metadata),
          ifContinue = t1 => t1.copy(metadata = t1.metadata ++ t0.metadata)
        )
      },
      ifHalt = { t0 =>
        val optRecover : Option[() => Transition[I,B]] = t0.state.optRecover map { recover => () => flatMapIterateeTransition[I,A,B](recover(), f) }
        new Halt(
          state = State.Halted(
            issues = t0.state.issues,
            optRecover = optRecover
          ),
          output = t0.output,
          overflow = t0.overflow,
          metadata = t0.metadata
        )
      }
    )
  }

  // TODO: DRY me
  private[impl] def flatMapIterateeDoneTransition[I,A,B](t0 : DoneTransition[I,A], f: A => State[I,B]) : DoneTransition[I,B] = {
    t0.doneFold[DoneTransition[I,B]](
      ifSucceed = { t0 =>
        val t1 = utility.forceDoneTransition(utility.applySeqToState(t0.overflow,f(t0.state.value)))
        t1.doneFold(
          ifSucceed = t1 => t1.copy(metadata = t1.metadata ++ t0.metadata),
          ifHalt = t1 => t1.copy(metadata = t1.metadata ++ t0.metadata)
        )
      },
      ifHalt = { t0 =>
        val optRecover : Option[() => Transition[I,B]] = t0.state.optRecover map { recover => () => flatMapIterateeTransition[I,A,B](recover(), f) }
        new Halt(
          state = State.Halted(
            issues = t0.state.issues,
            optRecover = optRecover
          ),
          output = t0.output,
          overflow = t0.overflow,
          metadata = t0.metadata
        )
      }
    )
  }

  //This case class used instead of anonymous class for better debug messages
  private[impl] case class FlatMapIterateeContinuation[I,A,B](s : State.Continuation[I,A], f: A => State[I,B]) extends State.Continuation[I,B] {
    override def apply(xs: Seq[I]) = flatMapIterateeTransition(s.apply(xs),f)
  
    def apply(x: I) = flatMapIterateeTransition(s.apply(x),f)
  
    def apply(x: EndOfInput) = flatMapIterateeDoneTransition(s.apply(x),f)
  }

  /**
   * Implementation of Iteratee.flatMap that allows extracting the final value, connects the overflow of the outer
   * Iteratee to the inner Iteratee, accumulates metadata and composes Halted states.
   * @param s
   * @param f
   * @tparam I
   * @tparam A
   * @tparam B
   * @return
   */
  def flatMapIterateeState[I,A,B](s : State[I,A], f: A => State[I,B]) : State[I,B] = {
    s.fold(
      ifContinuation = { q =>
        FlatMapIterateeContinuation(q,f)
      },
      ifSuccess = { q =>
        // Note: if s is initially Success, there is no way to connect the overflow from the final Transition to the inner Iteratee
        // Note: connecting overflow is correctly handled by FlatMapIterateeContinuation
        f(q.value)
      },
      ifHalted = { q =>
        val optRecover : Option[() => Transition[I,B]] = q.optRecover map { recover => () => flatMapIterateeTransition[I,A,B](recover(),f) }
        State.Halted(
          issues = q.issues,
          optRecover = optRecover
        )
      }
    )
  }

  
  /**
   * Implementation of Iteratee.map in terms of flatMap/bind
   * @param m
   * @param f
   * @tparam I
   * @tparam A
   * @tparam B
   * @return
   */
  def mapIteratee[I,A,B](m : Iteratee[I,A], f: A => B) : StateMachine[I,Unit,B] = {
    flatMapIteratee(m, { (a: A) => bindIteratee(f(a)) })
  }

  /**
   * Implementation of Iteratee.bind
   * @param value
   * @tparam I
   * @tparam A
   * @return
   */
  def bindIteratee[I,A](value : A) = new Iteratee[I,A] {
    def s0 = bindIterateeState(value)
  }

  //This case class used instead of anonymous class for better debug messages
  private[impl] case class FlatMapIteratee[I,A,B](m : Iteratee[I,A], f: A => Iteratee[I,B]) extends Iteratee[I,B] {
    def s0 = flatMapIterateeState(m.s0, { (a : A) => f(a).s0 })
  }

  /**
   * Implementation of Iteratee.flatMap
   * @param m
   * @param f
   * @tparam I
   * @tparam A
   * @tparam B
   * @return
   */
  def flatMapIteratee[I,A,B](m : Iteratee[I,A], f: A => Iteratee[I,B]) : Iteratee[I,B] = FlatMapIteratee(m,f)

  private[impl] case class PeekIterateeState[A]() extends State.Continuation[A,A] {
    def apply(x : A) = {
      Succeed(
        value = x,
        overflow = x :: Nil
      )
    }
    def apply(x : EOI) = Halt.fatal("No input")
  }

  private[impl] case class PeekIteratee[A]() extends Iteratee[A,A] {
    def s0 = PeekIterateeState()
  }

  def peek[A] = PeekIteratee[A]()
}
