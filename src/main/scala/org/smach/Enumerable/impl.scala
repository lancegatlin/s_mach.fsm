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
package org.smach.Enumerable

import org.smach._
import org.smach.utility
import scala.collection.immutable.Seq

object impl {
  /**
   * Step an Enumerable.Transition (apply an instance of unit to the state)
   * @param t0
   * @tparam O
   * @tparam A
   * @return
   */
  def stepEnumerableTransition[O,A](t0: Transition[O,A]) : Transition[O,A] = {
    t0.fold(
      ifContinue = t0 => utility.accumulateTransitions(t0,t0.state(())),
      ifSucceed = _ => t0,
      ifHalt = _ => t0
    )
  }

  /**
   * Step an Enumerable.State (apply an instance of unit to the state)
   * @param s
   * @tparam O
   * @tparam A
   * @return
   */
  def stepEnumerableState[O,A](s: State[O,A]) : Transition[O,A] = {
    s.fold[Transition[O,A]](
      ifContinuation = q => q(()),
      ifSuccess = q => new Succeed(state=q,output=Seq.empty,overflow=Seq.empty,metadata=Seq.empty),
      ifHalted = q => new Halt(state=q,output=Seq.empty,overflow=Seq.empty,metadata=Seq.empty)
    )
  }

  /**
   * Step an Enumerable (apply an instance of unit to the s0 state)
   * @param m
   * @tparam O
   * @tparam A
   * @return
   */
  def stepEnumerable[O,A](m : Enumerable[O,A]) : Transition[O,A] = stepEnumerableState(m.s0)

  /**
   * Step an Enumerable.Transition until it returns Success/Failure with the option to recover from all recoverable Failures
   * @param t0
   * @param _haltedRecoveryStrategy
   * @tparam O
   * @tparam A
   * @return
   */
  def runEnumerableTransition[O,A](t0 : Transition[O,A], _haltedRecoveryStrategy: HaltedRecoveryStrategy[Unit,O,A]) : (Transition[O,A], HaltedRecoveryStrategy[Unit,O,A]) = {
    var done = false
    val accumulator = utility.TransitionAccumulator(t0)
    var haltedRecoveryStrategy = _haltedRecoveryStrategy
    do {
      accumulator.state.fold(
        ifContinuation = { q =>
          // Accumulate transition from applying unit to continuation
          // TODO: handle possibility of infinite recursion here somehow -- look for Progress?
          accumulator.accumulate(q(()))
        },
        ifSuccess = { q =>
          done = true
        },
        ifHalted = { q =>
          val (recoveredTransition,newHaltedRecoveryStrategy) = haltedRecoveryStrategy.recoverAll(q)
          haltedRecoveryStrategy = newHaltedRecoveryStrategy
          accumulator.accumulate(recoveredTransition)
          if(accumulator.state.isContinuation == false) {
            done = true
          }

        }
      )
    } while(done == false)
    (accumulator.toTransition, haltedRecoveryStrategy)
  }

  /**
   * Step an Enumerable.State until it returns Success/Failure with the option to recover from all recoverable Failures
   * @param s
   * @param haltedRecoveryStrategy
   * @tparam O
   * @tparam A
   * @return
   */
  def runEnumerableState[O,A](s: State[O,A], haltedRecoveryStrategy: HaltedRecoveryStrategy[Unit,O,A]) : (Transition[O,A],HaltedRecoveryStrategy[Unit,O,A]) = {
    val t0 = s.fold(
      ifSuccess = q => new Succeed(state=q,output=Seq.empty,overflow=Seq.empty,metadata=Seq.empty),
      ifHalted = q => new Halt(state=q,output=Seq.empty,overflow=Seq.empty,metadata=Seq.empty),
      ifContinuation = q => new Continue(state=q,output=Seq.empty,metadata=Seq.empty)
    )
    runEnumerableTransition(t0, haltedRecoveryStrategy)
  }

  /**
   * Step an Enumerable until it returns Success/Failure with the option to recover from all recoverable Failures
   * @param m
   * @param haltedRecoveryStrategy
   * @tparam O
   * @tparam A
   * @return
   */
  def runEnumerable[O,A](m: Enumerable[O,A], haltedRecoveryStrategy: HaltedRecoveryStrategy[Unit,O,A]) : (Transition[O,A],HaltedRecoveryStrategy[Unit,O,A]) = runEnumerableState(m.s0, haltedRecoveryStrategy)

  /**
   * Map an Enumerable Transition in terms of bind/flatMap
   * @param s
   * @param f
   * @tparam O
   * @tparam A
   * @tparam OO
   * @tparam B
   * @return
   */
  def mapEnumerableTransition[O,A,II,OO,B](s : Transition[O,A], f: A => B) : StateMachine.Transition[II,OO,B] = {
    flatMapEnumerableTransition[O,A,II,OO,B](s, { (a : A) => StateMachine.Succeed(f(a)) } )
  }

  /**
   * Flat map an Enumerable DoneTransition such that if the Transition is Success the value is re-packaged with metadata
   * and returned. If it is Halted, a new Halted Transition is returned that recursively flatMaps the recover result
   * (should one exist).
   * @param t0
   * @param f
   * @tparam O
   * @tparam A
   * @tparam OO
   * @tparam B
   * @return
   */
  def flatMapEnumerableTransition[O,A,II,OO,B](t0 : Transition[O,A], f: A => StateMachine.Transition[II,OO,B]) : StateMachine.Transition[II,OO,B] = {
    t0.fold(
      ifContinue = { t0 =>
        // Run the enumerable to extract the value or get a halted state
        val (t1, _) = Enumerable.impl.runEnumerableTransition(t0, HaltedRecoveryStrategy.STRICT[Unit,O,A])
        // Run enumerable only returns Success/Halted but because it returns Transition this isn't captured by type system - so there can't be infinite recursion here
        flatMapEnumerableTransition(t1, f)
      },
      ifSucceed = { t0 =>
        val t1 = f(t0.state.value)
        t1.fold(
          ifSucceed = t1 => t1.copy(metadata = t1.metadata ++ t0.metadata),
          ifHalt = t1 => t1.copy(metadata = t1.metadata ++ t0.metadata),
          ifContinue = t1 => t1.copy(metadata = t1.metadata ++ t0.metadata)
        )
      },
      ifHalt = { t0 =>
        val optRecover : Option[() => StateMachine.Transition[II,OO,B]] = t0.state.optRecover map { recover => { () =>
          flatMapEnumerableTransition(recover(), f)
        }}
        StateMachine.Halt(
          issues = t0.state.issues,
          optRecover = optRecover,
          metadata = t0.metadata
        )
      }
    )
  }

  /**
   * Map an Enumerable DoneTransition in terms of bind/flatMap
   * @param s
   * @param f
   * @tparam O
   * @tparam A
   * @tparam OO
   * @tparam B
   * @return
   */
  def mapEnumerableDoneTransition[O,A,II,OO,B](s : DoneTransition[O,A], f: A => B) : StateMachine.DoneTransition[II,OO,B] = {
    flatMapEnumerableDoneTransition[O,A,II,OO,B](s, { (a : A) => StateMachine.Succeed(f(a)) } )
  }

/**
   * Flat map an Enumerable Transition such that if the Transition is a continuation it is run until it is Success/Halt,
   * If it is Success the value is re-packaged with metadata and returned. If it is Halted, a new Halted Transition is
   * returned that recursively flatMaps the recover result (should one exist).
   * @param t0
   * @param f
   * @tparam O
   * @tparam A
   * @tparam OO
   * @tparam B
   * @return
   */
  def flatMapEnumerableDoneTransition[O,A,II,OO,B](t0 : DoneTransition[O,A], f: A => StateMachine.DoneTransition[II,OO,B]) : StateMachine.DoneTransition[II,OO,B] = {
    t0.doneFold(
      ifSucceed = { t0 =>
        val t1 = f(t0.state.value)
        t1.doneFold(
          ifSucceed = t1 => t1.copy(metadata = t1.metadata ++ t0.metadata),
          ifHalt = t1 => t1.copy(metadata = t1.metadata ++ t0.metadata)
        )
      },
      ifHalt = { t0 =>
        val optRecover : Option[() => StateMachine.Transition[II,OO,B]] = t0.state.optRecover map { recover => { () =>
          val t1 = utility.forceDoneTransition(recover())
          flatMapEnumerableDoneTransition(t1, f)
        }}
        StateMachine.Halt(
          issues = t0.state.issues,
          optRecover = optRecover,
          metadata = t0.metadata
        )
      }
    )
  }


  /**
   * Invert a collection of Transitions into a Transition of a collection.
   * @param xs
   * @param xt
   * @tparam O
   * @tparam A
   * @return
   */
  def sequenceEnumerableTransitionTraversable[O,A](xs: Traversable[Transition[O,A]], xt: List[A] = Nil) : Transition[O,Seq[A]] = {
    // TODO: optimize me?
    if(xs.isEmpty) {
      Succeed(xt.reverse)
    } else {
      flatMapEnumerableTransition[O,A,Unit,O,Seq[A]](xs.head, { x => sequenceEnumerableTransitionTraversable(xs.tail, x :: xt) })
    }
  }

  /**
   * Invert a collection of Transitions into a Transition of a collection.
   * @param xs
   * @param xt
   * @tparam O
   * @tparam A
   * @return
   */
  def sequenceEnumerableDoneTransitionTraversable[O,A](xs: Traversable[DoneTransition[O,A]], xt: List[A] = Nil) : DoneTransition[O,Seq[A]] = {
    // TODO: optimize me?
    if(xs.isEmpty) {
      Succeed(xt.reverse)
    } else {
      flatMapEnumerableDoneTransition[O,A,Unit,O,Seq[A]](xs.head, { x => sequenceEnumerableDoneTransitionTraversable(xs.tail, x :: xt) })
    }
  }

}
