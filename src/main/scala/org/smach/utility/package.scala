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
package org.smach

import org.smach.StateMachine._
//import annotation.tailrec
import collection.mutable
import IssueSeverityCode._
import scala.collection.immutable.Seq

package object utility {

  /**
   * Exhaustively recover all recoverable halted states
   * @param s
   * @param shouldRecover TRUE if the halted state should be recovered
   * @param maxAttempts the maximum number of times a halted state should be recovered
   * @tparam I
   * @tparam O
   * @tparam A
   * @return
   */
  def recoverAll[I,O,A](s: State.Halted[I,O,A], shouldRecover: State.Halted[I,O,A] => Boolean, maxAttempts : Int) : Transition[I,O,A] = {
    var done = false
    val r = TransitionAccumulator(s)
    var n = maxAttempts
    do {
      r.state.fold(
        ifContinuation = { q => done = true },
        ifSuccess = { q => done = true },
        ifHalted = { q =>
          if(q.optRecover.isDefined && shouldRecover(q)) {
            if(n > 0) {
              // Save any overflow
              val overflow = r.overflow
              // Add Halted issues as metadata
              r.metadata ++= q.issues
              // Clear overflow -- Not needed since accumulate below will replace it
              // t0.overflow = Nil
              // Recover and accumulate
              r.accumulate(q.optRecover.get.apply())
              // Apply overflow to recovered state and accumulate
              r.accumulate(applySeqToState(overflow, r.state))
              // Decrement maxAttempts
              n = n - 1
            } else {
              // Reached max recover attempts
              r.state = State.Halted(
                issues = Issue.fatal("Max recover attempts reached") :: Nil
              )
              // Dump output and overflow
              r.output.clear()
              r.overflow = Nil
              // Return accumulated metadata
              done = true
            }
          } else {
            done = true
          }
        }
      )
    } while(done == false)
    r.toTransition
  }

  /**
   * Force a Transition to be either Succeed or Halt by applying EndOfInput when required and
   * accumulate the Transitions.
   * @param t0
   * @tparam I
   * @tparam O
   * @tparam A
   * @return
   */
  def forceDoneTransition[I,O,A](t0: Transition[I,O,A]) : DoneTransition[I,O,A] = {
    t0.fold[DoneTransition[I,O,A]](
      ifSucceed = t0 => t0,
      ifHalt = t0 => t0,
      ifContinue = t0 => {
        val t1 : DoneTransition[I,O,A] = accumulateTransitions(t0, t0.state.apply(EndOfInput))
        t1.doneFold[DoneTransition[I,O,A]](
          ifSucceed = t1 => t1,
          ifHalt = t1 => t1
//          ,
//          ifContinue = t1 => throw new IllegalStateException
        )
      }
    )
  }

  /**
   * Force a State to be either Success or Halted by applying EndOfInput when required and
   * return the Transitions.
   * @param s0
   * @tparam I
   * @tparam O
   * @tparam A
   * @return
   */
  def forceDoneState[I,O,A](s0: State[I,O,A]) : DoneTransition[I,O,A] = {
    s0.fold[DoneTransition[I,O,A]](
      ifSuccess = q => Succeed(state=q, output=Seq.empty, overflow=Seq.empty, metadata=Seq.empty),
      ifHalted = q => Halt(state=q, output=Seq.empty, overflow=Seq.empty, metadata=Seq.empty),
      ifContinuation = q => {
        val t1 = q.apply(EndOfInput)
        t1.doneFold[DoneTransition[I,O,A]](
          ifSucceed = t1 => t1,
          ifHalt = t1 => t1
//          ,
//          ifContinue = t1 => throw new IllegalStateException
        )
      }
    )
  }

  /**
   * Accumulate two Transitions such that second.state and second.overflow replace first.state and first.overflow AND
   * second.output/metadata are appended to first.output/metadata
    * @param t0
   * @param t1
   * @tparam I
   * @tparam O
   * @tparam A
   * @return
   */
  def accumulateTransitions[I,O,A](t0 : Transition[I,O,A], t1 : Transition[I,O,A]) : Transition[I,O,A] = {
    t0.fold[Transition[I,O,A]](
      ifSucceed = t0 =>
        t1.fold[Transition[I,O,A]](
          ifSucceed = t1 => Succeed(state=t1.state, output=t1.output ++ t0.output, overflow=t1.overflow, metadata=t1.metadata ++ t0.metadata), 
          ifHalt = t1 => Halt(state=t1.state, output=t1.output ++ t0.output, overflow=t1.overflow, metadata=t1.metadata ++ t0.metadata),
          ifContinue = t1 => Continue(state=t1.state, output=t1.output ++ t0.output, metadata=t1.metadata ++ t0.metadata)
        ),
      ifHalt = t0 =>
        t1.fold[Transition[I,O,A]](
          ifSucceed = t1 => Succeed(state=t1.state, output=t1.output ++ t0.output, overflow=t1.overflow, metadata=t1.metadata ++ t0.metadata),
          ifHalt = t1 => Halt(state=t1.state, output=t1.output ++ t0.output, overflow=t1.overflow, metadata=t1.metadata ++ t0.metadata),
          ifContinue = t1 => Continue(state=t1.state, output=t1.output ++ t0.output, metadata=t1.metadata ++ t0.metadata)
        ),
      ifContinue = t0 =>
        t1.fold[Transition[I,O,A]](
          ifSucceed = t1 => Succeed(state=t1.state, output=t1.output ++ t0.output, overflow=t1.overflow, metadata=t1.metadata ++ t0.metadata),
          ifHalt = t1 => Halt(state=t1.state, output=t1.output ++ t0.output, overflow=t1.overflow, metadata=t1.metadata ++ t0.metadata),
          ifContinue = t1 => Continue(state=t1.state, output=t1.output ++ t0.output, metadata=t1.metadata ++ t0.metadata)
        )
    )
  }

  // TODO: DRY me
  def accumulateTransitions[I,O,A](t0 : Transition[I,O,A], t1 : DoneTransition[I,O,A]) : DoneTransition[I,O,A] = {
    t0.fold[DoneTransition[I,O,A]](
      ifSucceed = t0 =>
        t1.doneFold[DoneTransition[I,O,A]](
          ifSucceed = t1 => Succeed(state=t1.state, output=t1.output ++ t0.output, overflow=t1.overflow, metadata=t1.metadata ++ t0.metadata),
          ifHalt = t1 => Halt(state=t1.state, output=t1.output ++ t0.output, overflow=t1.overflow, metadata=t1.metadata ++ t0.metadata)
        ),
      ifHalt = t0 =>
        t1.doneFold[DoneTransition[I,O,A]](
          ifSucceed = t1 => Succeed(state=t1.state, output=t1.output ++ t0.output, overflow=t1.overflow, metadata=t1.metadata ++ t0.metadata),
          ifHalt = t1 => Halt(state=t1.state, output=t1.output ++ t0.output, overflow=t1.overflow, metadata=t1.metadata ++ t0.metadata)
        ),
      ifContinue = t0 =>
        t1.doneFold[DoneTransition[I,O,A]](
          ifSucceed = t1 => Succeed(state=t1.state, output=t1.output ++ t0.output, overflow=t1.overflow, metadata=t1.metadata ++ t0.metadata),
          ifHalt = t1 => Halt(state=t1.state, output=t1.output ++ t0.output, overflow=t1.overflow, metadata=t1.metadata ++ t0.metadata)
        )
    )
  }


  /**
   * A case class for an accumulator of Transitions.
   * @param state
   * @param output
   * @param overflow
   * @param metadata
   * @tparam I
   * @tparam O
   * @tparam A
   */
  case class TransitionAccumulator[I,O,A](
    var state : State[I,O,A],
    output : mutable.Buffer[O] = mutable.Buffer.empty[O],
    var overflow : Seq[I] = Seq.empty[I],
    metadata : mutable.Buffer[Any] = mutable.Buffer.empty[Any]
  ) {
    def accumulate(t0: Transition[I,O,A]) = {
      t0.fold[Unit](
        ifSucceed = t0 => {
          state = t0.state
          output ++= t0.output
          overflow = t0.overflow
          metadata ++= t0.metadata          
        },
        ifHalt = t0 => {
          state = t0.state
          output ++= t0.output
          overflow = t0.overflow
          metadata ++= t0.metadata          
        },
        ifContinue = t0 => {
          state = t0.state
          output ++= t0.output
          metadata ++= t0.metadata
        }
      )
    }
    def toTransition : Transition[I,O,A] = {
      state.fold[Transition[I,O,A]](
        ifSuccess = q => Succeed(state=q,output=output.toVector,overflow=overflow,metadata=metadata.toVector),
        ifHalted = q => Halt(state=q,output=output.toVector,overflow=overflow,metadata=metadata.toVector),
        ifContinuation = q => Continue(state=q,output=output.toVector,metadata=metadata.toVector)
      )
    }
  }

  object TransitionAccumulator {
    def apply[I,O,A](t0: Transition[I,O,A]) : TransitionAccumulator[I,O,A] = {
      t0.fold[TransitionAccumulator[I,O,A]](
        ifSucceed = t0 => TransitionAccumulator(t0.state, t0.output.toBuffer, t0.overflow, t0.metadata.toBuffer),
        ifHalt = t0 => TransitionAccumulator(t0.state, t0.output.toBuffer, t0.overflow, t0.metadata.toBuffer),
        ifContinue = t0 => TransitionAccumulator(t0.state, t0.output.toBuffer, Seq.empty, t0.metadata.toBuffer)
      )
    }
  }

  def applyInputToState[I,O,A](input: Input[I], s: State[I,O,A]) : Transition[I,O,A] = {
    input match {
      case chunk : Chunk[I] => applySeqToState(chunk.xs,s)
      case _ : EndOfInput => forceDoneState(s)
    }
  }

  def applySeqToState[I,O,A](xs: Seq[I], s: State[I,O,A]) : Transition[I,O,A] = applySeqToTransition(xs, Transition(s))

  /**
   * Apply xs to a transition with the option to recover from Halted states and accumulate all transitions.
   * @param t
   * @param xs
   * @tparam I
   * @tparam O
   * @tparam A
   * @return
   */
  def applySeqToTransition[I,O,A](xs: Seq[I], t: Transition[I,O,A]) : Transition[I,O,A] = {
    if(xs.nonEmpty) {
      var done = false
      val r = TransitionAccumulator[I,O,A](t)
      var i = xs
      do {
        r.state.fold(
          ifContinuation = { q =>
            // If xs isn't empty
            if(i.nonEmpty) {
              // Apply the xs head to the continuation and accumulate the transition
              r.accumulate(q(i.head))
              i = i.tail
            } else {
              // Input is exhausted so we are done
              done = true
            }
          },
          ifSuccess = { q =>
            // Append remaining xs to overflow
            r.overflow ++= i
            done = true
          },
          ifHalted = { q =>
            // Append remaining xs to overflow
            r.overflow ++= i
            done = true
          }
        )
      } while(done == false)
      r.toTransition
    } else {
      // Empty input return transition unchanged
      t
    }
  }

  private[utility] def composeTransition[A,B,C,D,ZZ](t0: Transition[A,B,ZZ], t1: Transition[B,C,D]) : Transition[A,C,D] = {
    t0.fold[Transition[A,C,D]](
      ifContinue = t0 =>
        t1.fold[Transition[A,C,D]](
          ifContinue = t1 =>
            Continue(state=compose(t0.state,t1.state),output=t1.output,metadata=t1.metadata ++ t0.metadata),
          ifSucceed = t1 =>
            Succeed(state=compose(t0.state,t1.state),output=t1.output,overflow=Seq.empty,metadata=t1.metadata ++ t0.metadata),
          ifHalt = t1 =>
            Halt(state=compose(t0.state,t1.state,t1.overflow),output=t1.output,overflow=Seq.empty,metadata=t1.metadata ++ t0.metadata)
        ),
      ifSucceed = t0 =>
        t1.fold[Transition[A,C,D]](
          ifContinue = t1 =>
            Halt(state=compose(t0.state,t1.state),output=t1.output,overflow=Seq.empty,metadata=t1.metadata ++ t0.metadata),
          ifSucceed = t1 =>
            Succeed(state=compose(t0.state,t1.state),output=t1.output,overflow=t0.overflow,metadata=t1.metadata ++ t0.metadata),
          ifHalt = t1 =>
            Halt(compose(t0.state,t1.state,t1.overflow),output=t1.output,overflow=t0.overflow,metadata=t1.metadata ++ t0.metadata)
        ),
      ifHalt = t0 =>
        t1.fold[Transition[A,C,D]](
          ifContinue = t1 =>
            Halt(compose(t0.state,t1.state),output=t1.output,overflow=t0.overflow,metadata=t1.metadata ++ t0.metadata),
          ifSucceed = t1 =>
            Halt(compose(t0.state,t1.state),output=t1.output,overflow=t0.overflow,metadata=t1.metadata ++ t0.metadata),
          ifHalt = t1 =>
            Halt(compose(t0.state,t1.state,t1.overflow),output=t1.output,overflow=t0.overflow,metadata=t1.metadata ++ t0.metadata)
        )
    )
  }

  // TODO: DRY me
  private[utility] def composeDoneTransition[A,B,C,D,ZZ](t0: DoneTransition[A,B,ZZ], t1: DoneTransition[B,C,D]) : DoneTransition[A,C,D] = {
    t0.doneFold[DoneTransition[A,C,D]](
      ifSucceed = t0 =>
        t1.doneFold[DoneTransition[A,C,D]](
          ifSucceed = t1 =>
            Succeed(state=compose(t0.state,t1.state),output=t1.output,overflow=t0.overflow,metadata=t1.metadata ++ t0.metadata),
          ifHalt = t1 =>
            Halt(compose(t0.state,t1.state,t1.overflow),output=t1.output,overflow=t0.overflow,metadata=t1.metadata ++ t0.metadata)
        ),
      ifHalt = t0 =>
        t1.doneFold[DoneTransition[A,C,D]](
          ifSucceed = t1 =>
            Halt(compose(t0.state,t1.state),output=t1.output,overflow=t0.overflow,metadata=t1.metadata ++ t0.metadata),
          ifHalt = t1 =>
            Halt(compose(t0.state,t1.state,t1.overflow),output=t1.output,overflow=t0.overflow,metadata=t1.metadata ++ t0.metadata)
        )
    )
  }

  private[utility] def composeTransitionAndStateContinue[A,B,C,D,ZZ](t0: Transition[A,B,ZZ], s1: State.Continuation[B,C,D]) : Transition[A,C,D] = {
    t0.fold[Transition[A,C,D]](
      ifSucceed = t0 => {
        val t1 = s1(t0.output)
        // If t0 is Success, feed an EOI to t1 since t1 will not receive further input
        composeTransition(t0, forceDoneTransition(t1))
      },
      ifHalt = t0 => {
        val t1 = s1(t0.output)
        composeTransition(t0,t1)
      },
      ifContinue = t0 => {
        val t1 = s1(t0.output)
        composeTransition(t0,t1)
      }
    )
  }

  // This is a case class instead of anonymous class for better debug messages
  private[utility] case class CompositeStateContinue[A,B,C,D,ZZ](s0: State.Continuation[A,B,ZZ], s1: State.Continuation[B,C,D]) extends State.Continuation[A,C,D] {
      override def apply(xs : Seq[A]) : Transition[A,C,D] = composeTransitionAndStateContinue(s0(xs),s1)

      def apply(x: A) = composeTransitionAndStateContinue(s0(x),s1)

      def apply(x: EndOfInput) = {
        val eoi_t0 : DoneTransition[A,B,ZZ] = s0(x)
        val output = eoi_t0.fold(ifSucceed = t => t.output, ifHalt = t => t.output, ifContinue = t => t.output)
        val t1 : Transition[B,C,D] = s1(output)
        val eoi_t1 : DoneTransition[B,C,D] = forceDoneTransition(t1)
        composeDoneTransition(eoi_t0, eoi_t1)
      }
  }

  private[utility] def compose[A,B,C,D,ZZ](s0 : State.Continuation[A,B,ZZ], s1 : State.Continuation[B,C,D]) : State.Continuation[A,C,D] = CompositeStateContinue(s0, s1)
  private[utility] def compose[A,B,C,D,ZZ](s0 : State.Continuation[A,B,ZZ], s1 : State.Success[B,C,D]) : State.Success[A,C,D] = State.Success(s1.value)
  private[utility] def compose[A,B,C,D,ZZ](s0 : State.Continuation[A,B,ZZ], s1 : State.Halted[B,C,D], overflow: Seq[B]) : State.Halted[A,C,D] = {
    // TODO: test to verify this
    val optRecover : Option[() => Transition[A,C,D]] =
      s1.optRecover map { recover =>
        () => {
          val t0 = Continue(s0)
          val t1 = applySeqToTransition(overflow, recover())
          composeTransition(t0,t1)
        }
      }
    State.Halted(
      issues = s1.issues,
      optRecover = optRecover
    )
  }
  
  private[utility] def compose[A,B,C,D,ZZ](s0 : State.Success[A,B,ZZ], s1 : State.Continuation[B,C,D]) : State.Halted[A,C,D] = {
    // Note: If Success/Continuation occurs, EOI should be applied to Continuation, however we can't do that here since it would discard the Transition
    // Note: can only reach here if user passes in Success/Continuation - recursion from ComposedState above handles EOI correctly
    // TODO: test to verify this
    val optRecover : Option[() => Transition[A,C,D]] = Some(
      () => composeTransition(
        new Succeed(state=s0, output=Seq.empty, overflow=Seq.empty, metadata=Seq.empty),
        s1.apply(EndOfInput)
      )
    )
    val msg = "No more xs available from s0, EOI should have been applied to s1 prior to composing s0 and s1"
    State.Halted(
      issues = Seq(Issue(WARN,msg)),
      optRecover = optRecover
    )
  }
  private[utility] def compose[A,B,C,D,ZZ](s0 : State.Success[A,B,ZZ], s1 : State.Success[B,C,D]) : State.Success[A,C,D] = State.Success(s1.value)
  private[utility] def compose[A,B,C,D,ZZ](s0 : State.Success[A,B,ZZ], s1 : State.Halted[B,C,D], overflow: Seq[B]) : State.Halted[A,C,D] = {
    // TODO: test to verify this
    val optRecover : Option[() => Transition[A,C,D]] =
      s1.optRecover map { recover =>
        () => {
          val t0 = new Succeed(state = s0, output=Seq.empty[B], overflow=Seq.empty[A],metadata=Seq.empty[Any])
          val t1 = applySeqToTransition(overflow, recover())
          composeTransition(t0,t1)
        }
      }
    State.Halted(
      issues = s1.issues,
      optRecover = optRecover
    )
  }
  
  private[utility] def compose[A,B,C,D,ZZ](s0 : State.Halted[A,B,ZZ], s1 : State.Continuation[B,C,D]) : State.Halted[A,C,D] = {
    // TODO: test to verify this
    val optRecover : Option[() => Transition[A,C,D]] =
      s0.optRecover map { recover =>
        () =>
          composeTransitionAndStateContinue(recover(), s1)
      }
    State.Halted(
      issues = s0.issues,
      optRecover = optRecover
    )
  }
  private[utility] def compose[A,B,C,D,ZZ](s0 : State.Halted[A,B,ZZ], s1 : State.Success[B,C,D]) : State.Halted[A,C,D] = {
    // TODO: test to verify this
    val optRecover : Option[() => Transition[A,C,D]] =
      s0.optRecover map { recover =>
        { () =>
          val t0 : Transition[A,B,ZZ] = recover()
          t0.fold[Transition[A,C,D]](
            ifSucceed = t0 => new Succeed(state=State.Success(s1.value),output=Seq.empty[C],overflow=t0.overflow,metadata=t0.metadata),
            ifHalt = t0 => Halt(state=compose(t0.state, s1),output=Seq.empty,overflow=t0.overflow,metadata=t0.metadata),
            ifContinue = t0 =>
              // Note: output Seq[B] from t0 is thrown away here
              new Succeed(state=State.Success(s1.value),output=Seq.empty[C], overflow=Seq.empty[A], metadata=t0.metadata)
          )
        }
      }
    State.Halted(
      issues = s0.issues,
      optRecover = optRecover
    )
  }
  private[utility] def compose[A,B,C,D,ZZ](s0 : State.Halted[A,B,ZZ], s1 : State.Halted[B,C,D], overflow: Seq[B]) : State.Halted[A,C,D] = {
    // TODO: test to verify this
    val optRecover : Option[() => Transition[A,C,D]] =
      for {
        s0_recover <- s0.optRecover
        s1_recover <- s1.optRecover
      }
      yield {
        () => {
          val t0 = s0_recover()
          val t1 = applySeqToTransition(overflow, s1_recover())
          composeTransition(t0,t1)
        }
      }
    State.Halted(
      issues = s1.issues ++ s0.issues,
      optRecover = optRecover
    )
  }
  
  /**
   * Connect two states into a composite state such that the output of s0 feeds the input of s1
   * @param s0
   * @param s1
   * @tparam A
   * @tparam B
   * @tparam C
   * @tparam D
   * @tparam ZZ
   * @return
   */
  def connectStates[A,B,C,D,ZZ](s0 : State[A,B,ZZ], s1 : State[B,C,D]) : State[A,C,D] = {
    s0.fold(
      ifContinuation  =   s0 => s1.fold( ifContinuation = s1 => compose(s0,s1), ifSuccess = s1 => compose(s0,s1), ifHalted = s1 => compose(s0,s1,Seq.empty[B])),
      ifSuccess       =   s0 => s1.fold( ifContinuation = s1 => compose(s0,s1), ifSuccess = s1 => compose(s0,s1), ifHalted = s1 => compose(s0,s1,Seq.empty[B])),
      ifHalted        =   s0 => s1.fold( ifContinuation = s1 => compose(s0,s1), ifSuccess = s1 => compose(s0,s1), ifHalted = s1 => compose(s0,s1,Seq.empty[B]))
    )
  }

  // This is a case class instead of anonymous class for better debug messages
  private[utility] case class ConnectedStateMachine[A,B,C,D,ZZ](m0 : StateMachine[A,B,ZZ], m1 : StateMachine[B,C,D]) extends StateMachine[A,C,D] {
    def s0 = connectStates(m0.s0,m1.s0)
  }

  /**
   * Connect two state machines into a composite state machine such that the output of s0 feeds the input of s1
   * @param m0
   * @param m1
   * @tparam A
   * @tparam B
   * @tparam C
   * @tparam D
   * @tparam ZZ
   * @return
   */
  def connectStateMachines[A,B,C,D,ZZ](m0 : StateMachine[A,B,ZZ], m1 : StateMachine[B,C,D]) : StateMachine[A,C,D]
    = ConnectedStateMachine(m0,m1)


}

