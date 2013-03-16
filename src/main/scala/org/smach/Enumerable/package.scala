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

import org.smach.IssueSeverityCode._
import scala.collection.immutable.Seq

package object Enumerable {
  type Transition[O,A]          = StateMachine.Transition[Unit, O, A]
  object Transition {
    def apply[O,A](
      state     :   State[O,A],
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) : Transition[O,A] = state.fold(
      ifSuccess = q => new Succeed(state=q, output=output, overflow=Seq.empty, metadata=metadata),
      ifHalted = q => new Halt(state=q, output=output, overflow=Seq.empty, metadata=metadata),
      ifContinuation = q => new Continue(state=q, output=output, metadata=metadata)
    )
  }

  type DoneTransition[O,A] = StateMachine.DoneTransition[Unit,O,A]

  type State[O,A]             = StateMachine.State[Unit, O, A]
  object State {
    type Continuation[O,A]    = StateMachine.State.Continuation[Unit, O, A]
    type Done[O,A]            = StateMachine.State.Done[Unit, O, A]

    type Success[O,A]         = StateMachine.State.Success[Unit, O, A]
    val Success               = StateMachine.State.Success

    type Halted[O,A]           = StateMachine.State.Halted[Unit, O, A]
    val Halted                 = StateMachine.State.Halted
  }

  type Continue[O,A] = StateMachine.Continue[Unit,O,A]
  val Continue = StateMachine.Continue

  type Succeed[O,A] = StateMachine.Succeed[Unit,O,A]
  val Succeed = StateMachine.Succeed

  type Halt[O,A] = StateMachine.Halt[Unit,O,A]
  object Halt {
    def apply[O,A](
      issues      :   Seq[Issue],
      optRecover  :   Option[() => Transition[O,A]] = None,
      output      :   Seq[O]                        = Seq.empty,
      metadata    :   Seq[Any]                      = Seq.empty
    ) = StateMachine.Halt[Unit,O,A](issues=issues, optRecover=optRecover, output=output, metadata=metadata)
    def warn[O,A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[O,A],
      output      :   Seq[O]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.warn[Unit,O,A](message=message, cause=cause, recover=recover, output=output, metadata=metadata)
    def error[O,A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[O,A],
      output      :   Seq[O]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.error[Unit,O,A](message=message, cause=cause, recover=recover, output=output, metadata=metadata)
    def fatal[O,A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      output      :   Seq[O]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.fatal[Unit,O,A](message=message, cause=cause, output=output, metadata=metadata)
  }

  /*
    Γ => output alphabet
    S => set of states
    s0 => initial state (s0 ∈ S)
    ∂ => state that can be transitioned
    F => set of final states (F ⊂ S)
  */
//  type  S  [Γ,A]   =   State                [Γ,A]
//  type  F  [Γ,A]   =   State.Done           [Γ,A]
//  type  ∂  [Γ,A]   =   State.Continue       [Γ,A]
//
//  val   ⊳        =   Continue
//  val   ⊡        =   Success
//  val   ⊠        =   Issue
}
