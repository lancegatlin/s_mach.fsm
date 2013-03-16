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

import IssueSeverityCode._
import scala.collection.immutable.Seq

package object Enumerator {
  val STD_CHUNK_SIZE = 256

  type Transition[O]        = StateMachine.Transition[Unit, O, Unit]
  object Transition {
    def apply[O](
      state     :   State[O],
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) : Transition[O] = state.fold(
      ifSuccess = q => new Succeed(state=q, output=output, overflow=Seq.empty, metadata=metadata),
      ifHalted = q => new Halt(state=q, output=output, overflow=Seq.empty, metadata=metadata),
      ifContinuation = q => new Continue(state=q, output=output, metadata=metadata)
    )
  }

  type DoneTransition[O,A] = StateMachine.DoneTransition[Unit,O,Unit]

  type State[O]             = StateMachine.State[Unit, O, Unit]
  object State {
    type Continuation[O]    = StateMachine.State.Continuation[Unit, O, Unit]
    type Done[O]            = StateMachine.State.Done[Unit, O, Unit]

    type Success[O]         = StateMachine.State.Success[Unit, O, Unit]
    val Success             = StateMachine.State.Success

    type Halted[O]          = StateMachine.State.Halted[Unit, O, Unit]
    val Halted              = StateMachine.State.Halted
  }

  type Continue[O] = StateMachine.Continue[Unit,O,Unit]
  val Continue = StateMachine.Continue
//  object Continue {
//    def apply[O](
//      state     :   State.Continuation[O],
//      output    :   Seq[O]                    = Seq.empty,
//      metadata  :   Seq[Any]                  = Seq.empty
//    ) = StateMachine.Continue[Unit,O,Unit](state=state, output=output, metadata=metadata)
//  }

  type Succeed[O] = StateMachine.Succeed[Unit,O,Unit]
  object Succeed {
    def apply[O](
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = StateMachine.Succeed[Unit,O,Unit](value=(), output=output, metadata=metadata)
  }

  type Halt[O] = StateMachine.Halt[Unit,O,Unit]
  object Halt {
    def apply[O](
      issues      :   Seq[Issue],
      optRecover  :   Option[() => Transition[O]]   = None,
      output      :   Seq[O]                        = Seq.empty,
      metadata    :   Seq[Any]                      = Seq.empty    
    ) = StateMachine.Halt[Unit,O,Unit](issues=issues, optRecover=optRecover, output=output, metadata=metadata) 
    def warn[O](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[O],
      output      :   Seq[O]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.warn[Unit,O,Unit](message=message, cause=cause, recover=recover, output=output, metadata=metadata)
    def error[O](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[O],
      output      :   Seq[O]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.error[Unit,O,Unit](message=message, cause=cause, recover=recover, output=output, metadata=metadata)
    def fatal[O](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      output      :   Seq[O]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.fatal[Unit,O,Unit](message=message, cause=cause, output=output, metadata=metadata)
  }

  /*
    Γ => output alphabet
    S => set of states
    s0 => initial state (s0 ∈ S)
    ∂ => state that can be transitioned
    F => set of final states (F ⊂ S)
  */
//  type  S  [Γ]  =  State                [Γ]
//  type  F  [Γ]  =  State.Done           [Γ]
//  type  ∂  [Γ]  =  State.Continue       [Γ]
//
//  val   ⊳       =  Continue
//  val   ⊡       =  Success
//  val   ⊠       =  Issue
}
