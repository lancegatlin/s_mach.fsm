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
package org.s_mach

import scala.collection.immutable.Seq

package object Plan {
  type Transition[A]        = StateMachine.Transition[Unit,Unit,A]
  object Transition {
    def apply[A](
      state     :   State[A],
      metadata  :   Seq[Any]                      = Seq.empty
    ) : Transition[A] = state.fold(
      ifSuccess = q => new Succeed(state=q, output=Seq.empty, overflow=Seq.empty, metadata=metadata),
      ifHalted = q => new Halt(state=q, output=Seq.empty, overflow=Seq.empty, metadata=metadata),
      ifContinuation = q => new Continue(state=q, output=Seq.empty, metadata=metadata)
    )
  }

  type DoneTransition[A] = StateMachine.DoneTransition[Unit,Unit,A]

  type State[A]             = StateMachine.State[Unit,Unit,A]
  object State {
    type Done[A]            = StateMachine.State.Done[Unit,Unit,A]

    type Continuation[A]    = StateMachine.State.Continuation[Unit,Unit,A]

    type Success[A]         = StateMachine.State.Success[Unit,Unit,A]
    val Success             = StateMachine.State.Success

    type Halted[A]           = StateMachine.State.Halted[Unit,Unit,A]
    val Halted               = StateMachine.State.Halted
  }

  type Continue[A] = StateMachine.Continue[Unit,Unit,A]
  object Continue {
    def apply[A](
      state     :   State.Continuation[A],
      metadata  :   Seq[Any]                      = Seq.empty
    ) = StateMachine.Continue[Unit,Unit,A](state=state, output=Seq.empty, metadata=metadata)
  }

  type Succeed[A] = StateMachine.Succeed[Unit,Unit,A]
  object Succeed {
    def apply[A](
      value : A,
      metadata    :   Seq[Any]                    = Seq.empty
    ) = StateMachine.Succeed[Unit,Unit,A](value=value, metadata=metadata)
  }

  type Halt[A] = StateMachine.Halt[Unit,Unit,A]
  object Halt {
    def apply[A](
      issues      :   Seq[Issue],
      optRecover  :   Option[() => Transition[A]]   = None,
      metadata    :   Seq[Any]                      = Seq.empty    
    ) = StateMachine.Halt[Unit,Unit,A](issues=issues, optRecover=optRecover, metadata=metadata) 
    def warn[A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[A],
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.warn[Unit,Unit,A](message=message, cause=cause, recover=recover, metadata=metadata)
    def error[A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[A],
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.error[Unit,Unit,A](message=message, cause=cause, recover=recover, metadata=metadata)
    def fatal[A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.fatal[Unit,Unit,A](message=message, cause=cause, metadata=metadata)
  }

    /*
  ∑ => input alphabet
  S => set of states
  s0 => initial state (s0 ∈ S)
  ∂ => transition function
  F => set of final states (F ⊂ S)
  A => final success value type
  ∅ => 1) the type of the empty set 2) instance of the empty set
   */
//  type  S  [A]   =   State                      [A]
//  type  F  [A]   =   State.Done                 [A]
//  type  ∂  [A]   =   State.Continue             [A]
//
//  val   ⊳          =   Continue
//  val   ⊡          =   Success
//  val   ⊠          =   Issue
}
