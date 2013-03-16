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

import scala.collection.immutable.Seq

package object Iteratee {
  type Transition[I,A]        = StateMachine.Transition[I,Unit,A]
  object Transition {
    def apply[I,A](
      state     :   State[I,A],
      overflow  :   Seq[I]                        = Seq.empty,
      metadata  :   Seq[Any]                      = Seq.empty
    ) : Transition[I,A] = state.fold(
      ifSuccess = q => new Succeed(state=q, output=Seq.empty, overflow=overflow, metadata=metadata),
      ifHalted = q => new Halt(state=q, output=Seq.empty, overflow=overflow, metadata=metadata),
      ifContinuation = q => new Continue(state=q, output=Seq.empty, metadata=metadata)
    )
  }

  type DoneTransition[I,A] = StateMachine.DoneTransition[I,Unit,A]

  type State[I,A]             = StateMachine.State[I,Unit,A]
  object State {
    type Done[I,A]            = StateMachine.State.Done[I,Unit,A]

    type Continuation[I,A]    = StateMachine.State.Continuation[I,Unit,A]
    
    type Success[I,A]         = StateMachine.State.Success[I,Unit,A]
    val Success               = StateMachine.State.Success

    type Halted[I,A]          = StateMachine.State.Halted[I,Unit,A]
    val Halted                = StateMachine.State.Halted
  }

  type Continue[I,A] = StateMachine.Continue[I,Unit,A]
  object Continue {
    def apply[I,A](
      state     :   State.Continuation[I,A],
      metadata  :   Seq[Any]                      = Seq.empty
    ) = StateMachine.Continue[I,Unit,A](state=state, output=Seq.empty, metadata=metadata)
  }

  type Succeed[I,A] = StateMachine.Succeed[I,Unit,A]
  object Succeed {
    def apply[I,A](
      value : A,
      overflow    :   Seq[I]                      = Seq.empty,
      metadata    :   Seq[Any]                    = Seq.empty
    ) = StateMachine.Succeed[I,Unit,A](value=value, output=Seq.empty, overflow=overflow, metadata=metadata)
  }

  type Halt[I,A] = StateMachine.Halt[I,Unit,A]
  object Halt {
    def apply[I,A](
      issues      :   Seq[Issue],
      optRecover  :   Option[() => Transition[I,A]] = None,
      overflow    :   Seq[I]                        = Seq.empty,
      metadata    :   Seq[Any]                      = Seq.empty    
    ) = StateMachine.Halt[I,Unit,A](issues=issues, optRecover=optRecover, overflow=overflow, metadata=metadata) 
    def warn[I,A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[I,A],
      overflow    :   Seq[I]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.warn[I,Unit,A](message=message, cause=cause, recover=recover, overflow=overflow, metadata=metadata)
    def error[I,A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[I,A],
      overflow    :   Seq[I]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.error[I,Unit,A](message=message, cause=cause, recover=recover, overflow=overflow, metadata=metadata)
    def fatal[I,A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      overflow    :   Seq[I]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.fatal[I,Unit,A](message=message, cause=cause, overflow=overflow, metadata=metadata)
  }

  def peek[A] : Iteratee[A,A] = impl.peek[A]

}
