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

package object StepMachine {

  type Transition[A] = StateMachine.DoneTransition[Unit,Unit,A]

  type State[A] = StateMachine.State.Done[Unit,Unit,A]
  object State {
    type Success[A] = StateMachine.State.Success[Unit,Unit,A]
    val Success = StateMachine.State.Success

    type Halted[A] = StateMachine.State.Halted[Unit,Unit,A]
    val Halted = StateMachine.State.Halted
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
      optRecover  :   Option[() => Transition[A]] = None,
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

  def tell[A] : StepMachine[A,A] = a => Succeed(a)

}
