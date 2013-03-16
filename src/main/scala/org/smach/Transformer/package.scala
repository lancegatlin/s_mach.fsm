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

package object Transformer {
  type Transition[I,O]        = StateMachine.Transition[I,O,Unit]
  object Transition {
    def apply[I,O](
      state     :   State[I,O],
      output    :   Seq[O]                      = Seq.empty,
      overflow  :   Seq[I]                      = Seq.empty,
      metadata  :   Seq[Any]                    = Seq.empty
    ) : Transition[I,O] = state.fold(
      ifSuccess = q => new Succeed(state=q, output=output, overflow=overflow, metadata=metadata),
      ifHalted = q => new Halt(state=q, output=output, overflow=overflow, metadata=metadata),
      ifContinuation = q => new Continue(state=q, output=output, metadata=metadata)
    )
  }

  type DoneTransition[I,O] = StateMachine.DoneTransition[I,O,Unit]

  type State[I,O]             = StateMachine.State[I,O,Unit]
  object State {
    type Done[I,O]            = StateMachine.State.Done[I,O,Unit]

    type Continuation[I,O]    = StateMachine.State.Continuation[I,O,Unit]

    type Success[I,O]         = StateMachine.State.Success[I,O,Unit]
    val Success               = StateMachine.State.Success

    type Halted[I,O]           = StateMachine.State.Halted[I,O,Unit]
    val Halted                 = StateMachine.State.Halted
  }

  type Continue[I,O] = StateMachine.Continue[I,O,Unit]
  object Continue {
    def apply[I,O](
      state     :   State.Continuation[I,O],
      output    :   Seq[O]                        = Seq.empty,
      metadata  :   Seq[Any]                      = Seq.empty
    ) = StateMachine.Continue[I,O,Unit](state=state, output=output, metadata=metadata)
  }

  type Succeed[I,O] = StateMachine.Succeed[I,O,Unit]
  object Succeed {
    def apply[I,O](
      output    :   Seq[O]                        = Seq.empty,
      overflow  :   Seq[I]                        = Seq.empty,
      metadata  :   Seq[Any]                      = Seq.empty
    ) = StateMachine.Succeed[I,O,Unit](value=(), output=output, overflow=overflow, metadata=metadata)
  }

  type Halt[I,O] = StateMachine.Halt[I,O,Unit]
  object Halt {
    def apply[I,O](
      issues      :   Seq[Issue],
      optRecover  :   Option[() => Transition[I,O]] = None,
      output      :   Seq[O]                        = Seq.empty,
      overflow    :   Seq[I]                        = Seq.empty,
      metadata    :   Seq[Any]                      = Seq.empty
    ) = StateMachine.Halt[I,O,Unit](issues=issues, optRecover=optRecover, output=output, overflow=overflow, metadata=metadata)
    def warn[I,O](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[I,O],
      output      :   Seq[O]                       = Seq.empty,
      overflow    :   Seq[I]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.warn[I,O,Unit](message=message, cause=cause, recover=recover, output=output, overflow=overflow, metadata=metadata)
    def error[I,O](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[I,O],
      output      :   Seq[O]                       = Seq.empty,
      overflow    :   Seq[I]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.error[I,O,Unit](message=message, cause=cause, recover=recover, output=output, overflow=overflow, metadata=metadata)
    def fatal[I,O](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      output      :   Seq[O]                       = Seq.empty,
      overflow    :   Seq[I]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.fatal[I,O,Unit](message=message, cause=cause, overflow=overflow, metadata=metadata)
  }

  // TODO: test me
  def tee[A](f: A => Unit) : Transformer[A,A] = new MapTransformer[A,A]({ a => f(a);a })
  def teeEOI[A](f: EndOfInput => Unit) : Transformer[A,A] = new EOITransformer[A]({ eoi => f(eoi);Nil })

  def collectTee[A](f: PartialFunction[Input[A],Unit]) : Transformer[A,A] = new MapInputTransformer[A,A]({ case i@Chunk(xs) => f(i);xs case i@EndOfInput => f(i);Nil })

  def map[A,B](f: A => B) : Transformer[A,B] = new MapTransformer[A,B](f)

  def collect[A,B](f: PartialFunction[Input[A],Seq[B]]) : Transformer[A,B] = new MapInputTransformer[A,B](f)

}
