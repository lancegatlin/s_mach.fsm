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

trait StateMachine[I,O,A] {
  import StateMachine._
  def s0 : State[I,O,A]
}
object StateMachine {
  sealed trait Transition[I,O,A] {
    def fold[X](ifSucceed: Succeed[I,O,A] => X, ifHalt: Halt[I,O,A] => X, ifContinue: Continue[I,O,A] => X) : X
    def state     :   State[I,O,A]
    def output    :   Seq[O]
    def metadata  :   Seq[Any]
  }

  object Transition {
    def apply[I,O,A](
      state : State[I,O,A]
    ) : Transition[I,O,A] = state.fold(
        ifSuccess = q => Succeed(state=q, output=Seq.empty, overflow=Seq.empty, metadata=Seq.empty),
        ifHalted = q => Halt(state=q, output=Seq.empty, overflow=Seq.empty, metadata=Seq.empty),
        ifContinuation = q => Continue(state=q,output=Seq.empty,metadata=Seq.empty)
      )
  }

  final case class Continue[I,O,A](
    state         :   State.Continuation[I,O,A],
    output        :   Seq[O] = Seq.empty[O],
    metadata      :   Seq[Any] = Seq.empty[Any]
  ) extends Transition[I,O,A] {
    def fold[X](ifSucceed: Succeed[I,O,A] => X, ifHalt: Halt[I,O,A] => X, ifContinue: Continue[I,O,A] => X) = ifContinue(this)
  }

  sealed trait DoneTransition[I,O,A] extends Transition[I,O,A] {
    def doneFold[X](ifSucceed: Succeed[I,O,A] => X, ifHalt: Halt[I,O,A] => X) : X
    override def state : State.Done[I,O,A]
    def overflow : Seq[I]
  }

  final case class Succeed[I,O,A](
    state         :   State.Success[I,O,A],
    output        :   Seq[O],
    overflow      :   Seq[I],
    metadata      :   Seq[Any]
  ) extends DoneTransition[I,O,A] {
    def fold[X](ifSucceed: Succeed[I,O,A] => X, ifHalt: Halt[I,O,A] => X, ifContinue: Continue[I,O,A] => X) = ifSucceed(this)
    def doneFold[X](ifSucceed: Succeed[I,O,A] => X, ifHalt: Halt[I,O,A] => X) = ifSucceed(this)
  }

  object Succeed {
    def apply[I,O,A](
      value       :   A,
      output      :   Seq[O] = Seq.empty[O],
      overflow    :   Seq[I] = Seq.empty[I],
      metadata    :   Seq[Any] = Seq.empty[Any]
    ) = new Succeed[I,O,A](
      state     =   State.Success(value),
      output    =   output,
      overflow  =   overflow,
      metadata  =   metadata
    )
  }

  final case class Halt[I,O,A](
    state         :   State.Halted[I,O,A],
    output        :   Seq[O],
    overflow      :   Seq[I],
    metadata      :   Seq[Any]
  ) extends DoneTransition[I,O,A] {
    def fold[X](ifSucceed: Succeed[I,O,A] => X, ifHalt: Halt[I,O,A] => X, ifContinue: Continue[I,O,A] => X) = ifHalt(this)
    def doneFold[X](ifSucceed: Succeed[I,O,A] => X, ifHalt: Halt[I,O,A] => X) = ifHalt(this)
  }

  object Halt {
    def apply[I,O,A](
      issues      :   Seq[Issue],
      optRecover  :   Option[() => Transition[I,O,A]],
      output      :   Seq[O]                            = Seq.empty[O],
      overflow    :   Seq[I]                            = Seq.empty[I],
      metadata    :   Seq[Any]                          = Seq.empty[Any]
    ) : Halt[I,O,A] = new Halt[I,O,A](
        state     =   State.Halted(
        issues        =   issues,
        optRecover    =   optRecover
      ),
      output    =   output,
      overflow  =   overflow,
      metadata  =   metadata
    )
    def warn[I,O,A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[I,O,A],
      output      :   Seq[O]                       = Seq.empty[O],
      overflow    :   Seq[I]                       = Seq.empty[I],
      metadata    :   Seq[Any]                     = Seq.empty[Any]
    ) = new Halt[I,O,A](State.Halted(issues=Seq(Issue.warn(message,cause)), optRecover=Some(recover)), output=output, overflow=overflow, metadata=metadata)
    def error[I,O,A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[I,O,A],
      output      :   Seq[O]                       = Seq.empty[O],
      overflow    :   Seq[I]                       = Seq.empty[I],
      metadata    :   Seq[Any]                     = Seq.empty[Any]
    ) = new Halt[I,O,A](State.Halted(issues=Seq(Issue.error(message,cause)), optRecover=Some(recover)), output=output, overflow=overflow, metadata=metadata)
    def fatal[I,O,A](
      message     :   String,
      cause       :   Option[Throwable]           = None,
      output      :   Seq[O]                      = Seq.empty[O],
      overflow    :   Seq[I]                      = Seq.empty[I],
      metadata    :   Seq[Any]                    = Seq.empty[Any]
    ) = new Halt[I,O,A](State.Halted(issues=Seq(Issue.fatal(message,cause))), output=output, overflow=overflow, metadata=metadata)
  }

  sealed trait State[I,O,A] {

    // Better performing alternative to using match statement
    def fold[X](
      ifContinuation  : State.Continuation  [I,O,A] => X,
      ifSuccess       : State.Success       [I,O,A] => X,
      ifHalted        : State.Halted        [I,O,A] => X
    ) : X
  }

  object State {
    sealed trait Done[I,O,A] extends State[I,O,A] {
      // Better performing alternative to using match statement
      def fold[X](
        ifSuccess     : State.Success  [I,O,A]  => X,
        ifHalted      : State.Halted   [I,O,A]  => X
      ) : X
    }

    trait Continuation[I,O,A] extends State[I,O,A] {

      def apply( xs  : Seq[I]     ) : Transition[I,O,A] = utility.applySeqToState(xs,this)
      def apply( x   : I          ) : Transition[I,O,A]
      def apply( x   : EndOfInput ) : DoneTransition[I,O,A]

      final def fold[X](
        ifContinuation  : State.Continuation  [I,O,A] => X,
        ifSuccess       : State.Success       [I,O,A] => X,
        ifHalted        : State.Halted        [I,O,A] => X
      ) = ifContinuation(this)

    }

    final case class Success[I,O,A](value : A) extends Done[I,O,A] {

      def fold[X](
        ifContinuation  : State.Continuation  [I,O,A] => X,
        ifSuccess       : State.Success       [I,O,A] => X,
        ifHalted        : State.Halted        [I,O,A] => X
      ) = ifSuccess(this)

      def fold[X](
        ifSuccess     : State.Success [I,O,A]  => X,
        ifHalted      : State.Halted  [I,O,A]  => X
      ) = ifSuccess(this)
    }

    final case class Halted[I,O,A](
      issues          :   Seq[Issue],
      optRecover      :   Option[() => Transition[I,O,A]] = None
    ) extends Done[I,O,A] {
      def fold[X](
        ifContinuation  : State.Continuation  [I,O,A] => X,
        ifSuccess       : State.Success       [I,O,A] => X,
        ifHalted        : State.Halted        [I,O,A] => X
      ) = ifHalted(this)

      def fold[X](
        ifSuccess     : State.Success    [I,O,A]  => X,
        ifHalted      : State.Halted     [I,O,A]  => X
      ) = ifHalted(this)

      lazy val severityCode = issues.maxBy({ _.severityCode }).severityCode
    }

    object Halted {
      def warn[I,O,A](
        message     :   String,
        cause       :   Option[Throwable]            = None,
        recover     :   () => Transition[I,O,A]
      ) = apply[I,O,A](issues=Seq(Issue.warn(message,cause)), optRecover=Some(recover))
      def error[I,O,A](
        message     :   String,
        cause       :   Option[Throwable]            = None,
        recover     :   () => Transition[I,O,A]
      ) = apply[I,O,A](issues=Seq(Issue.error(message,cause)), optRecover=Some(recover))
      def fatal[I,O,A](
        message     :   String,
        cause       :   Option[Throwable]            = None,
        metadata    :   Seq[Any]                     = Seq.empty
      ) = apply[I,O,A](issues=Seq(Issue.fatal(message,cause)))
    }
  }

}