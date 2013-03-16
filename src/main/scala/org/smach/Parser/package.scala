package org.smach

import scala.collection.immutable.Seq

package object Parser {

  type Transition[A] = StateMachine.DoneTransition[Unit,Unit,A]
//  object Transition {
//    def apply[A](
//      state     :   State[A],
//      metadata  :   Seq[Any]                      = Seq.empty
//    ) = StateMachine.Transition[Unit,Unit,A](state=state, metadata=metadata)
//  }

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

  def tell[A] : Parser[A,A] = a => Succeed(a)

    /*
  ∑ => input alphabet
  S => set of states
  s0 => initial state (s0 ∈ S)
  ∂ => transition function
  F => set of final states (F ⊂ S)
  A => final success value type
  ∅ => 1) the type of the empty set 2) instance of the empty set
   */
//  type  S  [∑,A]   =   State                      [∑,A]
//  type  F  [∑,A]   =   State.Done                 [∑,A]
//  type  ∂  [∑,A]   =   State.Continue             [∑,A]
//
//  val   ⊳          =   Continue
//  val   ⊡          =   Success
//  val   ⊠          =   Issue

}
