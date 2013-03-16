package org.smach.Parser

import org.smach._
import org.smach.Parser._
import scala.collection.immutable.Seq

object impl {

//  def flatMapTransition[B,C](t0: Transition[B], f: B => Transition[C]) : Transition[C] = {
//    t0.doneFold[Transition[C]](
//      ifContinue = t0 => throw new IllegalStateException,
//      ifSucceed = t0 => {
//        val t1 = f(t0.state.value)
//        t1.doneFold[Transition[C]](
//          ifContinue = t1 => throw new IllegalStateException,
//          ifSucceed = t1 => t0.copy(metadata = t1.metadata ++ t0.metadata),
//          ifHalt = t1 => t0.copy(metadata = t1.metadata ++ t0.metadata)
//        )
//      },
//      ifHalt = t0 => {
//        val optRecover : Option[() => Transition[C]] = t0.state.optRecover map { recover => { () =>
//          flatMapTransition(recover(),f)
//        }}
//        new Halt(
//          state = new State.Halted(
//            issues = t0.state.issues,
//            optRecover = optRecover
//          ),
//          output = Seq.empty,
//          overflow = Seq.empty,
//          metadata = t0.metadata
//        )
//      }
//    )
//  }

  def flatMapParser[A,B,C](p: Parser[A,B],f: B => Parser[A,C]) = new Parser[A,C] {
    def apply(a: A) = Enumerable.impl.flatMapEnumerableDoneTransition[Unit,B,Unit,Unit,C](p(a),{ b => f(b)(a) })
  }

  def mapParser[A,B,C](p: Parser[A,B],f: B => C) = new Parser[A,C] {
    def apply(a: A) = Enumerable.impl.flatMapEnumerableDoneTransition[Unit,B,Unit,Unit,C](p(a),{ b => Succeed(f(b)) })
  }

}
