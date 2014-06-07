package org.s_mach.StepMachine

import org.s_mach._
import scala.collection.immutable.Seq

object impl {

  def flatMapStepMachine[A,B,C](p: StepMachine[A,B],f: B => StepMachine[A,C]) = new StepMachine[A,C] {
    def apply(a: A) = Enumerable.impl.flatMapEnumerableDoneTransition[Unit,B,Unit,Unit,C](p(a),{ b => f(b)(a) })
  }

  def mapStepMachine[A,B,C](p: StepMachine[A,B],f: B => C) = new StepMachine[A,C] {
    def apply(a: A) = Enumerable.impl.flatMapEnumerableDoneTransition[Unit,B,Unit,Unit,C](p(a),{ b => Succeed(f(b)) })
  }

}
