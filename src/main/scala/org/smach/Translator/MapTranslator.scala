package org.smach.Translator

import org.smach._
import scala.collection.immutable.Seq

case class MapTranslator[A,B](f: A => B) extends Translator[A,B] {
  import Translator._
  case class MapTranslatorState() extends State.Continuation[A,B] {
    override def apply(xs : Seq[A]) = {
      Continue(
        state = this,
        output = xs map f
      )
    }
    def apply(x: A) = {
      Continue(
        state = this,
        output = f(x) :: Nil
      )
    }
    def apply(x: EndOfInput) = Succeed()
  }

  def s0 = MapTranslatorState()
}

case class EOITranslator[A](f: EndOfInput => Seq[A]) extends Translator[A,A] {
  import Translator._
  case class EOITranslatorState() extends State.Continuation[A,A] {
    override def apply(xs : Seq[A]) = {
      Continue(
        state = this,
        output = xs
      )
    }
    def apply(x: A) = {
      Continue(
        state = this,
        output = x :: Nil
      )
    }
    def apply(x: EndOfInput) = Succeed(
      output = f(x)
    )
  }

  def s0 = EOITranslatorState()
}

case class MapInputTranslator[A,B](f: PartialFunction[Input[A],Seq[B]]) extends Translator[A,B] {
  import Translator._
  case class InputMapTranslatorNState() extends State.Continuation[A,B] {
    override def apply(xs : Seq[A]) = {
      Continue(
        state = this,
        output = f.applyOrElse(Input(xs), { i : Input[A] => Nil })
      )
    }
    def apply(x: A) = {
      Continue(
        state = this,
        output = f.applyOrElse(Input(x), { i : Input[A] => Nil })
      )
    }
    def apply(x: EndOfInput) = Succeed(
      output = f.applyOrElse(Input(x), { i : Input[A] => Nil })
    )
  }

  def s0 = InputMapTranslatorNState()
}
