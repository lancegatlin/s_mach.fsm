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
package org.smach.Transformer

import org.smach._
import scala.collection.immutable.Seq

case class MapTransformer[A,B](f: A => B) extends Transformer[A,B] {
  import Transformer._
  case class MapTransformerState() extends State.Continuation[A,B] {
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

  def s0 = MapTransformerState()
}

case class EOITransformer[A](f: EndOfInput => Seq[A]) extends Transformer[A,A] {
  import Transformer._
  case class EOITransformerState() extends State.Continuation[A,A] {
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

  def s0 = EOITransformerState()
}

case class MapInputTransformer[A,B](f: PartialFunction[Input[A],Seq[B]]) extends Transformer[A,B] {
  import Transformer._
  case class InputMapTransformerNState() extends State.Continuation[A,B] {
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

  def s0 = InputMapTransformerNState()
}
