/*
    Copyright 2012 Georgia Tech Research Institute

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
package org.smach.test

import org.smach._
import collection.immutable.NumericRange
import scala.collection.immutable.Seq

object TestAlphaTranslator {
  val log = Log(classOf[TestAlphaTranslator])
  val abc = 'a' to 'z' map { _.toString }
  val cba = 'z'.toByte to 'a'.toByte by -1 map { _.toChar.toString }
}
case class TestAlphaTranslator() extends Translator[String, String] {
  import Translator._
  import TestAlphaTranslator._

  case class Cont(idx : Int) extends State.Continuation[String, String] {
    def apply(x : String) = {
      val expected = abc(idx)
      if(x == expected) {
        val output = Seq(cba(idx))
        if(idx + 1 == abc.size) {
          Succeed(output)
        } else {
          Continue(
            state = Cont(idx+1),
            output = output
          )
        }
      } else {
        Halt.fatal(s"expected $expected!")
      }
    }

    def apply(x: EndOfInput) = Halt.fatal("incomplete input!")
  }

  def s0 = Cont(0)
}
