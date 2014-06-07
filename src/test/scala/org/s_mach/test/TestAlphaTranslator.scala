/*
    Copyright 2012 Georgia Tech Research Institute

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
package org.s_mach.test

import org.s_mach._
import collection.immutable.NumericRange
import scala.collection.immutable.Seq

object TestAlphaTransformer {
  val log = Log(classOf[TestAlphaTransformer])
  val abc = 'a' to 'z' map { _.toString }
  val cba = 'z'.toByte to 'a'.toByte by -1 map { _.toChar.toString }
}
case class TestAlphaTransformer() extends Transformer[String, String] {
  import Transformer._
  import TestAlphaTransformer._

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
