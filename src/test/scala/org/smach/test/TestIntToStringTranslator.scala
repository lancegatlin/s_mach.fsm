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
import scala.collection.immutable.Seq

object TestIntToStringTranslator {
  val log = Log(classOf[TestIntToStringTranslator])
}
case class TestIntToStringTranslator() extends Translator[Int, String] {
  import Translator._
  import TestIntToStringTranslator._
  case class Cont() extends State.Continuation[Int,String]  {

    def apply(item: Int) = {
      val s : String = item.toString
      Continue(
        state = this,
        output = List(s),
        metadata = Seq(log.info("info1"),log.info("info2"))
      )
    }

    def apply(eoi : EndOfInput) = Succeed(
      metadata = Seq(log.info("info1"),log.info("info2"))
    )
  }

  def s0 = Cont()
}
