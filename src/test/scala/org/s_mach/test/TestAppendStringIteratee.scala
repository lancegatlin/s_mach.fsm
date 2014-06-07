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
import scala.collection.immutable.Seq

object TestAppendStringIteratee {
  val log = Log(classOf[TestAppendStringIteratee])
}
case class TestAppendStringIteratee() extends Iteratee[String, String] {
  import Iteratee._
  import TestAppendStringIteratee._
  case class Cont(acc : String) extends State.Continuation[String, String] {

    def apply(item: String) = Continue(
      state = Cont(acc + item),
      metadata = Seq(log.info("info1"),log.info("info2"))
    )

    def apply(eoi : EndOfInput) = Succeed(
      value = acc,
      metadata = Seq(log.info("info1"),log.info("info2"))
    )
  }
  def s0 = Cont("")
}
