///*
//    Copyright 2013 Lance Gatlin
//
//    Author: lance.gatlin@gmail.com
//
//    This file is part of org.smach library.
//
//    org.smach library is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    org.smach library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with org.smach library. If not, see <http://www.gnu.org/licenses/>.
//
//*/
//package org.smach.Plan
//
//import org.smach._
//
//object impl {
//
//  def mapPlanState[A,II,OO,BB](s : Plan.State[A], f: A => BB) : StateMachine.State[II,OO,BB] = {
//    flatMapPlanState[A,II,OO,BB](s, { (a : A) => bindPlanState(f(a)) } )
//  }
//
//  /**
//   * Implementation of Plan.State.bind
//   * @param value
//   * @tparam A
//   * @return
//   */
//  def bindPlanState[I,O,A](value : A) : StateMachine.State[I,O,A] = StateMachine.State.Success(value)
//
//
//  def flatMapPlanState[A,II,OO,BB](s : Plan.State[A], f: A => StateMachine.State[II,OO,BB]) : StateMachine.State[II,OO,BB] = {
//    s.doneFold(
//      ifContinuation = { q =>
////        val (t,_) = Enumerable.impl.runEnumerableState(s, HaltedRecoveryStrategy.STRICT[Unit,Unit,A])
////        if(t.state.isContinuation) {
////          // is it even possible to reach here? No, runEnumerable but because runEnumerable returns a Transition this can't be captured by the type system
////          StateMachine.State.Halted(Issue.fatal("Plan requires more input") :: Nil)
////        } else {
////          flatMapPlanState(t.state, f)
////        }
//        q
//      },
//      ifSuccess = { q =>
//        f(q.value)
//      },
//      ifHalted = { q =>
//        val optRecover : Option[() => StateMachine.Transition[II,OO,BB]] = q.optRecover map { recover => { () =>
//          // Throwing away overflow, output is ok here since for Plan I=Unit and O=Unit so only state matters
//          StateMachine.Transition[II,OO,BB](flatMapPlanState(recover().state, f))
//        }}
//        StateMachine.State.Halted(
//          issues = q.issues,
//          optRecover = optRecover
//        )
//      }
//    )
//  }
//
//  def bindPlan[I,O,A](value : A) = new StateMachine[I,O,A] {
//    def s0 = bindPlanState(value)
//  }
//
//  def mapPlan[A,II,OO,BB](m : Plan[A], f: A => BB) : StateMachine[II,OO,BB] = {
//    flatMapPlan[A,II,OO,BB](m, { (a: A) => bindPlan(f(a)) })
//  }
//
//  private[impl] case class FlatMapPlan[A,II,OO,BB](m : Plan[A], f: A => StateMachine[II,OO,BB]) extends StateMachine[II,OO,BB] {
//    def s0 = flatMapPlanState(m.s0, { (a : A) => f(a).s0 })
//  }
//
//  def flatMapPlan[A,II,OO,BB](m : Plan[A], f: A => StateMachine[II,OO,BB]) : StateMachine[II,OO,BB] = FlatMapPlan(m,f)
//}
