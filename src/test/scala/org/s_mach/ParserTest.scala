package org.s_mach

import org.scalatest.FunSpec
import org.s_mach._
import scala.util.Random
import scala.collection.immutable.Seq

class StepMachineTest extends FunSpec {
  val rnd = Stream.continually(Random.nextInt(100))

//  object AllOrNoneCode extends Enumeration {
//    type AllOrNoneCode = Value
//    val NONE, ALL= Value
//  }
//  import AllOrNoneCode._
//
//  case class AllOrNone[A](value: Either[AllOrNoneCode, A])
//
//  def StepMachine[A](subStepMachine: String => Plan.State.Done[A]) : String => Plan.State.Done[AllOrNone[A]] = {
//    case s : String if s == "" => Plan.State.Success(AllOrNone(Left(AllOrNoneCode.NONE)))
//    case s : String if s == "#all" => Plan.State.Success(AllOrNone(Left(AllOrNoneCode.ALL)))
//    case s : String =>
//      val a : Array[String] = s.split("\\s+")
//      val d : Plan.State[A] = Plan.State.Halted(Issue.warn("")::Nil)
//      a.foldLeft(d) { (b,z) =>
//        b.flatMap()
//      }
//  }
  describe("A StepMachine[A]") {
      def StepMachine1 : StepMachine[String,Int] = { s =>
        try {
          StepMachine.Succeed(s.toInt)
        } catch {
          case e : Exception => StepMachine.Halt.error(e.getMessage,Some(e),{ () =>
            StepMachine.Succeed(100)
          })
        }
      }
    it("should be composable") {
      val p : StepMachine[String,Int] =
        for {
          v1 <- StepMachine1
          v2 <- StepMachine1
        } yield v1 + v2
      val t0 = p("1")
      assert(t0 == StepMachine.Succeed(2))
    }
    it("should be composable even one Halts") {
      val p : StepMachine[String,Int] =
        for {
          v1 <- StepMachine1
          v2 <- StepMachine1
        } yield v1 + v2
      val t0 = p("asdf")
      assert(t0.state.isHalted && t0.state.isRecoverable)
      val t1 : StepMachine.Transition[Int] = utility.forceDoneTransition(t0.state.asInstanceOf[StepMachine.State.Halted[Int]].optRecover.get())
      assert(t0.state.isHalted && t0.state.isRecoverable)
      val t2 : StepMachine.Transition[Int] = utility.forceDoneTransition(t1.state.asInstanceOf[StepMachine.State.Halted[Int]].optRecover.get())
      assert(t2 == StepMachine.Succeed(200))
    }
    it("Transition should be composable") {
      val i : StepMachine.Transition[Int] =
        for {
          v1 <- StepMachine1("1")
          v2 <- StepMachine1("2")
        } yield v1 + v2
      assert(i == StepMachine.Succeed(3))
    }
    it("Transition should be composable into a Halted if one of the StepMachines Halts") {
      val i : StepMachine.Transition[Int] =
        for {
          v1 <- StepMachine1("1")
          v2 <- StepMachine1("asdf")
          v3 <- StepMachine1("2")
        } yield v1 + v2 + v3
      assert(i.state.isHalted && i.state.isRecoverable)
      val s1 : StepMachine.Transition[Int] = utility.forceDoneTransition(i.state.asInstanceOf[StepMachine.State.Halted[Int]].optRecover.get())
      assert(s1 == StepMachine.Succeed(103))
    }
    it("Transition should be composable from n StepMachine Transitions") {
      val v : Seq[String] = "1 2 3 4".split("\\s+").toIndexedSeq
      val t : Seq[StepMachine.Transition[Int]] =  v map { s => StepMachine1(s) }
      val t0 : StepMachine.Transition[Seq[Int]] = t.sequence
      assert(t0 == StepMachine.Succeed(List(1,2,3,4)))
    }
    it("Transtion should be composable from n StepMachine Transition even when one halts") {
      val v : Seq[String] = "1 2 asdf 3 4".split("\\s+").toIndexedSeq
      val t : Seq[StepMachine.Transition[Int]] =  v map { s => StepMachine1(s) }
      val t0 : StepMachine.Transition[Seq[Int]] = t.sequence
      assert(t0.state.isHalted && t0.state.isRecoverable)
      val t1 : StepMachine.Transition[Int] = utility.forceDoneTransition(t0.state.asInstanceOf[StepMachine.State.Halted[Int]].optRecover.get())
      assert(t1 == StepMachine.Succeed(List(1,2,100,3,4)))
    }
    it("Transition should be convertable to a Transformer Transition") {
      val v : Seq[String] = "1 2 asdf 3 4".split("\\s+").toIndexedSeq
      val t : Seq[StepMachine.Transition[Int]] =  v map { s => StepMachine1(s) }
      val t0 : StepMachine.Transition[Seq[Int]] = t.sequence
      assert(t0.state.isHalted && t0.state.isRecoverable)
      // this never gets used
      val temp = new Transformer.State.Continuation[Float,Int] {
        def apply(x: Float) = ???
        def apply(x: EndOfInput) = ???
      }
      val t1 : Transformer.Transition[Float,Int] = t0.asTransition.flatMap { xs => Transformer.Continue[Float,Int](state=temp, output=xs) }
      assert(t1.state.isHalted && t1.state.isRecoverable)
      val t2 : Transformer.Transition[Float,Int] = t1.state.asInstanceOf[Transformer.State.Halted[Float,Int]].optRecover.get()
      println(t2)
      assert(t2.isContinue && t2.output == List(1,2,100,3,4))
    }
  }

}
