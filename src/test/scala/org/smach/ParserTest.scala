package org.smach

import org.scalatest.FunSpec
import org.smach._
import scala.util.Random
import scala.collection.immutable.Seq

class ParserTest extends FunSpec {
  val rnd = Stream.continually(Random.nextInt(100))

//  object AllOrNoneCode extends Enumeration {
//    type AllOrNoneCode = Value
//    val NONE, ALL= Value
//  }
//  import AllOrNoneCode._
//
//  case class AllOrNone[A](value: Either[AllOrNoneCode, A])
//
//  def parser[A](subparser: String => Plan.State.Done[A]) : String => Plan.State.Done[AllOrNone[A]] = {
//    case s : String if s == "" => Plan.State.Success(AllOrNone(Left(AllOrNoneCode.NONE)))
//    case s : String if s == "#all" => Plan.State.Success(AllOrNone(Left(AllOrNoneCode.ALL)))
//    case s : String =>
//      val a : Array[String] = s.split("\\s+")
//      val d : Plan.State[A] = Plan.State.Halted(Issue.warn("")::Nil)
//      a.foldLeft(d) { (b,z) =>
//        b.flatMap()
//      }
//  }
  describe("A Parser[A]") {
      def parser1 : Parser[String,Int] = { s =>
        try {
          Parser.Succeed(s.toInt)
        } catch {
          case e : Exception => Parser.Halt.error(e.getMessage,Some(e),{ () =>
            Parser.Succeed(100)
          })
        }
      }
    it("should be composable") {
      val p : Parser[String,Int] =
        for {
          v1 <- parser1
          v2 <- parser1
        } yield v1 + v2
      val t0 = p("1")
      assert(t0 == Parser.Succeed(2))
    }
    it("should be composable even one Halts") {
      val p : Parser[String,Int] =
        for {
          v1 <- parser1
          v2 <- parser1
        } yield v1 + v2
      val t0 = p("asdf")
      assert(t0.state.isHalted && t0.state.isRecoverable)
      val t1 : Parser.Transition[Int] = utility.forceDoneTransition(t0.state.asInstanceOf[Parser.State.Halted[Int]].optRecover.get())
      assert(t0.state.isHalted && t0.state.isRecoverable)
      val t2 : Parser.Transition[Int] = utility.forceDoneTransition(t1.state.asInstanceOf[Parser.State.Halted[Int]].optRecover.get())
      assert(t2 == Parser.Succeed(200))
    }
    it("Transition should be composable") {
      val i : Parser.Transition[Int] =
        for {
          v1 <- parser1("1")
          v2 <- parser1("2")
        } yield v1 + v2
      assert(i == Parser.Succeed(3))
    }
    it("Transition should be composable into a Halted if one of the parsers Halts") {
      val i : Parser.Transition[Int] =
        for {
          v1 <- parser1("1")
          v2 <- parser1("asdf")
          v3 <- parser1("2")
        } yield v1 + v2 + v3
      assert(i.state.isHalted && i.state.isRecoverable)
      val s1 : Parser.Transition[Int] = utility.forceDoneTransition(i.state.asInstanceOf[Parser.State.Halted[Int]].optRecover.get())
      assert(s1 == Parser.Succeed(103))
    }
    it("Transition should be composable from n Parser Transitions") {
      val v : Seq[String] = "1 2 3 4".split("\\s+").toIndexedSeq
      val t : Seq[Parser.Transition[Int]] =  v map { s => parser1(s) }
      val t0 : Parser.Transition[Seq[Int]] = t.sequence
      assert(t0 == Parser.Succeed(List(1,2,3,4)))
    }
    it("Transtion should be composable from n Parser Transition even when one halts") {
      val v : Seq[String] = "1 2 asdf 3 4".split("\\s+").toIndexedSeq
      val t : Seq[Parser.Transition[Int]] =  v map { s => parser1(s) }
      val t0 : Parser.Transition[Seq[Int]] = t.sequence
      assert(t0.state.isHalted && t0.state.isRecoverable)
      val t1 : Parser.Transition[Int] = utility.forceDoneTransition(t0.state.asInstanceOf[Parser.State.Halted[Int]].optRecover.get())
      assert(t1 == Parser.Succeed(List(1,2,100,3,4)))
    }
    it("Transition should be convertable to a Translator Transition") {
      val v : Seq[String] = "1 2 asdf 3 4".split("\\s+").toIndexedSeq
      val t : Seq[Parser.Transition[Int]] =  v map { s => parser1(s) }
      val t0 : Parser.Transition[Seq[Int]] = t.sequence
      assert(t0.state.isHalted && t0.state.isRecoverable)
      // this never gets used
      val temp = new Translator.State.Continuation[Float,Int] {
        def apply(x: Float) = ???
        def apply(x: EndOfInput) = ???
      }
      val t1 : Translator.Transition[Float,Int] = t0.asTransition.flatMap { xs => Translator.Continue[Float,Int](state=temp, output=xs) }
      assert(t1.state.isHalted && t1.state.isRecoverable)
      val t2 : Translator.Transition[Float,Int] = t1.state.asInstanceOf[Translator.State.Halted[Float,Int]].optRecover.get()
      println(t2)
      assert(t2.isContinue && t2.output == List(1,2,100,3,4))
    }
  }

}
