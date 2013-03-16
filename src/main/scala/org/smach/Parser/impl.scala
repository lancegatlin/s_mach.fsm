package org.smach.Parser

import org.smach._
import scala.collection.immutable.Seq

object impl {

  def flatMapParser[A,B,C](p: Parser[A,B],f: B => Parser[A,C]) = new Parser[A,C] {
    def apply(a: A) = Enumerable.impl.flatMapEnumerableDoneTransition[Unit,B,Unit,Unit,C](p(a),{ b => f(b)(a) })
  }

  def mapParser[A,B,C](p: Parser[A,B],f: B => C) = new Parser[A,C] {
    def apply(a: A) = Enumerable.impl.flatMapEnumerableDoneTransition[Unit,B,Unit,Unit,C](p(a),{ b => Succeed(f(b)) })
  }

}
