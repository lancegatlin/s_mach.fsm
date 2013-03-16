<h2>Introduction</h2>
Smach is an open-source Scala library designed to support creation of functional, composable, streamable and recoverable state machines. Smach state machines can be composed together to form larger state machines or connected together to form a processing pipeline plan that can be run to produce a final value. Smach state machines also support a generic error recovery system (by way of the Halted state) that may be used to report errors encountered during processing by any composed or connected smach state machine. Additionally, when an error is encountered, smach state machines may provide an optional recovery method for fixing or ignoring broken input. While running a composed or connected smach state machine, the client is provided with the error message and empowered with the decision of whether to proceed with processing despite any errors.

In smach, a state machine is defined as an object that has an initial State:

````scala

trait StateMachine[I,O,A] {
  def s0 : State[I,O,A]
}

````

A State is a class that represents the state of a state machine at some point in processing. The State class is never instantiated directly, instead one of the three basic sub-types are created:
<ul>
  <li><strong>Continuation</strong>:  represents the state of a state machine that requires more input to continue processing.</li>
	<li><strong>Success</strong>: represents the state of a state machine that has completed processing and which resulted in a single value.</li>
	<li><strong>Halted</strong>: represents the state of a state machine that has performed some processing but did not successfully complete that processing and which might be recoverable</li>
</ul>
````scala

sealed trait State[I,O,A] { ... }

trait Continuation[I,O,A] extends State[I,O,A] {
  ...
  def apply(xs : Seq[I]) : Transition[I,O,A]
  def apply(x : I) : Transition[I,O,A]
  def apply(x : EndOfInput) : DoneTransition[I,O,A]
}

sealed trait Done[I,O,A] extends State[I,O,A] { ... }

final case class Success[I,O,A](value : A) extends Done[I,O,A] { ... }

final case class Halted[I,O,A](issues : Seq[Issue], optRecover : Option[() => Transition[I,O,A]] = None) extends Done[I,O,A] { ... }

````

<h3>Transitions</h3>
When input is applied to a Continuation, a Transition-derived object is created which has the next state of the state machine and any output produced by processing the input by the Continuation:
````scala
sealed trait Transition[I,O,A] {
  ...
  def state : State[I,O,A]
  def output : Seq[O]
}
````

The DoneTransition is a sub-type of Transition returned when EndOfInput (EOI) is applied to a Continuation state. A DoneTransition state can only return Done states: Halted or Success. A DoneTransition may also have overflow, input that was not consumed during processing.
````scala
sealed trait DoneTransition[I,O,A] extends Transition[I,O,A] {  
  override def state : State.Done[I,O,A]
  def overflow : Seq[I]
}
````

Each of the basic State-derived types has a corresponding Transition-derived type:
````scala
final case class Continue[I,O,A](
  state : State.Continuation[I,O,A],
  output : Seq[O] = Seq.empty[O]
  ...
) extends Transition[I,O,A] { ... }

final case class Succeed[I,O,A](
  state : State.Success[I,O,A],
  output : Seq[O],
  overflow : Seq[I]
  ...
) extends DoneTransition[I,O,A] { ... }

final case class Halt[I,O,A](
  state : State.Halted[I,O,A],
  output : Seq[O],
  overflow : Seq[I]
  ...
) extends DoneTransition[I,O,A] { ... }

````
Note: Transition-derived objects are <i>verbs</i> and their State-derived equivalents are <i>nouns</i>.
<h3>Type-Parameters</h3>
All smach state machine classes (StateMachine, State, Transition, etc) accept three type-parameters:
<ul>
	<li><span style="line-height: 14px;"><strong>I</strong> : The type of input consumed by the machine during processing (may be Unit)</span></li>
	<li><strong>O</strong> : The type of output produced by the machine during processing (may be Unit)</li>
	<li><strong>A</strong> : The final value type produced by the machine once processing has completed successfully (also may be Unit)</li>
</ul>
StateMachines are always connected input to output, with the first state machine typically accepting Unit. Only the final value type of the last state machine is passed on when connecting StateMachines:

````scala

val enumerator : StateMachine[Unit,Int,Unit] = ...

val translator : StateMachine[Int,String,Unit] = ...

val iteratee : StateMachine[String, Unit, Float] = ...

val plan : StateMachine[Unit,Unit,Float] = enumerator connect translator connect iteratee

````

<h3>Type-Aliases</h3>
To make working with smach state machines easier, type-aliases are provided based on the state machine's purpose:

An Enumerator produces output in chunks by stepping the enumerator:
````scala
type Enumerator[O] = StateMachine[Unit,O,Unit]
````
````scala
val e : Enumerator[Int] = ...
val tr : Enumerator.Transition[Int] = e.s0.step()
println(tr.output)
tr match {
  case q : Enumerator.Continue[Int] => println("step me more!")
  case q : Enumerator.Succeed[Int] => println("done!")
  case q : Enumerator.Halted[Int] => println("error!")
}
````

An Iteratee consumes input in chunks to eventually yield a final single value:
````scala
type Iteratee[I,A] = StateMachine[I,Unit,A]
````
````scala
val i : Iteratee[Int,String] = ...
val tr : Iteratee.Transition[Int,String] = i.s0(List(1,2,3))
tr match {
  case q : Iteratee.Continue[Int,String] => println("needs more input!")
  case q : Iteratee.Succeed[Int,String] => println(q.value)
  case q : Iteratee.Halted[Int,String] => println("error!")
}
````

A Translator transforms input chunks into output chunks of the same or a different type:
````scala
type Translator[I,O] = StateMachine[I,O,Unit]
````
````scala
val t : Translator[Int,String] = ...
val tr : Translator.Transition[Int,String] = t.s0(List(1,2,3))
println(tr.output)
tr match {
  case q : Translator.Continue[Int,String] => println("accepting more input!")
  case q : Translator.Succeed[Int,String] => println("done!")
  case q : Translator.Halted[Int,String] => println("error!")
}
````

A Plan to stream input from an Enumerator to an Iteratee by way of 0 or more Translators and eventually produce a final single value:
````scala
type Plan[A] = StateMachine[Unit,Unit,A]
````

````scala
val p : Plan[Float] = ...
val result : Plan.DoneTransition[Float] = p.run()
tr match {
  case q : Plan.Succeed[Int] => println("done="+q.value)
  case q : Plan.Halted[Int] => println("error!")
}
````
