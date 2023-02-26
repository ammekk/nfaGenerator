import scala.collection.mutable.ListBuffer

object Translate {
  def translate(pda: PDACoded): PDA = {
    val statesCoded = pda.states
    val states = statesCoded.zipWithIndex.map{ case (state, idx) =>
      val initial = if (idx == 0) true else false
      State(state.name, initial, state.accepted)
    }
    val transitions = new ListBuffer[Transition]
    statesCoded.foreach( state => {
      val statements = state.statements
      val conditions = statements.map(statement => statement.condition)
      val actions = statements.map(statement => statement.action)
      val condAct = conditions.zip(actions)
      condAct.foreach { case (cond, act) =>
        if (act.dst.getOrElse(None) != None || act.loop == true) {
          val dst = if (act.loop) state.name else act.dst.get
          val read = if (cond.getOrElse(None) != None) cond.get.read else None
          val pop = if (cond.getOrElse(None) != None) cond.get.pop else None
          transitions += Transition(state.name, dst, act.push, pop, read)
        }
      }
    })
    PDA(states, transitions.toList)
  }
}
sealed trait Expression
case class State(name: String, initial: Boolean = false, accepted: Boolean = false) extends Expression

case class Transition(src: String,  dst: String, push:Option[String], pop: Option[String], read: Option[String]) extends Expression
case class PDA(states: List[State], transition: List[Transition]) extends Expression
