
import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

case class ExtendedBooleanNFA(connections: List[(String, String, Bool)], idToNode: HashMap[String, Node], startNodes: List[String]) {
  def visualizeGraph(filename: String): Unit = {
    val file = new File(filename + ".gv.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    val concatenate_extended_boolean_nfa = "digraph " + filename + " {\n \tfontname=\"Helvetica,Arial,sans-serif\"\n" +
      "\tnode [fontname=\"Helvetica,Arial,sans-serif\"]\n" + "\tedge [fontname=\"Helvetica,Arial,sans-serif\"]\n" +
      "\trankdir=LR;\n" + "\tnode [shape = circle];\n"
    var list = ""
    connections.foreach { case (node, connection, condition) =>  list += node + " -> " + connection + "" + getLabel(condition) + ";\n" }
    bw.write(concatenate_extended_boolean_nfa + list + "}")
    bw.close()
  }

  def getLabel(bool: Bool): String = {
    " [label = \"" + getLabelBool(bool) + "\"]"
  }

  def getLabelBool(bool: Bool): String = {
    bool match {
      case ParenBool(value) =>
        "(" + getLabelBool(value) + ")"
      case ComplementBool(value) =>
        "not (" + getLabelBool(value) + ")"
      case OrBool(lhs, rhs) =>
        getLabelBool(lhs) + " or " + getLabelBool(rhs)
      case AndBool(lhs, rhs) =>
        getLabelBool(lhs) + " and " + getLabelBool(rhs)
      case EqualityBool(lhs, rhs) =>
        getLabelExpression(lhs) + " == " + getLabelExpression(rhs)
      case FlipCoinBool() =>
        "flip_coin()"
      case ValueBool(value) =>
        value.toString

    }
  }

  def getLabelExpression(expr: Expression): String = {
    expr match {
      case InputExpression() =>
        "input"
      case CharExpression(value) =>
        value.toString
      case EOFExpression() =>
        "EOF"
      case NumericExpression(value) =>
        value.toString
      case StringExpression(value) =>
        value
    }
  }

  def getOutGoingEdges(id: String): List[(String, Bool)] = {
    val lb = ListBuffer[(String, Bool)]()
    connections.foreach { case (node, nextNode, condition) =>
      if (node == id) lb += ((nextNode, condition))
    }
    lb.toList
  }
}

case class Graph(connections: List[(String, String, Bool)], idToNode: HashMap[String, Node]) {

  def getOutGoingEdges(id: String): List[(String, Bool)] = {
    val lb = ListBuffer[(String, Bool)]()
    connections.foreach { case (node, nextNode, condition) =>
      if (node == id) lb += ((nextNode, condition))
    }
    lb.toList
  }

  def getStartNodes(): List[String] = {
    idToNode.keySet.toList.filter{ potentialInitialNode =>
      var start = true
      // Can a node be a start node if the only ingoing edge it has is to itself? I think so
      // But then it complicates this because potentially start nodes can have ingoing edges
      // as long as they are loops. For right now I am going to pretend I don't know that
      // until I know it for sure because it makes coding this harder
      connections.foreach{ case(node, connection, _) =>
        if (connection == potentialInitialNode && node != potentialInitialNode) start = false
      }
      start
    }
  }
}

object Simplify {
  def convertControlFlowGraph(cfg: ControlFlowGraphSinglePass): Graph = {
    def checkForReturnAndProduceTransition(cur: String, next: String, condition: Bool): (String, String, Bool) = {
      val node = cfg.idToNode(next)
      node match {
        case Return(_, bool) =>
          (cur, next, AndBool(condition, bool))
        case _ =>
          (cur, next, condition)
      }
    }
    val transitions = ListBuffer[(String, String, Bool)]()
    cfg.idToNode.foreach{ case (id, node) =>
      node match {
        case Return(id, _) =>
          transitions += ((id, id, ValueBool(true)))
        case Continue(id, bool, next) =>
          transitions += checkForReturnAndProduceTransition(id, next, bool)
        case Read(id, next) =>
          transitions += checkForReturnAndProduceTransition(id, next, ValueBool(true))
        case Branch(id, bool, thenNode, elseNode, _) =>
          transitions += checkForReturnAndProduceTransition(id, thenNode, bool)
          transitions += checkForReturnAndProduceTransition(id, elseNode, ComplementBool(bool))
        case _ =>
      }}
    //Temp fix until I remove start node from control flow graph
    Graph(transitions.toList, cfg.idToNode-("Start0"))
  }

  def removeNodeAndCreateNewGraph(id: String, graph: Graph): Graph = {
    val outGoingEdges = graph.getOutGoingEdges(id)
    var newTransitions = List[(String, String, Bool)]()
    var newIDToNode = graph.idToNode.-(id)
    newTransitions = graph.connections.filter{ case(node, nextNode, _) => node != id && nextNode != id }
    graph.connections.foreach{ case(node, nextNode, condition) =>
      if(nextNode == id) {
        newTransitions = newTransitions ++ outGoingEdges.map{ case(nextOutNode, nextCondition) => (node, nextOutNode, AndBool(condition, nextCondition))}
      }
    }
    Graph(newTransitions, newIDToNode)
  }
  def makeExtendedBooleanNFA(cfg: ControlFlowGraphSinglePass): ExtendedBooleanNFA = {
    val initialGraph = convertControlFlowGraph(cfg)
    val nodesToRemove = initialGraph.idToNode.keySet.toList.filter(id => id.contains("If") || id.contains("While") || id.contains("Ensure") )
    var tempBENFA = initialGraph
    nodesToRemove.foreach{ node =>
      tempBENFA = removeNodeAndCreateNewGraph(node, tempBENFA)
    }
    val finalBENFA = tempBENFA
    ExtendedBooleanNFA(finalBENFA.connections, finalBENFA.idToNode, finalBENFA.getStartNodes())
  }

}