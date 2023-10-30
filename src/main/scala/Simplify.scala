import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

case class Graph(graph: List[(String, String, Bool)], idToNode: HashMap[String, Node]) {
  def visualizeGraph(): Unit = {
    val file = new File("NewSimplifiedGraph.gv.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    val concatenate_graph = "digraph control_flow_graph {\n \tfontname=\"Helvetica,Arial,sans-serif\"\n" +
      "\tnode [fontname=\"Helvetica,Arial,sans-serif\"]\n" + "\tedge [fontname=\"Helvetica,Arial,sans-serif\"]\n" +
      "\trankdir=LR;\n" + "\tnode [shape = circle];\n"
    var hashmap = ""
    graph.foreach { case (node, connection, condition) =>  hashmap += node + " -> " + connection + "" + getLabel(condition) + ";\n" }
    bw.write(concatenate_graph + hashmap + "}")
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
        case Return(id, bool) =>
          transitions += ((id, id, ValueBool(true)))
        case Continue(id, bool, next) =>
          transitions += checkForReturnAndProduceTransition(id, next, bool)
        case Read(id, next) =>
          transitions += checkForReturnAndProduceTransition(id, next, ValueBool(true))
        case Branch(id, bool, thenNode, elseNode, _) =>
          transitions += checkForReturnAndProduceTransition(id, thenNode, bool)
          transitions += checkForReturnAndProduceTransition(id, elseNode, ComplementBool(bool))
        case Start(id, next) =>
          transitions += checkForReturnAndProduceTransition(id, next, ValueBool(true))
      }}
    Graph(transitions.toList, cfg.idToNode)
  }

  def getOutGoingEdges(id: String, graph: Graph): List[(String, Bool)] = {
    val lb = ListBuffer[(String, Bool)]()
    graph.graph.foreach { case (node, nextNode, condition) =>
      if (node == id) lb += ((nextNode, condition))
    }
    lb.toList
  }

  def removeNodeAndCreateNewGraph(id: String, graph: Graph): Graph = {
    val outGoingEdges = getOutGoingEdges(id, graph)
    var newTransitions = List[(String, String, Bool)]()
    newTransitions = graph.graph.filter{ case(node, nextNode, _) => node != id && nextNode != id }
    graph.graph.foreach{ case(node, nextNode, condition) =>
      if(nextNode == id) {
        newTransitions = newTransitions ++ outGoingEdges.map{ case(nextOutNode, nextCondition) => (node, nextOutNode, AndBool(condition, nextCondition))}
      }
    }
    Graph(newTransitions, graph.idToNode)
  }
  def makeSimplifiedGraph(cfg: ControlFlowGraphSinglePass): Graph = {
    val initialGraph = convertControlFlowGraph(cfg)
    val nodesToRemove = initialGraph.idToNode.keySet.toList.filter(id => id.contains("If") || id.contains("While") || id.contains("Ensure") )
    var tempGraph = initialGraph
    nodesToRemove.foreach{ node =>
      tempGraph = removeNodeAndCreateNewGraph(node, tempGraph)
    }
    val finalGraph = tempGraph
    finalGraph
  }

}
