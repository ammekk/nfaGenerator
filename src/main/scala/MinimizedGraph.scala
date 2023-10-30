import MinimizeFunctions.isEOF

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable.{HashMap, ListBuffer}

class MinimizedGraph(val domain: List[String]) {
  var transitions = HashMap[String, List[(String, Option[Bool])]]()
  var firstMinimizedNode = ""
  var idToNode = HashMap[String, MinimizedNode]()
  val graph = new SimplifiedControlFlowGraph()
  var visitedNodes = Set[String]()

  def makeControlFlowAndSimplifiedGraph(stmts: List[Statement]): Unit = {
    graph.makeControlFlowGraph(stmts)
    graph.simplifyGraph(graph.getFirstControlFlowNode())
    graph.visualizeGraph()
  }
  def getFirstSimplifiedNode(): SimplifiedNode = {
    graph.idToNode(graph.firstSimplifiedNode)
  }
  def minimizeGraph(node: SimplifiedNode): Unit = {
    node match {
      case SimplifiedStartNode(id, nextNode) =>
        visitedNodes += id
        val connectionsList = nextNode.map({ case(id, bool) =>
          (id, getMinimizedBool(bool))
        })
        firstMinimizedNode = id
        transitions += (id -> connectionsList)
        idToNode += (id -> MinimizedStartNode(id, connectionsList))
        nextNode.foreach({ case(id, _) =>
          if (!visitedNodes.contains(id)) {
            minimizeGraph(graph.idToNode(id))
          }})
      case SimplifiedReadNode(id, nextNode) =>
        visitedNodes += id
        val connectionsList = nextNode.map({ case (id, bool) =>
          (id, getMinimizedBool(bool))
        })
        transitions += (id -> connectionsList)
        idToNode += (id -> MinimizedReadNode(id, connectionsList))
        nextNode.foreach({ case (id, _) =>
          if (!visitedNodes.contains(id)) {
            minimizeGraph(graph.idToNode(id))
          }
        })
      case SimplifiedReturnNode(id) =>
        visitedNodes += id
        val bool = getMinimizedBool(Some(ValueBool(true)))
        transitions += (id -> List((id, bool)))
        idToNode += (id -> MinimizedReturnNode(id, List((id, bool))))
    }
  }

  def getMinimizedBool(bool: Option[Bool]): Option[Bool] = {
    val boolList = new ListBuffer[Bool]()
    if (bool.isDefined) {
      val newBool = MinimizeFunctions.processFlipCoins(bool.get)
      domain.foreach ({
        l =>
          if (MinimizeFunctions.contains(l, newBool)) {
            boolList += EqualityBool(InputExpression(), StringExpression(l))
          }

      })
      if (MinimizeFunctions.isEOF(newBool)){
        boolList += EqualityBool(InputExpression(), EOFExpression())
      }
      if (boolList.isEmpty){
        boolList += ValueBool(false)
      }
    }
    if (boolList.length > 1) {
      Some(boolList.slice(1, boolList.length).fold(boolList(0))((b1, b2) => OrBool(b1, b2)))
    }
    else if (boolList.length == 1) {
      Some(boolList(0))
    }
    else {
      None
    }
  }

  def visualizeGraph(): File = {
    val file = new File("MinimizedGraph.gv.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    var concatenate_graph = "digraph control_flow_graph {\n \tfontname=\"Helvetica,Arial,sans-serif\"\n" +
      "\tnode [fontname=\"Helvetica,Arial,sans-serif\"]\n" + "\tedge [fontname=\"Helvetica,Arial,sans-serif\"]\n" +
      "\trankdir=LR;\n" + "\tnode [shape = circle];\n"
    var hashmap = ""
    transitions.foreach { case (k, v) => v.foreach({ case (state, bool) =>
      hashmap += k + " -> " + state + "" + getLabel(bool) + ";\n" })
      if (v.isEmpty) {
        hashmap += (k + ";\n")
      }
    }
    bw.write(concatenate_graph + hashmap + "}")
    bw.close()
    file
  }

  def getLabel(bool: Option[Bool]): String = {
    if (bool.isEmpty) {
      return ""
    }
    else {
      " [label = \"" + getLabelBool(bool.get) + "\"]"
    }
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

class MinimizedNode
case class MinimizedStartNode(id: String, nextNode: List[(String, Option[Bool])]) extends MinimizedNode
case class MinimizedReadNode(id: String, nextNode: List[(String, Option[Bool])]) extends MinimizedNode
case class MinimizedReturnNode(id: String, nextNode: List[(String, Option[Bool])]) extends MinimizedNode
