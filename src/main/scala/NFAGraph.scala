import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable.{HashMap, ListBuffer}

class NFAGraph(val domain: List[String]) {
  val graph = new MinimizedGraph(domain)
  var transitions = HashMap[String, List[(String, Option[NFAEdge])]]()
  var prunedTransitions = HashMap[String, List[(String, Option[NFAEdge])]]()
  var firstNFANode = ""
  var idToNode = HashMap[String, NFANode]()
  var visitedNodes = Set[String]()
  var acceptingNodes = Set[String]()
  var nonReturnNodes = Set[String]()


  def nfa(stmts: List[Statement]): Unit = {
    graph.makeControlFlowAndSimplifiedGraph(stmts)
    graph.minimizeGraph(graph.getFirstSimplifiedNode())
    graph.visualizeGraph()
    makeNFA(graph.idToNode(graph.firstMinimizedNode))
    deleteIsland()
    visualizeGraph()
  }

  def makeNFA(node: MinimizedNode): Unit = {
    node match {
      case MinimizedStartNode(oldID, nextNode) =>
        val id = getNumbers(oldID)
        visitedNodes += id
        val connectionsList = nextNode.map({ case (nextID, bool) =>
          (getNumbers(nextID), returnEdge(bool))
        })
        firstNFANode = id
        transitions += (id -> connectionsList)
        idToNode += (id -> NFAStartNode(id, connectionsList))
        nextNode.foreach({ case (nextID, _) =>
          if (!visitedNodes.contains(getNumbers(nextID))) {
            makeNFA(graph.idToNode(nextID))
          }
        })
        nonReturnNodes += id
      case MinimizedReadNode(oldID, nextNode) =>
        val id = getNumbers(oldID)
        visitedNodes += id
        val eof = nextNode.exists({ case (_, bool) => checkForEOF(bool)})
        val connectionsList = nextNode.map({ case (nextID, bool) =>
          (getNumbers(nextID), returnEdge(bool))
        })
        val connectionsListFiltered = connectionsList.filter({ case (_, edge) =>
          edge.get match {
            case FalseEdge() =>
              false
            case _ =>
              true
          }})
        transitions += (id -> connectionsListFiltered)
        if (eof) {
          idToNode += (id -> NFAReturnNode(id, connectionsListFiltered))
          acceptingNodes += id
        }
        else{
          idToNode += (id -> NFAReadNode(id, connectionsListFiltered))
          nonReturnNodes += id
        }
        nextNode.foreach({ case (nextID, _) =>
          if (!visitedNodes.contains(getNumbers(nextID))) {
            makeNFA(graph.idToNode(nextID))
          }
        })
      case MinimizedReturnNode(oldID, nextNode) =>
        val id = getNumbers(oldID)
        visitedNodes += id
        val connectionsList = nextNode.map({ case (nextID, bool) =>
          (getNumbers(nextID), returnEdge(bool))
        })
        idToNode += (id -> NFAReturnNode(id, connectionsList))
        transitions += (id -> connectionsList)
        acceptingNodes += id
    }
  }

  def deleteIsland(): Unit = {
    var seenNodes = Set[String]()
    def findPath(node: String): Unit = {
      seenNodes += node
      prunedTransitions += (node -> transitions(node))
      transitions(node).foreach({ case (id, _) =>
        if (!seenNodes.contains(id)) findPath(id)})
    }
    findPath(firstNFANode)
    acceptingNodes = acceptingNodes.intersect(seenNodes)
    nonReturnNodes = nonReturnNodes.intersect(seenNodes)
  }

  def getNumbers(id: String): String = {
    var newID = ""
    id.foreach({ s =>
      if (s.isDigit) newID += s
    })
    newID
  }
  def checkForEOF(bool: Option[Bool]): Boolean = {
    if (bool.isEmpty) return false
    bool.get match {
      case EqualityBool(l, r) =>
        MinimizeFunctions.evaluateExpression(l) == "EOF" || MinimizeFunctions.evaluateExpression(r) == "EOF"
      case _ =>
        false
    }
  }
  def returnEdge(bool: Option[Bool]): Option[NFAEdge] = {
    if (bool.isEmpty) {
      return Some(EpsilonEdge())
    }
    bool.get match {
      case EqualityBool(l, r) =>
        val input = getEqualityValue(l, r)
        if (input.isEmpty) return Some(FalseEdge())
        Some(InputEdge(List(input.get)))
      case a@OrBool(l, r) =>
        val input = getOrValue(a)
        if (input.isEmpty) Some(FalseEdge()) else Some(InputEdge(input))
      case ValueBool(value) =>
        Some(FalseEdge())
      case _ =>
        None
    }
  }

  def getOrValue(bool: Bool): List[String] = {
    bool match {
      case EqualityBool(l, r) =>
        val input = getEqualityValue(l, r)
        if (input.isEmpty) return List()
        List(input.get)
      case OrBool(l, r) =>
        val left = getOrValue(l)
        val right = getOrValue(r)
        left ++ right
      case _ =>
        List()
    }
  }
  def getEqualityValue(l: Expression, r: Expression): Option[String] = {
    val left = MinimizeFunctions.evaluateExpression(l)
    val right = MinimizeFunctions.evaluateExpression(r)
    if (left == "input") {
      if (right == "EOF") None else Some(right)
    }
    else if (left == "EOF") None else Some(right)
  }

  def visualizeGraph(): File = {
    val file = new File("NFA.gv.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    var concatenate_graph = "digraph control_flow_graph {\n \tfontname=\"Helvetica,Arial,sans-serif\"\n" +
      "\tnode [fontname=\"Helvetica,Arial,sans-serif\"]\n" + "\tedge [fontname=\"Helvetica,Arial,sans-serif\"]\n" +
      "\trankdir=LR;\n" + "\tnode [shape = circle];" + nonReturnNodes.mkString(" ") + ";\n" +
      "node [shape = doublecircle]; " + acceptingNodes.mkString(" ") + ";\n"
    var hashmap = ""
    prunedTransitions.foreach { case (k, v) => v.foreach({ case (state, edge) =>
      hashmap += k + " -> " + state + "" + getLabel(edge) + ";\n"
    })
      if (v.isEmpty) {
        hashmap += (k + ";\n")
      }
    }
    bw.write(concatenate_graph + hashmap + "}")
    bw.close()
    file
  }

  def getLabel(edge: Option[NFAEdge]): String = {
    if (edge.isEmpty) {
      return ""
    }
    else {
      " [label = \"" + getLabelEdge(edge.get) + "\"]"
    }
  }

  def getLabelEdge(edge: NFAEdge): String = {
    edge match {
      case EpsilonEdge() =>
        "ε"
      case InputEdge(input) =>
        input.mkString(",")
    }
  }
}
class NFAEdge

case class InputEdge(input: List[String]) extends NFAEdge

case class EpsilonEdge() extends NFAEdge

case class FalseEdge() extends NFAEdge

class NFANode

case class NFAStartNode(id: String, nextNode: List[(String, Option[NFAEdge])]) extends NFANode
case class NFAReadNode(id: String, nextNode: List[(String, Option[NFAEdge])]) extends NFANode
case class NFAReturnNode(id: String, nextNode: List[(String, Option[NFAEdge])]) extends NFANode