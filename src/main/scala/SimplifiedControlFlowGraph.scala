import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable.{HashMap, ListBuffer}

class SimplifiedControlFlowGraph {
  var nodeNum = 1
  var transitions = HashMap[String, List[(String, Option[Bool])]]()
  var graph = new ControlFlowGraphSinglePass()
  var idToNode = HashMap[String, SimplifiedNode]()
  var firstSimplifiedNode = ""
  var traversedNodes = Set[String]()
  var stmtIdtoNodeId = HashMap[String, String]()
  def makeControlFlowGraph(stmts: List[Statement]): Unit = {
    graph.makeGraph(stmts, List())
    graph.visualizeGraph()

  }
  def getFirstControlFlowNode(): Node = {
    graph.idToNode(graph.firstNode)

  }
  def simplifyGraph(node: Node): List[(String, Option[Bool])] = {
    node match {
      case Start(id, nextNode) =>
        val idStart = "Start0"
        traversedNodes += id
        if (nextNode != null && !stmtIdtoNodeId.contains(id)) {
          stmtIdtoNodeId += (id -> idStart)
          val connections = simplifyGraph(graph.idToNode(nextNode))
          transitions += (idStart -> connections)
          val node = SimplifiedStartNode(idStart, connections)
          idToNode += (idStart -> node)
          firstSimplifiedNode = idStart
        }
        List((idStart, None))
      case Read(id, nextNode) =>
        val idRead = if (stmtIdtoNodeId.contains(id)) stmtIdtoNodeId(id) else "Read" + nodeNum
        if (!stmtIdtoNodeId.contains(id)){
          nodeNum += 1
        }
        traversedNodes += id
        if (nextNode != null && !stmtIdtoNodeId.contains(id)) {
          stmtIdtoNodeId += (id -> idRead)
          val connections = simplifyGraph(graph.idToNode(nextNode))
          transitions += (idRead -> connections)
          val node = SimplifiedReadNode(idRead, connections)
          idToNode += (idRead -> node)
        }
        List((idRead, None))
      case Branch(id, cond, thenNode, elseNode, lastThenNode) =>
        var connectionThen = List[(String, Option[Bool])]()
        var connectionElse = List[(String, Option[Bool])]()
        if (id.contains("IfElse")) {
          connectionThen = simplifyGraph(graph.idToNode(thenNode))
          connectionThen = connectionThen.map({ case (id, bool) =>
            if (bool.isEmpty) {
              (id, Some(cond))
            }
            else {
              (id, Some(AndBool(bool.get, cond)))
            }
          })
          if (elseNode != null) {
            connectionElse = simplifyGraph(graph.idToNode(elseNode))
            connectionElse = connectionElse.map({ case (id, bool) =>
              if (bool.isEmpty) {
                (id, Some(ComplementBool(cond)))
              }
              else {
                (id, Some(AndBool(bool.get, ComplementBool(cond)))) // Not sure if right
              }
            })
          }
         // transitions += (stmtIdtoNodeId(lastThenNode.get) -> connectionElse)
        }
        if (id.contains("While") && !traversedNodes.contains(id + "_fin")) { // Not sure exactly what to do here
          if (traversedNodes.contains(id)) {                                 // if it only propagates up the first time
            traversedNodes += id + "_fin"                                    // then it does not connect to the inner part of the while
          }
          traversedNodes += id
          connectionThen = simplifyGraph(graph.idToNode(thenNode))
          connectionThen = connectionThen.map({ case (id, bool) =>
            if (bool.isEmpty) {
              ((id, Some(cond)))
            }
            else {
              (id, Some(AndBool(bool.get, cond)))
            }
          })
          if (elseNode != null) {
            connectionElse = simplifyGraph(graph.idToNode(elseNode))
            connectionElse = connectionElse.map({ case (id, bool) =>
              if (bool.isEmpty) {
                (id, Some(ComplementBool(cond)))
              }
              else {
                (id, Some(AndBool(bool.get, ComplementBool(cond)))) // Not sure if right
              }
            })
          }
          if (stmtIdtoNodeId.contains(thenNode)) {
            val connectionlastThenNode = List((stmtIdtoNodeId(thenNode), Some(cond))) ++ connectionElse
            if (!lastThenNode.isEmpty && stmtIdtoNodeId.contains(lastThenNode.get)) {
              transitions += (stmtIdtoNodeId(lastThenNode.get) -> connectionlastThenNode)
            }
          }
        }
        connectionElse ++ connectionThen
      case Continue(id, cond, nextNode) =>
        var connections = List[(String, Option[Bool])]()
        if (nextNode != null) {
          val connection = simplifyGraph(graph.idToNode(nextNode))
          connections = connection.map({ case (id, bool) =>
            if (bool.isEmpty) {
              (id, Some(cond))
            }
            else {
              (id, Some(AndBool(bool.get, cond)))
            }
          })
        }
        connections
      case Return(id, expr) =>
        val idReturn = if (stmtIdtoNodeId.contains(id)) stmtIdtoNodeId(id) else "Return" + nodeNum
        nodeNum += 1
        if(!stmtIdtoNodeId.contains(id)) {
          val node = SimplifiedReturnNode(idReturn)
          idToNode += (idReturn -> node)
          stmtIdtoNodeId += (id -> idReturn)
          transitions += (idReturn -> List((idReturn, Some(ValueBool(true)))))
        }
        List((idReturn, Some(expr)))
    }
  }
  def visualizeGraph(): File = {
    val file = new File("SimplifiedGraph.gv.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    var concatenate_graph = "digraph control_flow_graph {\n \tfontname=\"Helvetica,Arial,sans-serif\"\n" +
      "\tnode [fontname=\"Helvetica,Arial,sans-serif\"]\n" + "\tedge [fontname=\"Helvetica,Arial,sans-serif\"]\n" +
      "\trankdir=LR;\n" + "\tnode [shape = circle];\n"
    var hashmap = ""
    transitions.foreach { case (k, v) => v.foreach({ case (state, bool) => hashmap += k + " -> " + state + "" + getLabel(bool) + ";\n" })
    if(v.isEmpty) {
      hashmap += (k + ";\n")
    }}
    bw.write(concatenate_graph + hashmap + "}")
    bw.close()
    file
  }
  def getLabel(bool: Option[Bool] ): String = {
   if (bool.isDefined) {
     " [label = \"" + getLabelBool(bool.get) + "\"]"
   }
   else {
     ""
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


class SimplifiedNode
case class SimplifiedStartNode(id: String, nextNode: List[(String, Option[Bool])]) extends SimplifiedNode
case class SimplifiedReadNode(id: String, nextNode: List[(String, Option[Bool])]) extends SimplifiedNode
case class SimplifiedReturnNode(id: String) extends SimplifiedNode