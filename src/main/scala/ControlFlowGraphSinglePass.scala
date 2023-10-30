import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.immutable.HashMap
import scala.collection.mutable.{ListBuffer}

class ControlFlowGraphSinglePass() {
  var idToNode = HashMap[String, Node]()
  var idToStmt = HashMap[String, Statement]()
  var curNum = 1
  var transitions = HashMap[String, List[String]]()
  var stmtToID = HashMap[Statement, String]()
  var firstNode = ""
  var nestedConnections = Set[String]()
  var nestedIf = List[String]()
  var numToID = HashMap[Int, String]()

  def makeGraph(stmts: List[Statement], outerStmt: List[String]): String = {
    var stmt = new Statement()
    stmt = stmts(0)
    stmt match {
      case a@ReadInputStatement(id) =>
        val id = "Read" + curNum
        numToID += (curNum -> id)
        val first = if (curNum == 1) true else false
        curNum += 1
        idToStmt += (id -> a)
        stmtToID += (a -> id)
        var node = new Node()
        if (stmts.length > 1) {
          val nextStmt = makeGraph(stmts.slice(1, stmts.length), outerStmt) //Might not be right
          transitions += (id -> List(nextStmt))
          var node = Read(id, nextStmt)
          idToNode += (id -> node)
        }
        else {
          transitions += (id -> List())
          var node = Read(id, null)
          idToNode += (id -> node)
          nestedIf = nestedIf :+ id
        }
        if (first) {
          val idStart = "Start0"
          firstNode = idStart
          idToNode += (idStart -> Start(idStart, id))
          transitions += (idStart -> List(id))
        }
        return id
      case a@EnsureStatement(_, cond) =>
        val id = "Ensure" + curNum
        numToID += (curNum -> id)
        val first = if (curNum == 1) true else false
        curNum += 1
        idToStmt += (id -> a)
        stmtToID += (a -> id)
        var node = new Node()
        if (stmts.length > 1) {
          val nextStmt = makeGraph(stmts.slice(1, stmts.length), outerStmt) //Might not be right
          transitions += (id -> List(nextStmt))
          var node = Continue(id, cond, nextStmt)
          idToNode += (id -> node)
        }
        else {
          transitions += (id -> List())
          var node = Continue(id, cond, null)
          idToNode += (id -> node)
          nestedIf = nestedIf :+ id
        }
        if (first) {
          val idStart = "Start0"
          firstNode = idStart
          idToNode += (idStart -> Start(idStart, id))
          transitions += (idStart -> List(id))
        }
        return id
      case a@IfElseStatement(_, cond, ifStmts, elseStmts) =>
        val id = "IfElse" + curNum
        numToID += (curNum -> id)
        val first = if (curNum == 1) true else false
        stmtToID += (a -> id)
        curNum += 1
        idToStmt += (id -> a)
        val elseStmtValue = if (elseStmts.isEmpty) false else true
        var node = new Node()
        val nextIfStmt = makeGraph(ifStmts, outerStmt)
        var nextElseStmt = ""
        if (elseStmtValue) {
          nextElseStmt = makeGraph(elseStmts, outerStmt)
        }
          if (stmts.length > 1) {
          val nextStmt = makeGraph(stmts.slice(1, stmts.length), outerStmt) //Might not be right
          if (elseStmtValue) {
            transitions += (id -> List(nextIfStmt, nextElseStmt))
            node = createNode(a, Some(stmtToID(ifStmts(ifStmts.length - 1))))     // else statements need to be updated
            transitions += (stmtToID(elseStmts(elseStmts.length - 1)) -> List(nextStmt))
            idToNode += (stmtToID(elseStmts(elseStmts.length - 1)) -> createNode(elseStmts(elseStmts.length - 1), None))
            idToNode += (id -> node)
            transitions += (stmtToID(ifStmts(ifStmts.length - 1)) -> List(nextStmt))
            idToNode += (stmtToID(ifStmts(ifStmts.length - 1)) -> createNode(ifStmts(ifStmts.length - 1), None))
          }
          else {
            createNodeAndTransitions(id, nextIfStmt, Some(nextStmt), Some(stmtToID(ifStmts(ifStmts.length - 1))))
            createNodeAndTransitions(stmtToID(ifStmts(ifStmts.length - 1)), nextStmt, None, None)
          }
          //Handles connecting inner statements (that are the last statement in there list) to correct next statement.
          //If there is an outer while statement, it will connect to the while. If there is an inner if statement and no
          //While it will connect to the next statement
          var outerWhile = ""
          var nestedIfNums = HashMap[String, Int]();
            nestedIf.foreach({x =>
            var temp = ""
            for (c <- x.toCharArray) {
              if (c.isDigit) {
                temp += c
              }
            }
            nestedIfNums += (x -> temp.toInt)
          })
          var nextStmtNum = ""
            for (c <- nextStmt.toCharArray) {
              if (c.isDigit) {
                nextStmtNum += c
              }
            }
          val filterNestedIf = nestedIf.filter(x => nestedIfNums(x) < nextStmtNum.toInt)
          val newNestedIf = nestedIf.filter(x => nestedIfNums(x) >= nextStmtNum.toInt) //Almost works but doesn't work for read6 on 5.txt maybe instead of forall filter out all numbers that are less than or equal to the next stmt
          filterNestedIf.reverse.foreach({ n =>
            if (outerWhile.isEmpty) {
              createNodeAndTransitions(n, nextStmt, None, None)
            }
            else {
              createNodeAndTransitions(n, outerWhile, None, None)
            }
            if (n.contains("While")) {
              outerWhile = n
            }
          })
          nestedIf = newNestedIf
          }
        else {
            nestedIf = nestedIf :+ id
          transitions += (id -> List(nextIfStmt))
          var node = Branch(id, cond, nextIfStmt, null, None)
          idToNode += (id -> node)
        }
        if (first) {
          val idStart = "Start0"
          firstNode = idStart
          idToNode += (idStart -> Start(idStart, id))
          transitions += (idStart -> List(id))
        }
        id
      case a@WhileStatement(id, cond, whileStmts) =>
        val id = "While" + curNum
        numToID += (curNum -> id)
        val first = if (curNum == 1) true else false
        stmtToID += (a -> id)
        curNum += 1
        idToStmt += (id -> a)
        var node = new Node()
        val nextWhileStmt = makeGraph(whileStmts, outerStmt :+ id)
        if (stmts.length > 1) {
          if (!stmtToID(whileStmts.last).contains("If")) {
            createNodeAndTransitions(stmtToID(whileStmts.last), id, None, None)
          }
          else {
            createNodeAndTransitions(stmtToID(whileStmts.last), id, None, None)
            createNodeAndTransitions(numToID(curNum - 1), id, None, None)
          }
          val nextStmt = makeGraph(stmts.slice(1, stmts.length), outerStmt)
          createNodeAndTransitions(id, nextWhileStmt, Some(nextStmt), Some(stmtToID(whileStmts.last)))
          val firstWhile = nestedIf.findLast(x => x.contains("While"))
          if (!firstWhile.isEmpty) {
            createNodeAndTransitions(firstWhile.get, id, None, None)
          }
        }

        else {
          createNodeAndTransitions(id, nextWhileStmt, None, Some(stmtToID(whileStmts.last)))
          createNodeAndTransitions(stmtToID(whileStmts.last), id, None, None)
          nestedIf = nestedIf :+ id
        }
        if (first) {
          val idStart = "Start0"
          firstNode = idStart
          idToNode += (idStart -> Start(idStart, id))
          transitions += (idStart -> List(id))
        }
        id
      case a@ReturnStatement(id, cond) =>
        val id = "Return" + curNum
        val first = if (curNum == 1) true else false
        numToID += (curNum -> id)
        stmtToID += (a -> id)
        curNum += 1
        idToStmt += (id -> a)
        transitions += (id -> List(id))
        val node = createNode(a, None)
        idToNode += (id -> node)
        if (first) {
          val idStart = "Start0"
          firstNode = idStart
          idToNode += (idStart -> Start(idStart, id))
          transitions += (idStart -> List(id))
        }
        id
    }


    }
  def createNode(stmt: Statement, lastThenNode: Option[String]): Node = {
    stmt match {
      case a@IfElseStatement(_, cond, ifStmts, elseStmts) =>
        val trans = transitions(stmtToID(a)) // Might be wrong
        val next = if (trans.length == 1) null else trans(1)
        if (idToNode.contains(stmtToID(a)) && lastThenNode.isEmpty) {
          idToNode(stmtToID(a)) match {
            case Branch(id, cond, thenNode, elseNode, lastWhileNode) =>
              return Branch(stmtToID(a), cond, trans(0), next, lastWhileNode)
            case _ =>
          }
        }
        Branch(stmtToID(a), cond, trans(0), next, lastThenNode)
      case a@ReadInputStatement(_) =>
        val trans = transitions(stmtToID(a))
        transitions += (stmtToID(a) -> transitions(stmtToID(a)).slice(0, 1))
        val next = if (trans.isEmpty) null else trans(0)
        Read(stmtToID(a), next)
      case a@WhileStatement(id, cond, stmts) =>
        val trans = transitions(stmtToID(a))
        val next = if (trans.length == 1) null else trans(1)
        if (idToNode.contains(stmtToID(a)) && lastThenNode.isEmpty) {
          idToNode(stmtToID(a)) match {
            case Branch(id, cond, thenNode, elseNode, lastWhileNode) =>
              return Branch(stmtToID(a), cond, trans(0), next, lastWhileNode)
            case _ =>
          }
        }
        Branch(stmtToID(a), cond, trans(0), next, lastThenNode)
      case a@ReturnStatement(id, cond) =>
        Return(stmtToID(a), cond)
      case a@EnsureStatement(_, cond) =>
        val trans = transitions(stmtToID(a))
        val next = if (trans.isEmpty) null else trans(0)
        Continue(stmtToID(a), cond, next)
    }
  }
  def createNodeAndTransitions(id: String, firstTrans: String, secondTrans: Option[String], lastThenNode: Option[String]): Unit = {
    if((!transitions.contains(id) && id != firstTrans && id != secondTrans.getOrElse("None")) || (transitions(id).length < 2 && id != firstTrans && id != secondTrans.getOrElse("None")) ) {
      if (!id.contains("Return")) {
        if (secondTrans.isEmpty) {
          if (transitions.contains(id)) {
            transitions += (id -> (transitions(id) ++ List(firstTrans)))
          }
          else {
            transitions += (id -> List(firstTrans))
          }
        }
        else {
          transitions += (id -> List(firstTrans, secondTrans.get))
        }
      }
      idToNode += (id -> createNode(idToStmt(id), lastThenNode))
    }
  }

  def visualizeGraph(): File = {
    val file = new File("ControlFlowGraph.gv.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    var concatenate_graph = "digraph control_flow_graph {\n \tfontname=\"Helvetica,Arial,sans-serif\"\n" +
      "\tnode [fontname=\"Helvetica,Arial,sans-serif\"]\n" + "\tedge [fontname=\"Helvetica,Arial,sans-serif\"]\n" +
      "\trankdir=LR;\n" + "\tnode [shape = circle];\n"
    var hashmap = ""
    transitions.foreach { case (k, v) => v.slice(0, 2).foreach({
      s => hashmap += k + " -> " + s + "" + getLabel(k, s) + ";\n"})}
    bw.write(concatenate_graph + hashmap + "}")
    bw.close()
    file
  }

  def getLabel(nodeID: String, transitionID: String): String = {
    val node = idToNode(nodeID)
    node match {
      case Branch(id, cond, thenNode, elseNode, _) =>
        if (transitionID == thenNode) " [label = \""+ getLabelBool(cond) + "\"]" else " [label = \""+ getLabelBool(ComplementBool(cond)) + "\"]"
      case Continue(id, exp, nextNode) =>
        " [label = \""+ getLabelBool(exp) + "\"]"
      case _ =>
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


case class Return(id: String, expr: Bool) extends Node
case class Continue(id: String, exp: Bool, nextNode: String) extends Node
case class Read(id: String, nextNode: String) extends Node

case class Branch(id: String, cond: Bool, thenNode: String, elseNode: String, lastThenNode: Option[String]) extends Node

case class Start(id: String, nextNode: String) extends Node

class Node()


// Return nodes, Read nodes, Third kind everything else (reducable nodes, can be eliminated, Other)
// FOr each node kind = other: (n1)
//For each node