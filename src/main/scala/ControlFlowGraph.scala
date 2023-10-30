//import scala.collection.IterableOnce.iterableOnceExtensionMethods
//import scala.collection.mutable.{HashMap, ListBuffer}
//import java.io._
//class ControlFlowGraph {
//  var transitions = HashMap[String, List[String]]()
//  var numToID = HashMap[Int, String]()
//  var stmtToID = HashMap[Statement, String]()
//  var idToStmt = HashMap[String, Statement]()
//  val idToNode = HashMap[String, Node]()
//  var numOfNode = 0
//  var curNumNode = 0
//  var startNode: Option[Node] = None
//  var seenPaths = Set[Node]()
//  var traversal = ListBuffer[List[Node]]()
//  //val idToNode = HashMap[String, Node]()
//
//  def makeID(stmts: List[Statement]): Unit = {
//    stmts.foreach( stmt =>
//      stmt match {
//        case a@IfElseStatement(_, cond, ifStmts, elseStmts) =>
//        val nodeID = "IfElse" + numOfNode
//        stmtToID += (a -> nodeID)
//        numToID += (numOfNode -> nodeID)
//        idToStmt += (nodeID -> a)
//        numOfNode += 1
//        makeID(ifStmts)
//        makeID(elseStmts)
//        case a@ReadInputStatement(_) =>
//          val nodeID = "Read" + numOfNode
//          stmtToID += (a -> nodeID)
//          numToID += (numOfNode -> nodeID)
//          idToStmt += (nodeID -> a)
//          numOfNode += 1
//        case a@ReturnStatement(id, cond) =>
//          val nodeID = "Return" + numOfNode
//          stmtToID += (a -> nodeID)
//          numToID += (numOfNode -> nodeID)
//          idToStmt += (nodeID -> a)
//          numOfNode += 1
//        case a@WhileStatement(id, cond, stmts) =>
//            val nodeID = "While" + numOfNode
//            stmtToID += (a -> nodeID)
//            numToID += (numOfNode -> nodeID)
//            idToStmt += (nodeID -> a)
//            numOfNode += 1
//            makeID(stmts)
//
//      })
//  }
//
//  def makeGraph( stmts: List[Statement]): HashMap[String, List[String]] = {
//    //For each statement make node and track transitions,
//    // return hashmap where key is node identifier and value is transitions
//    stmts.zipWithIndex.foreach( { case(stmt,index) => {
//      stmt match {
//        case a@IfElseStatement(_, cond, ifStmts, elseStmts) =>
//          //Link to first if statement and else statement. If no else statement, link to next statement
//        //While on this level might have to link last line of if and else to next statement
//          val elseEmpty = if (elseStmts.isEmpty) true else false
//          transitions += (stmtToID(a) -> List(stmtToID(ifStmts(0))))
//          if (!elseEmpty) {
//            transitions(stmtToID(a)) = transitions(stmtToID(a)) :+ stmtToID(elseStmts(0))
//          }
//          val curNode = stmtToID(a)
//          var first = false
//          if (curNumNode == 0) {
//            first = true
//          }
//          curNumNode += 1
//          makeGraph(ifStmts)
//          val lastIfNode = numToID(curNumNode - 1)
//          if (!elseEmpty) {
//            curNumNode += 1
//            makeGraph(elseStmts)
//          }
//          val lastNode = numToID(curNumNode - 1)
//          val lastlastNode = numToID(curNumNode - 2)
//          if ( stmts.length - 1 > index && !lastNode.contains("Return")) {
//            transitions += (lastNode -> List(stmtToID(stmts(index + 1))))
//            if (!elseEmpty) {
//               transitions += (lastlastNode -> List(stmtToID(stmts(index + 1))))
//              idToNode += (lastlastNode -> createNode(idToStmt(lastlastNode)))
//            }
//            if (lastIfNode != lastNode) {
//              transitions += (lastIfNode -> List(lastNode))
//              idToNode += (lastIfNode -> createNode(idToStmt(lastIfNode)))
//            }
//            println(idToNode)
//            idToNode += (lastNode -> createNode(idToStmt(lastNode)))
//         }
//          if (numToID.contains(curNumNode)) {
//             transitions(curNode) = transitions(curNode) :+ numToID(curNumNode)
//          }
//          val node = createNode(a)
//          if (first) {
//            startNode = Some(node)
//          }
//          idToNode += (curNode -> node)
//        case a@ReadInputStatement(_) =>
//          if (index < stmts.length - 1) {
//            transitions += (stmtToID(a) -> List(stmtToID(stmts(index + 1))))
//          }
//          else {
//            transitions += (stmtToID(a)-> List())
//          }
//            val node = createNode(a)
//            if (curNumNode == 0) {
//              startNode = Some(node)
//            }
//            idToNode += (stmtToID(a) -> node)
//          curNumNode += 1
//        case a@WhileStatement(id, cond, stmts) =>
//          transitions += (stmtToID(a) -> List(stmtToID(stmts(0))))
//          val curNode = stmtToID(a)
//          val first = if (curNumNode == 0) false else true
//          curNumNode += 1
//          makeGraph(stmts)
//          val lastNode = numToID(curNumNode - 1)
//          if ( stmts.length - 1 > index && !lastNode.contains("Return")) {
//            transitions += (lastNode -> List(stmtToID(a)))
//            val node = createNode(idToStmt(lastNode))
//            idToNode += (lastNode -> node)
//          }
//          if (numToID.contains(curNumNode)) {
//             transitions(curNode) = transitions(curNode) :+ numToID(curNumNode)
//          }
//          val node = createNode(a)
//          if (first) {
//            startNode = Some(node)
//          }
//          idToNode += (curNode -> node)
//        case a@ReturnStatement(id, cond) =>
//          transitions += (stmtToID(a) -> List(stmtToID(stmts(index + 1))))
//           val node = createNode(a)
//           if (curNumNode == 0) {
//             startNode = Some(node)
//           }
//           idToNode += (stmtToID(a) -> node)
//           curNumNode += 1
//
//      }
//    }})
//    transitions
//  }
//  def createNode(stmt: Statement): Node = {
//    stmt match {
//      case a@IfElseStatement(_, cond, ifStmts, elseStmts) =>
//        val trans = transitions(stmtToID(a))                     // Might be wrong
//        val next = if (trans.length == 1) null else trans(1)
//        Branch(stmtToID(a), cond, trans(0), next)
//      case a@ReadInputStatement(_) =>
//        val trans = transitions(stmtToID(a))
//        val next = if (trans.isEmpty) null else trans(0)
//        Read(stmtToID(a), next)
//      case a@WhileStatement(id, cond, stmts) =>
//        val trans = transitions(stmtToID(a))
//        val next = if (trans.length == 1) null else trans(1)
//        Branch(stmtToID(a), cond, trans(0), next)
//      case a@ReturnStatement(id, cond) =>
//        Return(stmtToID(a), cond)
//
//
//    }
//  }
//
//  def traverseGraph(node: Node, lb: ListBuffer[Node]): Unit = {
//    node match {
//      case a@Branch(id, cond, thenNode, elseNode) =>
//        if (!seenPaths.contains(idToNode(thenNode))) {
//          seenPaths += idToNode(thenNode)
//          lb += a
//          traverseGraph(idToNode(thenNode), lb)
//          traverseGraph(startNode.get, ListBuffer[Node]())
//        }
//        else if(elseNode != null && !seenPaths.contains(idToNode(elseNode))) {
//          seenPaths += idToNode(elseNode)
//          lb += a
//          traverseGraph(idToNode(elseNode), lb)
//        }
//        else if (elseNode == null) {
//          lb += a
//          val list : List[Node] = lb.toList
//          traversal += list
//        }
//      case a@Read(id, nextNode) =>
//        lb += a
//        if (nextNode != null) {
//          traverseGraph(idToNode(nextNode), lb)
//        }
//        else {
//          val list = lb.toList
//          traversal += list
//        }
//
//    }
//
//  }
//
//
//
//
//  def visualizeGraph(): File = {
//  val file = new File("graph.gv.txt")
//  val bw = new BufferedWriter(new FileWriter(file))
//  var concatenate_graph = "digraph control_flow_graph {\n \tfontname=\"Helvetica,Arial,sans-serif\"\n" +
//    "\tnode [fontname=\"Helvetica,Arial,sans-serif\"]\n" + "\tedge [fontname=\"Helvetica,Arial,sans-serif\"]\n" +
//    "\trankdir=LR;\n" + "\tnode [shape = circle];\n"
//  var hashmap = ""
//  transitions.foreach { case (k, v) => v.foreach ( s => hashmap += k + " -> " + s + "[label = \"SS(B)\"];\n")}
//  bw.write(concatenate_graph + hashmap + "}")
//  bw.close()
//  file
//}
//
//}





