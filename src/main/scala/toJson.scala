import scala.collection.mutable.ListBuffer
import upickle.default._

import java.io.{BufferedWriter, File, FileWriter}


object toJsonFile {

  def nfa(nfa: NFAGraph): Unit = {
    var edges = ListBuffer[Edge]()
    nfa.prunedTransitions.foreach({case (src, trans) => trans.foreach({case (dst, edge) =>
      edge.get match {
        case InputEdge(input) =>
          input.foreach(char => edges += Edge(src, dst, char))
        case EpsilonEdge() =>
          edges += Edge(src, dst, null)
      }
    val nfaObject = NFA(nfa.firstNFANode, nfa.acceptingNodes.toList, edges.toList)
    val json = write(nfaObject)
    val file = new File("nfa.json")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(json)
    bw.close()
    }) })



  }

  case class NFA(start_state: String, accepted_states: List[String], edges: List[Edge])
  case class Edge(src: String, dst: String, char: String)

  implicit val nfarw: ReadWriter[NFA] = macroRW[NFA]

  implicit val edgerw: ReadWriter[Edge] = macroRW[Edge]

}
