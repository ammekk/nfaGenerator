import scala.collection.mutable.ListBuffer
import scala.io.Source

object Main extends App{
  def parseNFA(code: String): Seq[NFAParsed] = {
    val parsed = Parser(code)
    parsed.left.foreach(error => println(s"\'$code\' parsing error: $error"))
    //parsed.toSeq.foreach(expr => println(expr))
    parsed.toSeq
  }
  def readFile(file: String): String = {
    val source = Source.fromFile(file)
    source.getLines().mkString("")
  }
  def getDomain(domainArg: String): List[String] = {
    domainArg.substring(1, domainArg.length() - 1).split(',').toList
  }
  val parsed = parseNFA(readFile(args(0)))(0)
  val domain = getDomain(args(1))
  val graph = new ControlFlowGraphSinglePass()
  graph.makeGraph(parsed.statements, List())
  val simplifiedGraph = Simplify.makeSimplifiedGraph(graph)
  simplifiedGraph.visualizeGraph()
  //val graph = new NFAGraph(domain)
  //graph.nfa(parsed.statements)
  //toJsonFile.nfa(graph)





}