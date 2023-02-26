import scala.io.Source

object Main extends App{
  def makePDA(code: String): Seq[PDACoded] = {
    val parsed = PDACodedParser(code)
    parsed.left.foreach(error => println(s"\'$code\' parsing error: $error"))
    parsed.toSeq.foreach(expr => println(expr))
    parsed.toSeq
  }
  def readFile(file: String): String = {
    val source = Source.fromFile(file)
    source.getLines().mkString("")
  }
  val pdaCoded = makePDA(readFile(args(0)))(0)
  val pda = Translate.translate(pdaCoded)
 // print(pda)

}