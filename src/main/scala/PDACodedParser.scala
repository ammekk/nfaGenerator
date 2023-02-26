import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

object PDACodedParser extends RegexParsers {
  private val state = "[a-zA-Z0-9_]+".r
  private val letter = "[a-zA-Z0-9_]+".r
  def pda: Parser[PDACoded] = rep( stateParse ) ^^ {case sts => PDACoded(sts)}
  def stateParse: Parser[StateCoded] = state ~ ":" ~ rep(statement)~opt(accepted)  ^^
    //"{" ~ rep(statement)~opt(accepted) <~ "}" ^^
    {case a => a match { case st~_~stmt~Some(acpt) => StateCoded(st, stmt, acpt)
                         case st~_~stmt~None => StateCoded(st, stmt, false)}}
  def statement: Parser[Statement] = opt("if") ~> opt("("~>condition <~ ")") ~ opt("then") ~"{" ~ action <~ "}" ^^
    { case cond~_~_~act => Statement(cond, act)}
  def condition: Parser[Condition] = opt("read" ~> letter) ~ opt("and") ~ opt("pop" ~> letter) ^^
    { case read~_~pop => Condition(read, pop)}
  def action: Parser[Action] = opt("push" ~> letter <~ ";") ~ opt("spawn" ~> state <~ ";") ~ opt("loop;") ^^
    { case push~spawn~loop =>
    val ifLoop = loop.getOrElse(None)
    val toLoop = if (ifLoop == None) false else true
  Action(push, spawn, toLoop)}
  def accepted: Parser[Boolean] = "return " ~> ("true" |  "false" ) <~ ";" ^^
    { case a => a.toBoolean}


  def apply(code: String): Either[ParserError, PDACoded] = {
    parseAll(pda, new CharSequenceReader(code)) match {
      case Success(result, next) => Right(result)
      case NoSuccess(msg, next) => Left(ParserError(msg))
    }
  }
  case class ParserError(msg: String)
}

case class StateCoded(name: String, statements: List[Statement], accepted: Boolean) //rejected: Boolean
case class Action(push: Option[String], dst: Option[String], loop: Boolean)
case class Condition(read: Option[String], pop: Option[String])
case class Statement(condition: Option[Condition], action: Action)
case class PDACoded(states: List[StateCoded])
