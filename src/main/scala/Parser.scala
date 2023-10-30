import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

object Parser extends RegexParsers {
  private val outputChar = "[A-Za-z0-9_]".r // Want to have regex that matches any non whitespace character but can't find one that works
  private val outputNumera = "[0-9]".r

  def nfa: Parser[NFAParsed] = rep(statement) ^^ { case sts => NFAParsed(sts) }

  def statement: Parser[Statement] = {
    val readInput = "input = read();" ^^ { case _ => ReadInputStatement(java.util.UUID.randomUUID.toString) }
    val ensure = "ensure(" ~> bool ~ ")" <~ ";" ^^ { case bl~_ => EnsureStatement(java.util.UUID.randomUUID.toString, bl)}
    val ifElse = "if(" ~ bool ~ ")" ~ "{" ~ rep(statement) ~ "}" ~ opt("else"~> "{" ~ rep(statement) ~ "}") ^^ //Should a space between the if and condition be legal? Same goes for while
                  { case _ ~ ifBool ~ _ ~ _~ ifStmts ~ _ ~ Some(_ ~ elseStmts ~ _) => IfElseStatement(java.util.UUID.randomUUID.toString, ifBool, ifStmts, elseStmts)
                   case _ ~ ifBool ~ _ ~ _ ~ ifStmts ~ _ ~ None => IfElseStatement(java.util.UUID.randomUUID.toString, ifBool, ifStmts, List()) }
    val whileStmt = "while(" ~ orBool ~ ")" ~ "{" ~ rep(statement) ~ "}" ^^ { case _~cond~_~_~stmts~_ => WhileStatement(java.util.UUID.randomUUID.toString, cond, stmts) }
    val returnStmt = "return" ~ orBool ~";" ^^ { case _ ~ returnBool ~ _ => ReturnStatement(java.util.UUID.randomUUID.toString, returnBool) }
    readInput | ensure | ifElse | whileStmt | returnStmt
  }

  def orBool: Parser[Bool] = {
    val or = andBool ~ "or" ~ orBool ^^ { case l ~ _ ~ r => OrBool(l, r)} //Somethings wrong with and and or
    val parenBool = "(" ~> orBool <~ ")" ^^ { case pb => ParenBool(pb) }
    val complement = "not" ~> orBool ^^ { case not => ComplementBool(not) }
    or | andBool | parenBool | complement
  }
  def andBool: Parser[Bool] = {
    val and = bool ~ "and" ~ andBool ^^ { case l ~ _ ~ r => AndBool(l, r)}
    and | bool
  }

  def bool: Parser[Bool] = {
    val trueValue = "true" ^^ { case value => ValueBool(value.toBoolean)}
    val falseValue = "false" ^^ { case value => ValueBool(value.toBoolean) }
    val flipCoin = "flip_coin()" ^^ { case flpcn => FlipCoinBool() }
    val equality = expression ~ "==" ~ expression ^^ { case l ~_~ r => EqualityBool(l, r)}
    trueValue | falseValue | flipCoin | equality  // or | and
  }

  def expression: Parser[Expression] = {
    val charExp = "'" ~> outputChar <~ "'" ^^ { case charValue => CharExpression(charValue.toCharArray.head) }
    val numExp = outputNumera ^^ { case numValue => NumericExpression(numValue.toInt)}
    val inputExp = "input" ^^ { case _ => InputExpression() }
    val EOFExp = "EOF" ^^ { case _ => EOFExpression() }
    val StringExp = "\"" ~> outputChar <~ "\"" ^^ { case stringValue => StringExpression(stringValue) }
    charExp | numExp | inputExp | EOFExp
  }


  def apply(code: String): Either[ParserError, NFAParsed] = {
    parseAll(nfa, new CharSequenceReader(code)) match {
      case Success(result, next) => Right(result)
      case NoSuccess(msg, next) => Left(ParserError(msg))
    }
  }
  case class ParserError(msg: String)
}

case class InputExpression() extends Expression
case class CharExpression(value: Char) extends Expression
case class StringExpression(value: String) extends Expression
case class NumericExpression(value: Int) extends Expression
case class EOFExpression() extends Expression
class Expression()
case class ParenBool(value: Bool) extends Bool
case class ComplementBool(value: Bool) extends Bool
case class OrBool(lhs: Bool, rhs: Bool) extends Bool
case class AndBool(lhs: Bool, rhs: Bool) extends Bool
case class EqualityBool(lhs: Expression, rhs: Expression) extends Bool
case class FlipCoinBool() extends Bool
case class ValueBool(value: Boolean) extends Bool
class Bool()
case class WhileStatement(id: String,  cond: Bool, stmts: List[Statement]) extends Statement
case class IfElseStatement(id: String, cond: Bool, ifStmts: List[Statement], elseStmts: List[Statement]) extends Statement
case class EnsureStatement(id: String, cond: Bool) extends Statement
case class ReadInputStatement(id: String) extends Statement
case class ReturnStatement(id: String, cond: Bool) extends Statement
 class Statement()
case class NFAParsed(statements: List[Statement])
