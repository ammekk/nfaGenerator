import scala.collection.mutable.ListBuffer

object MinimizeFunctions {
  def processFlipCoins(bool: Bool): Bool = {
    val newBoolList = replaceFlipCoins(bool)
    if (newBoolList.length > 1) {
      val parenNewBoolList = newBoolList.map(b => ParenBool(b))
      var firstBool = parenNewBoolList(0)
      val finalOrBool = parenNewBoolList.slice(1, parenNewBoolList.length).fold(firstBool.asInstanceOf[Bool])((b1, b2) => OrBool(b1, b2))
      finalOrBool
    }
    else {
      newBoolList(0)
    }
  }

  private def replaceFlipCoins(bool: Bool): List[Bool] = {
    bool match {
      case OrBool(l, r) =>
        val listLeft = replaceFlipCoins(l)
        val listRight = replaceFlipCoins(r)
        val result = ListBuffer[Bool]()
        listLeft.foreach {
          (bl =>
            listRight.foreach {
              (br =>
                result += OrBool(bl, br))
            })
        }
        result.toList
      case ParenBool(b) =>
        val list = replaceFlipCoins(b)
        list.map(x => ParenBool(x))
      case AndBool(l, r) =>
        val listLeft = replaceFlipCoins(l)
        val listRight = replaceFlipCoins(r)
        val result = ListBuffer[Bool]()
        listLeft.foreach {
          (bl =>
            listRight.foreach {
              (br =>
                result += AndBool(bl, br))
            })
        }
        result.toList
      case FlipCoinBool() =>
        List(ValueBool(true), ValueBool(false))
      case ComplementBool(b) =>
        val list = replaceFlipCoins(b)
        list.map(x => ComplementBool(x))
      case x@_ =>
        List(x)

    }
  }

  def contains(x: String, exp: Bool): Boolean = {
    exp match {
      case EqualityBool(l, r) =>
        val left = evaluateExpression(l)
        val right = evaluateExpression(r)
        if (left == "input") right == x else left == x
      case OrBool(l, r) =>
        contains(x, l) || contains(x, r)
      case ParenBool(b) =>
        contains(x, b)
      case ComplementBool(b) =>
        !contains(x, b)
      case AndBool(l, r) =>
        contains (x, l) && contains (x, r)
      case ValueBool(b) =>
        b
      case _ =>
        true
    }
  }

  def isEOF(exp: Bool): Boolean = {
    exp match {
      case EqualityBool(l, r) =>
        val left = evaluateExpression(l)
        val right = evaluateExpression(r)
        if (left == "input") right == "EOF" else left == "EOF"
      case OrBool(l, r) =>
        isEOF(l) || isEOF(r)
      case ParenBool(b) =>
        isEOF(b)
      case ComplementBool(b) =>
        !isEOF(b)
      case AndBool(l, r) =>
        val left = isEOF(l)
        val right = isEOF(r)
        isEOF(l) && isEOF(r)
      case ValueBool(b) =>
        false
      case _ =>
        false
    }
  }


 def evaluateExpression(exp: Expression): String = {
    exp match {
      case InputExpression() => "input"
      case CharExpression(v) => v.toString
      case NumericExpression(v) => v.toString
      case EOFExpression() => "EOF"
      case StringExpression(v) => v
    }
  }
}
