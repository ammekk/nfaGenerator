import scala.collection.mutable.ListBuffer

object Minimize {
  def minimizeExtendedBooleanNFA(eBNFA: ExtendedBooleanNFA, alphabet: List[String]): ExtendedBooleanNFA = {
    val connectionsWithoutFlipCoin = eBNFA.connections.map{ case(node, connection, condition) =>
      (node, connection, processFlipCoins(condition))
    }
    val connectionsWithoutFalseEdges = connectionsWithoutFlipCoin.filter{ case(_, _, condition) =>
    !isEOFOrFalse(condition)}
    val connectionsWithInstantiatedAlphabet = connectionsWithoutFalseEdges.map{ case(node, connection, condition) =>
      val boolList = new ListBuffer[Bool]()
      alphabet.foreach({ l =>
        if(contains(l, condition)) boolList += EqualityBool(InputExpression(), StringExpression(l))
      })
      if (boolList.length > 1) (node, connection, boolList.slice(1, boolList.length).fold(boolList.head)((b1, b2) => OrBool(b1, b2)))
      else  (node, connection, boolList.head)
    }
    // Forgot to deal with accepting nodes
    ExtendedBooleanNFA(connectionsWithInstantiatedAlphabet, eBNFA.idToNode, eBNFA.startNodes)
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
        contains(x, l) && contains(x, r)
      case ValueBool(b) =>
        b
      case _ =>
        true
    }
  }

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

  def isEOFOrFalse(exp: Bool): Boolean = {
    exp match {
      case EqualityBool(l, r) =>
        val left = evaluateExpression(l)
        val right = evaluateExpression(r)
        if (left == "input") right == "EOF" else left == "EOF"
      case OrBool(l, r) =>
        // Used to be || but this should only evaluate to true if both are true
        isEOFOrFalse(l) && isEOFOrFalse(r)
      case ParenBool(b) =>
        isEOFOrFalse(b)
      case ComplementBool(b) =>
        !isEOFOrFalse(b)
      case AndBool(l, r) =>
        // Used to be && but this should evaluate to true if either are true
        isEOFOrFalse(l) || isEOFOrFalse(r)
      case ValueBool(b) =>
        !b
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
