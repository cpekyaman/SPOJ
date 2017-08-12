package spoj.classical

import scala.collection.mutable
import scala.io.StdIn

/**
  * Created by raistlin on 8/11/2017.
  */
object TransformExpression extends App {
  private val rightAssocOperators = Set.empty[Char]
  private val leftAssocOperators = Set('+', '-', '*', '/', '^')

  private val stack = new mutable.Stack[Char]

  private def precedence(c: Char): Int = {
    c match {
      case '+' | '-'  => 1
      case '*' | '/'  => 2
      case '^'        => 3
      case _          => 0
    }
  }

  private def isRightAssociative(c: Char): Boolean = rightAssocOperators.contains(c)
  private def isLeftAssociative(c: Char): Boolean = leftAssocOperators.contains(c)

  private def canPushOperator(c: Char): Boolean = {
    val currentPrec = precedence(c)
    val headPrec = precedence(stack.top)

    currentPrec > headPrec || (currentPrec == headPrec && isRightAssociative(c))
  }

  private def consumeTillLeftParen(): Unit = {
    while(stack.top != '(') {
      print(stack.pop())
    }
    stack.pop()
  }

  private def shouldConsumeOperatorInStack(c: Char):Boolean = {
    val currentPrec = precedence(c)
    val isLeftAssoc = isLeftAssociative(c)
    val topPrec = precedence(stack.top)

    currentPrec < topPrec || (currentPrec == topPrec && isLeftAssoc)
  }

  private def isOperator(c: Char): Boolean = leftAssocOperators.contains(c) || rightAssocOperators.contains(c)
  private def isPrintedToken(c: Char): Boolean = Character.isAlphabetic(c) || Character.isWhitespace(c)

  // Shunting Yard
  private def processToken(c: Char): Unit = {
    if(isPrintedToken(c)) {
      print(c)
    } else if(c == '(') {
      stack.push(c)
    } else if(c == ')') {
      consumeTillLeftParen()
    } else if (isOperator(c)) {
      processOperator(c)
    }
  }

  private def processOperator(c: Char) = {
    if (stack.isEmpty || stack.top == '(') {
      stack.push(c)
    } else if (canPushOperator(c)) {
      stack.push(c)
    } else {
      while (stack.nonEmpty && shouldConsumeOperatorInStack(c)) {
        print(stack.pop())
      }
      stack.push(c)
    }
  }

  val cases = StdIn.readInt()
  for(t <- 1 to cases) {
    val expression = StdIn.readLine()

    stack.clear()
    for(c <- expression.toCharArray) {
      processToken(c)
    }
    stack.foreach(print)
    println()
  }
}
