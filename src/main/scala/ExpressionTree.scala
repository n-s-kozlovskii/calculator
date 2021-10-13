import scala.annotation.tailrec
import scala.collection.mutable

object ExpressionTree extends App{
  case class Node(value: String, left: Option[Node], right: Option[Node]) {
    def isOperand: Boolean = left.isEmpty && right.isEmpty
  }
  object Node{
    def newNode(value: String): Node = Node(value, None, None)

    def build(str: String): Node  = {
      var operand = new StringBuilder
      var t: Node = null
      var t1,t2: Option[Node] = null
      val nodeStack = mutable.Stack.empty[Node]
      val operatorStack = mutable.Stack.empty[Char]
      val priority = Map(
        ')'  -> 0,
        '+' -> 1,
        '-' -> 1,
        '*' -> 2,
        '/' -> 2,
      )

      for (char <- str) {
        if (char == '(') {
          operatorStack.push(char)
        } else if (char.isDigit) {
          operand += char
        } else if (priority.getOrElse(char, 0) > 0) {
          if (operand.nonEmpty) {
            nodeStack.push(Node.newNode(operand.toString()))
            operand = new StringBuilder
          }

          while (operatorStack.nonEmpty && operatorStack.top != '(') {
            t1 = nodeStack.headOption
            t2 = nodeStack.headOption
            t = Node(operatorStack.pop().toString, t1, t2)
            nodeStack.push(t)
          }
          operatorStack.push(char)
        } else if (char == ')') {
          if (operand.nonEmpty) {
            nodeStack.push(Node.newNode(operand.toString()))
            operand = new StringBuilder
          }

          while (operatorStack.nonEmpty && operatorStack.top != '(') {
            if (nodeStack.size < 2) {throw new IllegalArgumentException("expression invalid" )}
            t1 = Some(nodeStack.pop())
            t2 = Some(nodeStack.pop())
            t = Node(operatorStack.pop().toString, t1, t2)
            nodeStack.push(t)
          }
          if (operatorStack.isEmpty) {throw new IllegalArgumentException("parenthesis unbalanced")}
          operatorStack.pop()
        }
      }

      if (operatorStack.nonEmpty)  {throw new IllegalArgumentException("parenthesis unbalanced")}
      nodeStack.pop()
    }
  }

  def compute(root: Node): Double = {
    root match {
      case Node(value, None, None) => value.toDouble
      case Node(value, Some(left), Some(right)) =>
        val x = compute(right)
        val y = compute(left)
        value match {
          case "*" => x * y
          case "/" => x / y
          case "+" => x + y
          case "-" => x - y
        }
    }
  }



  def postorder(node: Option[Node]): Unit = {
    node match {
      case None => print("")
      case Some(n) =>
        postorder(n.left)
        postorder(n.right)
        print(n.value)
        print(" ")
    }
  }

}
