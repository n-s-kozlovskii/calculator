import ExpressionTree.{Node, postorder, compute}
import RnpCalculator.{computeRpn, rpn}

object Test extends App {
  val test = Map(
    "(2+1)" -> 3.0,
    "(2-1)" -> 1.0,
    "(4*2)" -> 8.0,
    "(4/2)" -> 2.0,
    "(2*(1+2))" -> 6.0,
    "((2*1)+2)" -> 4.0,
    "((1+2)*(3+(4*5)))" -> 69.0,
    "((1+2)*(3+(40/5)))" -> 33.0,
    "((1020)*(3+(2-5)))" -> 0.0
  )

  println("rpn method")
    for ((expression, expected) <- test) {
      val result = computeRpn(rpn(expression.toCharArray.iterator))
      println(s"$expression=$result, expected $expected")
      assert(result - expected < 0.0000000000001)
    }

  println()
  println("expression tree method")
  for ((expression, expected) <- test) {
    val result = compute(Node.build(expression))
    println(s"$expression=$result, expected $expected")
    assert(result - expected < 0.0000000000001)
  }
}
