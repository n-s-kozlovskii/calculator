import RnpCalculator.{computeRpn, rpn}

object Main extends App {
  val b = "((1+2)*(3+(40/5)))".toCharArray.iterator
  val rpnStr = rpn(b)
  println(rpnStr mkString " ")
  val result = computeRpn(rpnStr)
  println(result)
}
