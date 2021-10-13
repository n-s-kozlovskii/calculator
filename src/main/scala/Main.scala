import RnpCalculator.{computeRpn, rpn}

import scala.io.StdIn

object Main extends App{
  var input = ""
  println("enter expression to calculate or :q to quit")
  input = StdIn.readLine()
  while (input != ":q") {
    try {
      println(s"$input = ${computeRpn(rpn(input.toCharArray.iterator))}")
    } catch {
      case e: Exception => println("somehow you expression didn't pass. Try again")
    }
    println("enter expression to calculate or :q to quit")
    input = StdIn.readLine()
  }
}
