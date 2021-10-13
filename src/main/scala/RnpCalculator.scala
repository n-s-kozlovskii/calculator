import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.collection.mutable.Seq

/**
 * Задача “Калькулятор”
 *
 * Написать консольное REPL приложение, позволяющее вычислять арифметические выражения вида
 * (1 + 2) * (3 + (4 * 5))
 * Требования:
 * Приложение считывает строки одна за другой из stdin
 * Каждая строка - одно выражение или пустая строка
 * После считывания выражения приложение выводит результат вычисления в stdout
 * Приложение должно корректно обрабатывать ошибки
 * (продолжать корректную работу при вводе некорректной строки)
 * Выражение:
 * Состоит из бинарных операций + - * /
 * Операнды - целые числа
 * Каждая бинарная операция заключена в круглые скобки: ((1+2)*(3+(4*5)))
 * Может содержать пробелы: (1 + ( 2  ))
 *
 * Язык программирования - Scala
 * Отсутствие сторонних библиотек
 * (за исключением тестовых библиотек, если возникнет желание использовать таковые)
 */


object RnpCalculator{

  def computeRpn(exp: Array[String]): Double = {
    val operators = Set("*", "/", "-", "+")
    val stack = mutable.Stack.empty[String]
    var x, y: Double = 0
    for (token <- exp) {
      if (operators contains token) {
        if (stack.length < 2) {
          throw new IllegalArgumentException("rpn malformed")
        }
        y = stack.pop().toDouble
        x = stack.pop().toDouble
        token match {
          case "*" =>
            stack.push((x*y).formatted("%.5f"))
          case "/" =>
            stack.push((x/y).formatted("%.5f"))
          case "-" =>
            stack.push((x-y).formatted("%.5f"))
          case "+" =>
            stack.push((x+y).formatted("%.5f"))
        }
      } else {
        stack.push(token)
      }
    }
    stack.pop().toDouble
  }

  def rpn(a: Iterator[Char]): Array[String] = {
    val buffer = new StringBuilder()
    val stack = mutable.Stack.empty[Char]


    while (a.hasNext) {
      val token = a.next()
      if (token == '-' || token == '+') {
        while (stack.nonEmpty && (stack.top == '*' || stack.top == '/')) {
          buffer += ' '
          buffer += stack.pop()
        }
        buffer += ' '
        stack.push(token)
      } else if (token == '*' || token == '/') {
        buffer += ' '
        stack.push(token)
      } else if (token == '(') {
        stack.push(token)
      } else if (token == ')') {
        while (stack.nonEmpty && stack.top != '(') {
          buffer += ' '
          buffer += stack.pop()
        }
        if (stack.isEmpty) {
          throw new IllegalArgumentException("parenthesis are unbalanced")
        }
        stack.pop()
      } else {
        buffer += token
      }
    }
    if (stack.nonEmpty) {
      throw new IllegalArgumentException
    }
    buffer.toString().split(" ")
  }
}
