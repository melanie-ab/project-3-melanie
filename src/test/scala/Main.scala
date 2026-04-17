package edu.luc.cs.laufer.cs371.expressions

import util.{ Success, Failure }
import org.scalatest.funsuite.AnyFunSuite

import behaviors.*
import TestFixtures.*

object Main:
  def main(args: Array[String]): Unit =
    process("p", complex1)
    process("q", complex2)
    process("f", bad)

  def process(n: String, e: Expr): Unit =
    println(s"$n = $e")
    println(s"evaluate($n) = ${evaluate(e)}")
    println(s"size($n) = ${size(e)}")
    println(s"height($n) = ${height(e)}")

end Main

class Test extends AnyFunSuite:
  test("evaluate(p)") { assert(evaluate(complex1).get == -1) }
  test("size(p)") { assert(size(complex1) == 9) }
  test("height(p)") { assert(height(complex1) == 4) }
  test("evaluate(q)") { assert(evaluate(complex2).get == 0) }
  test("size(q)") { assert(size(complex2) == 10) }
  test("height(q)") { assert(height(complex2) == 5) }
  test("evaluate(bad)") { assert(evaluate(bad).isFailure) }
  test("assignment creates variable and returns Num(0)") {
    memory.clear()
    val program = List(Assign("x", Constant(3)))
    assert(evaluate(program).get == Value.Num(0))
    assert(memory("x") == Value.Num(3))
  }

  test("block returns last value") {
    memory.clear()
    val program = List(
      Block(List(
        Assign("x", Constant(7)),
        ExprStmt(Variable("x"))
      ))
    )
    assert(evaluate(program).get == Value.Num(7))
    assert(memory("x") == Value.Num(7))
  }

  test("while loop multiplies by repeated addition") {
    memory.clear()
    val program = List(
      Assign("x", Constant(2)),
      Assign("y", Constant(3)),
      Assign("r", Constant(0)),
      Loop(
        Variable("y"),
        Block(List(
          Assign("r", Plus(Variable("r"), Variable("x"))),
          Assign("y", Minus(Variable("y"), Constant(1)))
        ))
      )
    )
    assert(evaluate(program).get == Value.Num(0))
    assert(memory("x") == Value.Num(2))
    assert(memory("y") == Value.Num(0))
    assert(memory("r") == Value.Num(6))
  }

  test("conditional true branch") {
    memory.clear()
    val program = List(
      Assign("x", Constant(1)),
      Cond(
        Variable("x"),
        Block(List(ExprStmt(Constant(10)))),
        Block(List(ExprStmt(Constant(20))))
      )
    )
    assert(evaluate(program).get == Value.Num(10))
  }

  test("conditional false branch") {
    memory.clear()
    val program = List(
      Assign("x", Constant(0)),
      Cond(
        Variable("x"),
        Block(List(ExprStmt(Constant(10)))),
        Block(List(ExprStmt(Constant(20))))
      )
    )
    assert(evaluate(program).get == Value.Num(20))
  }

  test("unknown variable fails") {
    memory.clear()
    val program = List(ExprStmt(Variable("z")))
    assert(evaluate(program).isFailure)
  }
end Test
