package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite

import Expr.*
import behaviors.*

class Main extends AnyFunSuite:

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

  test("while loop computes multiplication via addition") {
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

  test("unknown variable should fail") {
    memory.clear()
    val program = List(ExprStmt(Variable("z")))
    assert(evaluate(program).isFailure)
  }

end Main
