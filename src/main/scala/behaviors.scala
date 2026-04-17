package edu.luc.cs.laufer.cs371.expressions

import util.Try
import scala.collection.mutable
import Expr.*

object behaviors:

  enum Value derives CanEqual:
    case Num(value: Int)

  type Memory = mutable.Map[String, Value]

  val memory: Memory = mutable.Map.empty

  private def asInt(v: Value): Int = v match
    case Value.Num(n) => n

  private def evaluateExprR(e: Expr): Value = e match
    case Constant(c) => Value.Num(c)

    case Variable(name) =>
      memory.getOrElse(name, throw new NoSuchFieldException(name))

    case UMinus(r) =>
      Value.Num(-asInt(evaluateExprR(r)))

    case Plus(l, r) =>
      Value.Num(asInt(evaluateExprR(l)) + asInt(evaluateExprR(r)))

    case Minus(l, r) =>
      Value.Num(asInt(evaluateExprR(l)) - asInt(evaluateExprR(r)))

    case Times(l, r) =>
      Value.Num(asInt(evaluateExprR(l)) * asInt(evaluateExprR(r)))

    case Div(l, r) =>
      Value.Num(asInt(evaluateExprR(l)) / asInt(evaluateExprR(r)))

    case Mod(l, r) =>
      Value.Num(asInt(evaluateExprR(l)) % asInt(evaluateExprR(r)))

  private def evaluateStmtR(s: Stmt): Value = s match
    case ExprStmt(expr) =>
      evaluateExprR(expr)

    case Assign(name, expr) =>
      val value = evaluateExprR(expr)
      memory(name) = value
      Value.Num(0)

    case Cond(condition, thenBlock, elseBlock) =>
      if asInt(evaluateExprR(condition)) != 0 then
        evaluateBlockR(thenBlock)
      else
        evaluateBlockR(elseBlock)

    case Loop(condition, body) =>
      while asInt(evaluateExprR(condition)) != 0 do
        evaluateBlockR(body)
      Value.Num(0)

    case b: Block =>
      evaluateBlockR(b)

  private def evaluateBlockR(block: Block): Value =
    block.statements match
      case Nil => Value.Num(0)
      case stmts =>
        var result: Value = Value.Num(0)
        for stmt <- stmts do
          result = evaluateStmtR(stmt)
        result

  def evaluate(e: Expr): Try[Int] =
    Try(asInt(evaluateExprR(e)))

  def evaluate(stmt: Stmt): Try[Value] =
    Try(evaluateStmtR(stmt))

  def evaluate(program: List[Stmt]): Try[Value] =
    Try {
      var result: Value = Value.Num(0)
      for stmt <- program do
        result = evaluateStmtR(stmt)
      result
    }

  def size(e: Expr): Int = e match
    case Constant(_) => 1
    case Variable(_) => 1
    case UMinus(r) => 1 + size(r)
    case Plus(l, r) => 1 + size(l) + size(r)
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r) => 1 + size(l) + size(r)
    case Mod(l, r) => 1 + size(l) + size(r)

  def height(e: Expr): Int = e match
    case Constant(_) => 1
    case Variable(_) => 1
    case UMinus(r) => 1 + height(r)
    case Plus(l, r) => 1 + math.max(height(l), height(r))
    case Minus(l, r) => 1 + math.max(height(l), height(r))
    case Times(l, r) => 1 + math.max(height(l), height(r))
    case Div(l, r) => 1 + math.max(height(l), height(r))
    case Mod(l, r) => 1 + math.max(height(l), height(r))

  import org.json4s.JsonAST.JValue
  import org.json4s.JsonDSL.*

  def toJson(e: Expr): JValue = e match
    case Constant(c) => ("Constant" -> c)
    case Variable(name) => ("Variable" -> name)
    case UMinus(expr) => ("UMinus" -> toJson(expr))
    case Plus(left, right) => ("Plus" -> List(toJson(left), toJson(right)))
    case Minus(left, right) => ("Minus" -> List(toJson(left), toJson(right)))
    case Times(left, right) => ("Times" -> List(toJson(left), toJson(right)))
    case Div(left, right) => ("Div" -> List(toJson(left), toJson(right)))
    case Mod(left, right) => ("Mod" -> List(toJson(left), toJson(right)))

  def toJson(s: Stmt): JValue = s match
    case ExprStmt(expr) =>
      ("ExprStmt" -> toJson(expr))
    case Assign(name, expr) =>
      ("Assign" -> ("name" -> name) ~ ("expr" -> toJson(expr)))
    case Cond(condition, thenBlock, elseBlock) =>
      ("Cond" ->
        ("condition" -> toJson(condition)) ~
        ("thenBlock" -> toJson(thenBlock)) ~
        ("elseBlock" -> toJson(elseBlock)))
    case Loop(condition, body) =>
      ("Loop" ->
        ("condition" -> toJson(condition)) ~
        ("body" -> toJson(body)))
    case b: Block =>
      toJson(b)

  def toJson(b: Block): JValue =
    ("Block" -> b.statements.map(toJson))

  def unparse(e: Expr): String = e match
    case Constant(c) => c.toString
    case Variable(name) => name
    case UMinus(expr) => s"(-${unparse(expr)})"
    case Plus(left, right) => s"(${unparse(left)} + ${unparse(right)})"
    case Minus(left, right) => s"(${unparse(left)} - ${unparse(right)})"
    case Times(left, right) => s"(${unparse(left)} * ${unparse(right)})"
    case Div(left, right) => s"(${unparse(left)} / ${unparse(right)})"
    case Mod(left, right) => s"(${unparse(left)} % ${unparse(right)})"

  private def indent(depth: Int): String = " " * depth

  def unparse(stmt: Stmt, depth: Int = 0): String = stmt match
    case ExprStmt(expr) =>
      s"${indent(depth)}${unparse(expr)};"
    case Assign(name, expr) =>
      s"${indent(depth)}$name = ${unparse(expr)};"
    case Cond(condition, thenBlock, elseBlock) =>
      s"${indent(depth)}if (${unparse(condition)}) ${unparse(thenBlock, depth)} else ${unparse(elseBlock, depth)}"
    case Loop(condition, body) =>
      s"${indent(depth)}while (${unparse(condition)}) ${unparse(body, depth)}"
    case b: Block =>
      s"${indent(depth)}${unparse(b, depth)}"

  def unparse(block: Block, depth: Int): String =
    if block.statements.isEmpty then
      s"""{ ${indent(depth)}}"""
    else
      val body = block.statements.map(stmt => unparse(stmt, depth + 1)).mkString("\n")
      s"""{ $body
${indent(depth)}}"""

  def unparse(program: List[Stmt]): String =
    program.map(stmt => unparse(stmt, 0)).mkString("\n")

end behaviors
