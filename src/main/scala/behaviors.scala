package edu.luc.cs.laufer.cs371.expressions

import util.Try

import Expr.*

object behaviors:

  // Expression evaluation only for now.
  // Variables are not interpreted yet in project 3a.
  private def evaluateR(e: Expr): Int = e match
    case Constant(c) => c
    case Variable(name) =>
      throw new UnsupportedOperationException(
        s"cannot evaluate variable $name without an environment"
      )
    case UMinus(r)   => -evaluateR(r)
    case Plus(l, r)  => evaluateR(l) + evaluateR(r)
    case Minus(l, r) => evaluateR(l) - evaluateR(r)
    case Times(l, r) => evaluateR(l) * evaluateR(r)
    case Div(l, r)   => evaluateR(l) / evaluateR(r)
    case Mod(l, r)   => evaluateR(l) % evaluateR(r)

  def evaluate(e: Expr): Try[Int] = Try(evaluateR(e))

  def size(e: Expr): Int = e match
    case Constant(_) => 1
    case Variable(_) => 1
    case UMinus(r)   => 1 + size(r)
    case Plus(l, r)  => 1 + size(l) + size(r)
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r)   => 1 + size(l) + size(r)
    case Mod(l, r)   => 1 + size(l) + size(r)

  def height(e: Expr): Int = e match
    case Constant(_) => 1
    case Variable(_) => 1
    case UMinus(r)   => 1 + height(r)
    case Plus(l, r)  => 1 + math.max(height(l), height(r))
    case Minus(l, r) => 1 + math.max(height(l), height(r))
    case Times(l, r) => 1 + math.max(height(l), height(r))
    case Div(l, r)   => 1 + math.max(height(l), height(r))
    case Mod(l, r)   => 1 + math.max(height(l), height(r))

  import org.json4s.JsonAST.JValue
  import org.json4s.JsonDSL.*

  def toJson(e: Expr): JValue = e match
    case Constant(c) =>
      ("Constant" -> c)
    case Variable(name) =>
      ("Variable" -> name)
    case UMinus(expr) =>
      ("UMinus" -> toJson(expr))
    case Plus(left, right) =>
      ("Plus" -> List(toJson(left), toJson(right)))
    case Minus(left, right) =>
      ("Minus" -> List(toJson(left), toJson(right)))
    case Times(left, right) =>
      ("Times" -> List(toJson(left), toJson(right)))
    case Div(left, right) =>
      ("Div" -> List(toJson(left), toJson(right)))
    case Mod(left, right) =>
      ("Mod" -> List(toJson(left), toJson(right)))

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
    case Constant(c) =>
      c.toString
    case Variable(name) =>
      name
    case UMinus(expr) =>
      s"(-${unparse(expr)})"
    case Plus(left, right) =>
      s"(${unparse(left)} + ${unparse(right)})"
    case Minus(left, right) =>
      s"(${unparse(left)} - ${unparse(right)})"
    case Times(left, right) =>
      s"(${unparse(left)} * ${unparse(right)})"
    case Div(left, right) =>
      s"(${unparse(left)} / ${unparse(right)})"
    case Mod(left, right) =>
      s"(${unparse(left)} % ${unparse(right)})"

  private def indent(depth: Int): String =
    "  " * depth

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
      s"""{
${indent(depth)}}"""
    else
      val body = block.statements.map(stmt => unparse(stmt, depth + 1)).mkString("\n")
      s"""{
$body
${indent(depth)}}"""

  def unparse(program: List[Stmt]): String =
    program.map(stmt => unparse(stmt, 0)).mkString("\n")

end behaviors
