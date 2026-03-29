package edu.luc.cs.laufer.cs371.expressions

enum Expr derives CanEqual:
  case Constant(value: Int)
  case Variable(name: String)
  case UMinus(expr: Expr)
  case Plus(left: Expr, right: Expr)
  case Minus(left: Expr, right: Expr)
  case Times(left: Expr, right: Expr)
  case Div(left: Expr, right: Expr)
  case Mod(left: Expr, right: Expr)

sealed trait Stmt derives CanEqual

case class ExprStmt(expr: Expr) extends Stmt
case class Assign(name: String, expr: Expr) extends Stmt
case class Cond(condition: Expr, thenBlock: Block, elseBlock: Block) extends Stmt
case class Loop(condition: Expr, body: Block) extends Stmt
case class Block(statements: List[Stmt]) extends Stmt
