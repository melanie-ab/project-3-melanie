package edu.luc.cs.laufer.cs371.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import Expr.*

trait ExprParser extends JavaTokenParsers:

  given [A, B](using CanEqual[A, A], CanEqual[B, B]): CanEqual[A ~ B, A ~ B] =
    CanEqual.derived

  def expr: Parser[Expr] =
    term ~ rep(("+" | "-") ~ term) ^^ {
      case first ~ rest =>
        rest.foldLeft(first) {
          case (left, "+" ~ right) => Plus(left, right)
          case (left, "-" ~ right) => Minus(left, right)
          case (left, _ ~ right)   => left
        }
    }

  def term: Parser[Expr] =
    factor ~ rep(("*" | "/" | "%") ~ factor) ^^ {
      case first ~ rest =>
        rest.foldLeft(first) {
          case (left, "*" ~ right) => Times(left, right)
          case (left, "/" ~ right) => Div(left, right)
          case (left, "%" ~ right) => Mod(left, right)
          case (left, _ ~ right)   => left
        }
    }

  def factor: Parser[Expr] =
    ident ^^ { s => Variable(s) } |
      wholeNumber ^^ { s => Constant(s.toInt) } |
      "+" ~> factor ^^ { e => e } |
      "-" ~> factor ^^ { e => UMinus(e) } |
      "(" ~> expr <~ ")"

  def statement: Parser[Stmt] =
    assignment |
      conditional |
      loop |
      block |
      expr <~ ";" ^^ { e => ExprStmt(e) }

  def assignment: Parser[Stmt] =
    ident ~ ("=" ~> expr) <~ ";" ^^ {
      case name ~ value => Assign(name, value)
    }

  def conditional: Parser[Stmt] =
    ("if" ~> "(" ~> expr <~ ")") ~ block ~ opt("else" ~> elsePart) ^^ {
      case condition ~ thenBlock ~ elseOpt =>
        Cond(condition, thenBlock, elseOpt.getOrElse(Block(Nil)))
    }

  def elsePart: Parser[Block] =
    conditional ^^ { stmt => Block(List(stmt)) } |
      block

  def loop: Parser[Stmt] =
    ("while" ~> "(" ~> expr <~ ")") ~ block ^^ {
      case condition ~ body => Loop(condition, body)
    }

  def block: Parser[Block] =
    "{" ~> rep(statement) <~ "}" ^^ { stmts => Block(stmts) }

  def repl: Parser[List[Stmt]] =
    phrase(rep(statement))

end ExprParser
