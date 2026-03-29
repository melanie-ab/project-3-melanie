package edu.luc.cs.laufer.cs371.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import Expr.*
import Stmt.*

trait ExprParser extends JavaTokenParsers:

  /**
   * Enable missing typesafe equality for `~`.
   * TODO remove once the combinator parser library provides this.
   */
  given [A, B](using CanEqual[A, A], CanEqual[B, B]): CanEqual[A ~ B, A ~ B] =
    CanEqual.derived

  /** expression ::= term { { "+" | "-" } term }* */
  def expr: Parser[Expr] =
    term ~ rep(("+" | "-") ~ term) ^^ {
      case first ~ rest =>
        rest.foldLeft(first) {
          case (left, "+" ~ right) => Plus(left, right)
          case (left, "-" ~ right) => Minus(left, right)
        }
    }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] =
    factor ~ rep(("*" | "/" | "%") ~ factor) ^^ {
      case first ~ rest =>
        rest.foldLeft(first) {
          case (left, "*" ~ right) => Times(left, right)
          case (left, "/" ~ right) => Div(left, right)
          case (left, "%" ~ right) => Mod(left, right)
        }
    }

  /**
   * factor ::= ident | number | "+" factor | "-" factor | "(" expression ")"
   */
  def factor: Parser[Expr] =
    ident ^^ { s => Variable(s) } |
    wholeNumber ^^ { s => Constant(s.toInt) } |
    "+" ~> factor ^^ { e => e } |
    "-" ~> factor ^^ { e => UMinus(e) } |
    "(" ~> expr <~ ")"

  /**
   * statement ::= expression ";" | assignment | conditional | loop | block
   */
  def statement: Parser[Stmt] =
    assignment |
    conditional |
    loop |
    block |
    expr <~ ";" ^^ { e => ExprStmt(e) }

  /** assignment ::= ident "=" expression ";" */
  def assignment: Parser[Stmt] =
    ident ~ ("=" ~> expr) <~ ";" ^^ {
      case name ~ value => Assign(name, value)
    }

  /** conditional ::= "if" "(" expression ")" block [ "else" block ] */
  def conditional: Parser[Stmt] =
    ("if" ~> "(" ~> expr <~ ")") ~ block ~ opt("else" ~> block) ^^ {
      case condition ~ thenBlock ~ elseBlockOpt =>
        Cond(condition, thenBlock, elseBlockOpt.getOrElse(Block(Nil)))
    }

  /** loop ::= "while" "(" expression ")" block */
  def loop: Parser[Stmt] =
    ("while" ~> "(" ~> expr <~ ")") ~ block ^^ {
      case condition ~ body => Loop(condition, body)
    }

  /** block ::= "{" statement* "}" */
  def block: Parser[Block] =
    "{" ~> rep(statement) <~ "}" ^^ { stmts => Block(stmts) }

  /** repl ::= statement* */
  def repl: Parser[List[Stmt]] =
    phrase(rep(statement))

end ExprParser
