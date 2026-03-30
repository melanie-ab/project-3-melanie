package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite

class TestCombinatorParser extends AnyFunSuite:

  def parse(input: String) =
    ASTBuilder.parseAll(ASTBuilder.repl, input)

  // The valid tests

  test("assignment parses") {
    assert(parse("x = 5;").successful)
  }

  test("multiple assignments parse") {
    assert(parse("x = 5; y = 7;").successful)
  }

  test("expression statement parses") {
    assert(parse("3 + 4 - 5;").successful)
  }

  test("assignment with expression parses") {
    assert(parse("x = (1 + 2) * 3;").successful)
  }

  test("if without else parses") {
    assert(parse("if (1) { x = 2; }").successful)
  }

  test("if with else parses") {
    assert(parse("if (1) { x = 2; } else { x = 3; }").successful)
  }

  test("while loop parses") {
    assert(parse("while (y) { x = x + 1; }").successful)
  }

  test("block parses") {
    assert(parse("{ x = 1; y = 2; }").successful)
  }

  test("nested structures parse") {
    val input = "if (1) { while (y) { x = x + 1; } }"
    assert(parse(input).successful)
  }

  // If the test then becomes a Invalid tests

  test("missing semicolon fails") {
    assert(!parse("x = 5").successful)
  }

  test("bad assignment fails") {
    assert(!parse("= 5;").successful)
  }

  test("if missing parentheses fails") {
    assert(!parse("if 1 { x = 2; }").successful)
  }

  test("while missing block fails") {
    assert(!parse("while (1) x = 2;").successful)
  }
    test("else if parses") {
    assert(parse("if (1) { x = 2; } else if (3) { x = 4; }").successful)
  }

  test("else if else parses") {
    assert(parse("if (1) { x = 2; } else if (3) { x = 4; } else { x = 5; }").successful)
  }

end TestCombinatorParser
