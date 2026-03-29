package edu.luc.cs.laufer.cs371.expressions

object Calculator:

  def processInput(input: String): Unit =
    println("You entered: " + input)
    val result = ASTBuilder.parseAll(ASTBuilder.repl, input)
    if result.isEmpty then
      println("This input could not be parsed")
    else
      import org.json4s.native.JsonMethods.{pretty, render}
      import behaviors.*

      val program = result.get

      println("The parsed statements are:")
      program.foreach(println)

      println("The corresponding JSON structure is:")
      if program.length == 1 then
        println(pretty(render(toJson(program.head))))
      else
        println(pretty(render(toJson(Block(program)))))

      println("The unparsed statements are:")
      println(unparse(program))

  def main(args: Array[String]): Unit =
    if args.length > 0 then
      processInput(args.mkString(" "))
    else
      print("minijs> ")
      scala.io.Source.stdin.getLines().foreach:
        line =>
          processInput(line)
          print("minijs> ")

end Calculator
