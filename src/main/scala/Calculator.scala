package edu.luc.cs.laufer.cs371.expressions

object Calculator:

  def processInput(input: String): Unit =
    println("Memory: " + behaviors.memory)
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

      println("It evaluates to " + evaluate(program))
      println("Memory: " + memory)


  def main(args: Array[String]): Unit =
    import org.jline.reader.{EndOfFileException, LineReaderBuilder, UserInterruptException}
    import org.jline.terminal.TerminalBuilder

    if args.length > 0 then
      processInput(args.mkString(" "))
    else
      val terminal = TerminalBuilder.builder().system(true).build()
      val reader = LineReaderBuilder.builder().terminal(terminal).build()

      try
        while true do
          val inputBuilder = new StringBuilder
          var reading = true

          while reading do
            val prompt =
              if inputBuilder.isEmpty then "minijs> "
              else "... "

            val line = reader.readLine(prompt)

            if line.trim.isEmpty then
              reading = false
            else
              inputBuilder.append(line).append("\n"): Unit

          val input = inputBuilder.toString.trim
          if input.nonEmpty then
            processInput(input)

      catch
        case _: UserInterruptException =>
          println("\nExiting...")
        case _: EndOfFileException =>
          println("\nGoodbye!")

end Calculator
