package edu.luc.cs.laufer.cs371.expressions

object Calculator:



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
