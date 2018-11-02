package LettuceAST

object LettuceConsole {
    val debug = true
    /* - read in from the standard input and parse/interpret -*/
    def readOneProgram(): (Boolean, String) = {
        var retStr = ""
        var s: String = scala.io.StdIn.readLine()
        if (s == "exit;;" ){
            return (false, "")
        }
        while (!s.endsWith(";;")){
            retStr = retStr + s
            s = scala.io.StdIn.readLine("|")
        }
        retStr = retStr + s.dropRight(2) + "\n"
        if (debug){
            println(" --- Debug --- ")
            print(retStr)
            println(" --- Debug --- ")
        }
        return (true, retStr)
    }

    def processInput(s: String): Value = {
        val p: Program = new LettuceParser().parseString(s)
        if (debug) {
            println("--- Debug --- ")
            println(p)
            println("--- Debug ---")
        }
        val v = LettuceInterpreter.evalProgram(p)
        println(s"Returned value : $v")
        v
    }
    def main(args: Array[String]): Unit = {
        while (true){
            print("> ")
            try {
                val (b, s) = readOneProgram()
                if (b) {
                    val v = processInput(s)
                } else {
                    sys.exit(1)
                }
            } catch {
                case UnboundIdentifierError(msg) => println(s"Error: Unbound Identifier - $msg")
                case TypeConversionError(msg)=> println(s"Error: Type Conversion error $msg")
                case SyntaxError(msg) => println(s"Syntax Error: $msg")
                case RuntimeError(msg) => println(s"Runtime Error: $msg")
            }
        }
    }


}
