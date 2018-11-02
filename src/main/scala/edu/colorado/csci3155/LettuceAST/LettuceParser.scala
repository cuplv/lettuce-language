package LettuceAST
import scala.util.parsing.combinator.RegexParsers
import LettuceAST._

class LettuceParser extends RegexParsers {
    def floatingPointNumber: Parser[String] = {
        """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
    }

    def identifier: Parser[String] = {
        """[a-zA-Z_][a-zA-Z0-9_]*""".r
    }
    def LetKwd: Parser[String] = {
        "let"  | "Let"
    }

    def compOperator: Parser[String] = {
        ">=" | "<=" | ">" | "<" | "==" | "!="
    }

    def funDefinition: Parser[FunDef] = {
        ( ("function" ~"(") ~> repsep(identifier,",")) ~ (")" ~> exprLev1)  ^^ {
            case idList~e => FunDef(idList, e)

        }
    }

    def funCallArgs: Parser[List[Expr]] = {
        "(" ~> rep1sep(exprLev1, ",") <~ ")"
    }

    def exprLev1: Parser[Expr] = {
        val opt1 = ("let" ~> identifier) ~ ("=" ~> exprLev1) ~ ("in" ~> exprLev1)  ^^ {
            case s1 ~ e1 ~ e2 => Let(s1, e1, e2)
        }
        val opt2 = ("letrec" ~> identifier) ~ ("=" ~> funDefinition) ~ ("in" ~> exprLev1 ) ^^ {
            case s1 ~ fd ~ e2 => {
                LetRec(s1, fd, e2)
            }
        }
        val opt3 = funDefinition ^^ { s => s }

        val opt4 = ("if" ~> exprLev2)~("then" ~> exprLev1)~("else" ~> exprLev1) ^^ {
            case e ~ e1 ~ e2 => IfThenElse(e, e1, e2)
        }
        val opt6 = "assignref"~>exprLev1 ~ "<-" ~ exprLev1 ^^ { case s ~ "<-" ~ e => AssignRef(s, e)}
        val opt8 = "begin"~>rep1sep(exprLev1, ";")<~"end" ^^ {Block(_)}
        val opt7 = "newref"~> ("("~>exprLev1<~")") ^^{NewRef(_)}

        val opt5 = exprLev2~ opt(("&&"|"||") ~ exprLev1)^^ {
            case e1 ~ Some("&&" ~ e2) => And(e1, e2)
            case e1 ~ Some("||" ~ e2) => Or(e1, e2)
            case e1 ~ None => e1
        }
        /*val opt5 = exprLev2 ~ compOperator~ exprLev1 ^^{
            case e1~">="~e2 => Geq(e1, e2)
            case e1~"<="~e2 => Geq(e2, e1)
            case e1~"=="~e2 => Eq(e1, e2)
            case e1~"!="~e2 => Neq(e1, e2)
            case e1~">"~e2 => Gt(e1, e2)
            case e1~"<"~e2 => Gt(e2, e1)
        }*/

         opt1 | opt2 | opt3 | opt4 |   opt6 | opt7 |  opt8 | opt5
    }

    def exprLev2: Parser[Expr] = {
        exprLev3 ~ opt(compOperator~ exprLev2) ^^{
            case e1~Some(">="~e2) => Geq(e1, e2)
            case e1~Some("<="~e2) => Geq(e2, e1)
            case e1~Some("=="~e2) => Eq(e1, e2)
            case e1~Some("!="~e2) => Neq(e1, e2)
            case e1~Some(">"~e2) => Gt(e1, e2)
            case e1~Some("<"~e2) => Gt(e2, e1)
            case e1~None => e1
        }
    }

    def exprLev3: Parser[Expr] = {
        exprLev4 ~ opt( ("+"| "-") ~ exprLev3 ) ^^ {
            case e1 ~ Some("+" ~ e2) => Plus(e1, e2)
            case e1 ~ Some("-" ~ e2) => Minus(e1, e2)
            case e1 ~ None => e1
        }
    }

    def exprLev4: Parser[Expr] = {

          exprLev5 ~ opt(("*"|"/") ~ exprLev4) ^^ {
              case e1 ~ Some("*" ~ e2) => Mult(e1, e2)
              case e1 ~ Some("/" ~ e2) => Div(e1, e2)
              case e1 ~ None => e1
          }

    }

    def exprLev5: Parser[Expr] = {
          ( floatingPointNumber ^^ { s => ConstNum(s.toFloat)} ) |
          (  "true"^^{ _ => ConstBool(true) } ) |
          ( "false" ^^{ _ => ConstBool(false) } ) |
          (  "(" ~> exprLev1 <~ ")" ) |
          ( ( "sin" | "cos" | "log" | "exp" | "!"  ) ~ ("(" ~> exprLev1 <~ ")") ^^{
              case "sin"~e => Sine(e)
              case "cos"~e => Cosine(e)
              case "log"~e => Log(e)
              case "exp"~e => Exp(e)
              case "!"~e => Not(e)
          } ) |
         ("deref"~ ("(" ~> exprLev1 <~ ")") ~ rep(funCallArgs) ^^{
             case "deref"~e1~Nil => DeRef(e1)
             case "deref"~e1~l => l.foldLeft[Expr] (DeRef(e1)) { case (e, lj) => FunCall(e, lj) }
         }  ) |
         ( identifier ~ rep(funCallArgs)  ^^ {
             case s~Nil => Ident(s)
             case s~l => l.foldLeft[Expr] (Ident(s)) { case (e, lj) => FunCall(e, lj) }
         })
    }

    def parseString(s: String): Program = {
        val e= parseAll(exprLev1, s)
        e match {
            case Success(p, _) => TopLevel(p)
            case Failure(msg, _) => throw new IllegalArgumentException("Failure:" + msg)
            case Error(msg, _) => throw new IllegalArgumentException("Error: " + msg)
        }
    }
}
