package LettuceAST
import org.scalatest.FunSuite

class ParserTests extends FunSuite {
    test ("simple let binding program 1") {
        val s ="let x = 10 in x + 15"
        val p = new LettuceParser()
        val t = p.parseString(s)
        println(t)
        assert( t == TopLevel(Let("x", ConstNum(10.0), Plus(Ident("x"), ConstNum(15.0)))))
    }

    test("let binding with more arithmetic") {
        val s =
            """
              | let x = 10 in
              |    let y = 15 - x in
              |      let z = y - x * y/cos(x) in
              |            (x * y - sin(z))
            """.stripMargin
        val p = new LettuceParser()
        val t = p.parseString(s)
        println(t)
    }

    test("let binding function 1") {
        val s =
            """
              |let f = function (x, y)
              |           (x * y - y)/(1 + x + y)
              |    in
              | let z = 20 in
              |     f (10, z)
            """.stripMargin
        val p = new LettuceParser()
        val t = p.parseString(s)
        println(t)
    }

    test("let binding function 2") {
        val s =
            """
              |let f = function (x, y)
              |           if (x <= 0)
              |           then   (x * y - y)/(1 + x + y)
              |           else ( let z = 25 in x * z - cos(y) )
              |    in
              | let z = 20 in
              |     f (10, z)
            """.stripMargin
        val p = new LettuceParser()
        val t = p.parseString(s)
        println(t)
    }

    test("recursion 1") {
        val s =
            """
              |letrec f = function (x)
              |             if (x <= 0)
              |             then 1
              |             else x * f(x-1)
              | in
              |   f(10)
            """.stripMargin
        val p = new LettuceParser()
        val t = p.parseString(s)
        println(t)
    }

    test("curried function") {
        val s =
            """
              |let f = function (x)
              |         function (y)
              |             if (y <= 0)
              |             then 1
              |             else x / exp(1+ y)
              | in
              |   f(10) (25)
            """.stripMargin
        val p = new LettuceParser()
        val t = p.parseString(s)
        println(t)
    }

    test("test with references 1"){
        val s =
            """
              |let z = 26 in
              |   let x = newref( z) in
              |     let dummy = assignref x <- 35 in
              |             deref(x)
            """.stripMargin
        val p = new LettuceParser()
        val t = p.parseString(s)
        println(t)
    }
    test("test with references 2"){
        val s =
            """
              |let z = 26 in
              |   let x = newref( function(x) x - 35 ) in
              |     let g = function(y) y + 36 in
              |        let dummy = assignref x <- (if (z >= 0) then deref(x) else g ) in
              |             deref(x)(45)
            """.stripMargin
        val p = new LettuceParser()
        val t = p.parseString(s)
        println(t)
    }

    test("Block test") {
        val s =
            """
              |let x = begin
              |          let y = 20 in y + 10 ;
              |          newref(35)
              |        end in
              |        deref(x) + 34
            """.stripMargin
        val p = new LettuceParser()
        val t = p.parseString(s)
        println(t)
    }

}
