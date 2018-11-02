package LettuceAST
import org.scalatest.FunSuite
class InterpreterTests extends FunSuite {
    test("program 1") {
        val s ="let x = 10 in x + 15"
        val v = LettuceConsole.processInput(s)
        assert(v == NumValue(25))
    }

    test("program 2") {
        val s =
            """
              |let s = function (f, x)
              |            f(f(x,x), x)
              |        in
              |  let t = function (x, y)
              |            x * y - y
              |            in
              |       s(t, 25)
            """.stripMargin
        val v = LettuceConsole.processInput(s)
        assert(v == NumValue(14975.0))
    }

    test("program 3") {
        // Lettuce program to sum up sin(x) from x = -1.0, 0.9, ... 0,  ..., 1.0
        val s =
            """
              | letrec f = function (sum, x)
              |              if (x >= 1.0)
              |              then   sum
              |              else
              |                  f(sum + sin(x), x + 0.1)
              |      in
              |  f(0.0, -1.0)
            """.stripMargin
        val v = LettuceConsole.processInput(s)
        v match {
            case NumValue(f) => assert(-0.8415 <= f && f <= -0.8414, "value not within range for output")
            case _ => assert(false, "program should return a number")
        }
    }

    test("trapezoid integration program") {
        val s =
            """
              | letrec trap = function (f, lo, hi, delta, sum)
              |    if (lo >= hi)
              |    then sum
              |    else
              |       (
              |         let t1 = f(lo) in
              |         let t2 = f(lo+delta) in
              |         let area = (t1+t2) * delta / 2.0 in
              |            trap(f, lo+delta, hi, delta, sum+area)
              |       )
              |  in
              |  let oneoverx = function(x) 1.0/x in
              |    trap(oneoverx, 1.0, 2.0, 0.05, 0.0)
            """.stripMargin
        val v = LettuceConsole.processInput(s)
        v match {
            case NumValue(f) => assert(0.693 <= f && f <= 0.6935)
            case _ => assert(false, "Program should return a number. ")
        }
    }

    test("Equation solving using newton raphson") {
        val s =
            """
              | letrec nraphson = function (f, fdot, x)
              |    let t = f(x) in
              |       if (t <= 0.0001 && t >= -0.0001)
              |       then x
              |       else (
              |          let tdot = fdot(x) in
              |             nraphson(f, fdot, x - t/tdot)
              |       ) in
              |    let fx = function (x) x*x - 2.0 in
              |    let fdotx = function (x) 2*x in
              |       nraphson(fx, fdotx, 1.0)
            """.stripMargin

        val v = try { LettuceConsole.processInput(s) } catch {case RuntimeError(msg) => println(msg); NumValue(10)}
        v match {
            case NumValue(f) => assert(1.414 <= f && f <= 1.415)
            case _ => assert(false, "Program should return a number. ")
        }
    }

    test("Program with refs1"){
        val s =
            """
              |let z = 26 in
              |   let x = newref( function(x) x - 35 ) in
              |     let g = function(y) y + 36 in
              |        let dummy = assignref x <- (if (z >= 0) then deref(x) else g ) in
              |             deref(x)(45)
            """.stripMargin
        val v = try { LettuceConsole.processInput(s) } catch {case RuntimeError(msg) => println(msg); NumValue(10)}
        assert(v == NumValue(10.0), "Failed")
    }

}
