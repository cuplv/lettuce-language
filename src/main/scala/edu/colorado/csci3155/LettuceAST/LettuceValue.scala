package LettuceAST

sealed trait Value

case class NumValue(f: Double) extends Value
case class BoolValue(b: Boolean) extends Value
case class Closure(argList: List[String], e: Expr, sigma: LettuceEnvironment) extends Value
case class Reference(k: Int) extends Value

object  LettuceValue {

    def valueToNum(v: Value): Double =  v match {
        case NumValue(f) => f
        case _ =>  throw new TypeConversionError("Converting from non numeric to number value")
    }

    def valueToBool(v: Value): Boolean = v match  {
        case BoolValue(b) => b
        case _ => throw new TypeConversionError("Converting from non boolean to boolean value")
    }

    def valueToClosure(v: Value): Closure = v match  {
        case Closure(aList, e, sigma) => Closure(aList, e, sigma)
        case _ => throw new TypeConversionError("Converting from non closure to a closure value")
    }

    def valueToReference(v: Value): Int =  v match {
        case Reference(k) => k
        case _ => throw new TypeConversionError("Converting from non reference to a reference value")
    }
}
