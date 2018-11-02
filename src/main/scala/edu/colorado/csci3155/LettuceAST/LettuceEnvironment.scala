package LettuceAST

sealed trait LettuceEnvironment {
    def lookup(s: String): Value
}

case object EmptyEnvironment extends LettuceEnvironment {
    override def lookup(s:String): Value = throw new UnboundIdentifierError(s"Identifier $s is not known")
}
case class ExtendEnv(m: Map[String, Value], e: LettuceEnvironment) extends LettuceEnvironment {
    override def lookup(s: String): Value = {
        if (m contains s){
            m(s)
        } else {
            e.lookup(s)
        }
    }
}


case class ExtendEnvRec(f: String, xList: List[String], e: Expr,  eHat: LettuceEnvironment) extends LettuceEnvironment {
    override def lookup(s: String): Value = {
        if (s == f){
            Closure(xList, e, this)
        } else {
            eHat.lookup(s)
        }
    }
}


object EnvironmentUtils {
    def make_extension(l: List[String], vList: List[Value], env: LettuceEnvironment): LettuceEnvironment = {
        val lvList = l zip vList
        val lvMap = Map() ++ lvList
        ExtendEnv(lvMap, env)
    }
}