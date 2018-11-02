package LettuceAST

case class LettuceStore(m: Map[Int, Value], n: Int) {
    def map = m
    def N = n
    override def toString: String = {
        var st: String = s"Store($n): { "
        for ((i, v) <- m) {
            st = st + s"(cell: $i -> value:$v) "
        }
        st = st + "}"
        return st
    }
}

object StoreInterface {

    def emptyStore: LettuceStore = LettuceStore(Map(), 0)

    def mkNewReference(s: LettuceStore, v: Value): (Int, LettuceStore) = {
        val j = s.N
        val newMap = s.map + (j -> v)
        (j, LettuceStore(newMap, j+1))
    }

    def mkDeref(j: Int, s: LettuceStore): Value = {
        if (s.map contains j)
            s.map(j)
        else
            throw new RuntimeError(s"Deref: Reference to cell $j in store ${s} is missing")
    }


    def mkAssign(j: Int, v: Value, s: LettuceStore): LettuceStore = {
        if (s.map contains j) {
            val newMap = s.map + (j -> v)
            LettuceStore(newMap, s.N)
        } else {
            throw new RuntimeError(s"Assign: Reference to cell $j in store ${s} is missing")
        }
    }
}
