object polynomials {
  class Poly(val terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
    def +(other: Poly) = new Poly(terms ++ (other.terms map adjust))
    
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      (exp, coeff + terms(exp))
    }

    def get(c: Int): Double = terms.get(c) match {
      case None => 0
      case Some(c) => c
    }
    
    override def toString =
      (for ((exp, coeff) <- terms.toList.sortWith(_._1 > _._1)) yield coeff+ getX(exp)) mkString " + "
    def getX(n: Int): String = if (0 == n) "" else "x^"+n

    def plus(other: Poly) = new Poly((other.terms foldLeft terms) (addTerm))
    
    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (e, c) = term
      terms + (e -> (c + new Poly(terms).get(e)))
    }
  }
  
  val m = Map(0 -> 0.0)                           //> m  : scala.collection.immutable.Map[Int,Double] = Map(0 -> 0.0)
  val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2) //> p1  : polynomials.Poly = 6.2x^5 + 4.0x^3 + 2.0x^1
  val p2 = new Poly(0 -> 3.0, 3 -> 7.0)           //> p2  : polynomials.Poly = 7.0x^3 + 3.0
  p1 + p2                                         //> res0: polynomials.Poly = 6.2x^5 + 11.0x^3 + 2.0x^1 + 3.0
  p2 + p1                                         //> res1: polynomials.Poly = 6.2x^5 + 11.0x^3 + 2.0x^1 + 3.0
  p1 plus p2                                      //> res2: polynomials.Poly = 6.2x^5 + 11.0x^3 + 2.0x^1 + 3.0
}