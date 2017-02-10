object SequenceOperations {
  val list = List(1,2,3)                          //> list  : List[Int] = List(1, 2, 3)
  list.flatMap(x => Range(1,x+1))                 //> res0: List[Int] = List(1, 1, 2, 1, 2, 3)
  
  def isPrime(x: Int): Boolean = Range(2,x).forall(y => x % y != 0)
                                                  //> isPrime: (x: Int)Boolean
    
  isPrime(4)                                      //> res1: Boolean = false
  isPrime(5)                                      //> res2: Boolean = true
  
  def scalarProduct(xs: List[Double], ys: List[Double]): Double =
    (for { (x, y) <- xs.zip(ys) } yield x * y).sum//> scalarProduct: (xs: List[Double], ys: List[Double])Double
    
  val list2 = List(1.0,2.0)                       //> list2  : List[Double] = List(1.0, 2.0)
  val list3 = List(2.0, 3.0)                      //> list3  : List[Double] = List(2.0, 3.0)
  scalarProduct(list2, list3)                     //> res3: Double = 8.0
}