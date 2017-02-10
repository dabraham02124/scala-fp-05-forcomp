object translate {
  val words = List("scala","scaja","is","fun")    //> words  : List[String] = List(scala, scaja, is, fun)
  
  def translate(num: List[Int]): List[String] = num match {
    case List(x) => List(x.toString)//getMnem(x)
    case x :: xs => List(x.toString) ++ translate(xs)//mnem.get(x).get.toList.foldLeft(translate(xs)) (prepend)
  }                                               //> translate: (num: List[Int])List[String]
  
  def prepend(ss: List[String], c: Char): List[String] =
    for (s <- ss) yield c.toString ++ s           //> prepend: (ss: List[String], c: Char)List[String]
  
  val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
                      '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
                                                  //> mnem  : scala.collection.immutable.Map[Char,String] = Map(8 -> TUV, 4 -> GHI
                                                  //| , 9 -> WXYZ, 5 -> JKL, 6 -> MNO, 2 -> ABC, 7 -> PQRS, 3 -> DEF)
                      
  val charCode: Map[Char, Char] =
    for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit
                                                  //> charCode  : Map[Char,Char] = Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, J -
                                                  //| > 5, U -> 8, F -> 3, A -> 2, M -> 6, I -> 4, G -> 4, V -> 8, Q -> 7, L -> 5,
                                                  //|  B -> 2, P -> 7, C -> 2, H -> 4, W -> 9, K -> 5, R -> 7, O -> 6, D -> 3, Z -
                                                  //| > 9, S -> 7)

  def wordCode(word: String): String =
    word.toUpperCase map charCode                 //> wordCode: (word: String)String
    
  def wordsForNum: Map[String, Seq[String]] =
    words groupBy wordCode                        //> wordsForNum: => Map[String,Seq[String]]

  wordsForNum("72252")                            //> res0: Seq[String] = List(scala, scaja)

  def getFromMap(x: Int): List[String] = {
    val a = mnem.get(2)
    val b = a.getOrElse("?")
    val c = b.toList
    val d = c.map(ch => ch.toString)
    d
  }                                               //> getFromMap: (x: Int)List[String]
  //translate(List(7,2,2,5,2,4,7,3,8,6))
  translate(List(7,2,2))                          //> res1: List[String] = List(7, 2, 2)
}