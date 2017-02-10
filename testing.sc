object testing {
  type Word = String

  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]
  
  val w:Word = "arugula"                          //> w  : testing.Word = arugula
  val s:Sentence = List("arugula","is","a","vegetable")
                                                  //> s  : testing.Sentence = List(arugula, is, a, vegetable)
  
  val dict: List[Word] = List("is", "tea", "eat", "ate")
                                                  //> dict  : List[testing.Word] = List(is, tea, eat, ate)
    
  def wordOccurrences(w: Word): Occurrences =
    w.toLowerCase.filter(_.isLetter).groupBy(_.charValue()).toList.map(p => (p._1, p._2.length))
                                                  //> wordOccurrences: (w: testing.Word)testing.Occurrences
  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences(s.foldLeft(List[Char]())((a,b) => a ++ b).mkString)
                                                  //> sentenceOccurrences: (s: testing.Sentence)testing.Occurrences
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dict.groupBy(wordOccurrences(_))              //> dictionaryByOccurrences: => Map[testing.Occurrences,List[testing.Word]]
  
  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences.get(wordOccurrences(word)).get
                                                  //> wordAnagrams: (word: testing.Word)List[testing.Word]
                                                  
  val wo = wordOccurrences("tat")                 //> wo  : testing.Occurrences = List((t,2), (a,1))
  
    def getSublists(x:Occurrences): Occurrences =
      x match {
        case List((_, 1)) => x
        case List((l, n)) => (l, n) :: getSublists(List((l, n-1)))
      }                                           //> getSublists: (x: testing.Occurrences)testing.Occurrences
  val ms = getSublists(List(('m',2)))             //> ms  : testing.Occurrences = List((m,2), (m,1))
  val hs = getSublists(List(('h',1)))             //> hs  : testing.Occurrences = List((h,1))
  val js = getSublists(List(('j',1)))             //> js  : testing.Occurrences = List((j,1))
      
      
  def emptySafeMap(occ: (Char, Int), acc: List[Occurrences]): List[Occurrences] =
    acc match {
      case List() => List(List(occ)) ::: List(List())
      case x::xs => acc.map(y => occ :: x)
    }                                             //> emptySafeMap: (occ: (Char, Int), acc: List[testing.Occurrences])List[testin
                                                  //| g.Occurrences]
  def prepend(occ: (Char, Int), acc: List[Occurrences]): List[Occurrences] =
    acc ::: emptySafeMap(occ, acc)                //> prepend: (occ: (Char, Int), acc: List[testing.Occurrences])List[testing.Occ
                                                  //| urrences]

  def prependEach(occ: Occurrences, acc: List[Occurrences]): List[Occurrences] =
    occ.flatMap(occN => prepend(occN, acc)).toSet.toList ::: acc
                                                  //> prependEach: (occ: testing.Occurrences, acc: List[testing.Occurrences])List
                                                  //| [testing.Occurrences]

  
  val x:(Char,Int) = ('a',2)                      //> x  : (Char, Int) = (a,2)
  val y:List[Occurrences] = List(List(),List(('g',1)))
                                                  //> y  : List[testing.Occurrences] = List(List(), List((g,1)))
  prepend(x,y)                                    //> res0: List[testing.Occurrences] = List(List(), List((g,1)), List((a,2)), Li
                                                  //| st((a,2)))

  prependEach(ms, y)                              //> res1: List[testing.Occurrences] = List(List(), List((g,1)), List((m,2)), Li
                                                  //| st((m,1)), List(), List((g,1)))
  prependEach(ms, List())                         //> res2: List[testing.Occurrences] = List(List((m,2)), List(), List((m,1)))
   
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def combinations(occurrences: Occurrences, acc: List[Occurrences]): List[Occurrences] = {
    println("occ = "+occurrences+", acc = "+acc)
      occurrences match {
        case List() => acc
        case (x :: xs) => {
          val sl = getSublists(List(x))
          println("x = "+x+", sl = "+sl+", acc = "+acc)
          println("prependEach = "+prependEach(sl, acc))
          combinations(xs, prependEach(sl, acc))
        }
      }
    }
    combinations(occurrences, List()).toSet.toList
  }                                               //> combinations: (occurrences: testing.Occurrences)List[testing.Occurrences]
  
  wordOccurrences("tat")                          //> res3: testing.Occurrences = List((t,2), (a,1))
  combinations(wordOccurrences("tat"))            //> occ = List((t,2), (a,1)), acc = List()
                                                  //| x = (t,2), sl = List((t,2), (t,1)), acc = List()
                                                  //| prependEach = List(List((t,2)), List(), List((t,1)))
                                                  //| occ = List((a,1)), acc = List(List((t,2)), List(), List((t,1)))
                                                  //| x = (a,1), sl = List((a,1)), acc = List(List((t,2)), List(), List((t,1)))
                                                  //| prependEach = List(List((t,2)), List(), List((t,1)), List((a,1), (t,2)), Li
                                                  //| st((t,2)), List(), List((t,1)))
                                                  //| occ = List(), acc = List(List((t,2)), List(), List((t,1)), List((a,1), (t,2
                                                  //| )), List((t,2)), List(), List((t,1)))
                                                  //| res4: List[testing.Occurrences] = List(List((t,2)), List(), List((t,1)), Li
                                                  //| st((a,1), (t,2)))

  
}