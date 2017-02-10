object testing2 {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]
  
  val w:Word = "arugula"                          //> w  : testing2.Word = arugula
  val s:Sentence = List("arugula","is","a","vegetable")
                                                  //> s  : testing2.Sentence = List(arugula, is, a, vegetable)
  val dict: List[Word] = List("is", "tea", "eat", "ate")
                                                  //> dict  : List[testing2.Word] = List(is, tea, eat, ate)
    
  def wordOccurrences(w: Word): Occurrences =
    w.toLowerCase.filter(_.isLetter).groupBy(_.charValue()).toList.map(p => (p._1, p._2.length))
                                                  //> wordOccurrences: (w: testing2.Word)testing2.Occurrences
  def getSublists(x:Occurrences): Occurrences =
    x match {
      case List((_, 1)) => x
      case List((l, n)) => (l, n) :: getSublists(List((l, n-1)))
    }                                             //> getSublists: (x: testing2.Occurrences)testing2.Occurrences
  val ms = getSublists(List(('m',2)))             //> ms  : testing2.Occurrences = List((m,2), (m,1))
  val hs = getSublists(List(('h',1)))             //> hs  : testing2.Occurrences = List((h,1))
  val wo = wordOccurrences("tat")                 //> wo  : testing2.Occurrences = List((t,2), (a,1))
  
  def prepend(o: Occurrences, acc:List[Occurrences]): List[Occurrences] = {
    acc.flatMap(a => for { ci <- o } yield ci :: a)
  }                                               //> prepend: (o: testing2.Occurrences, acc: List[testing2.Occurrences])List[test
                                                  //| ing2.Occurrences]
  
  def smoosh(o: Occurrences, acc:List[Occurrences]): List[Occurrences] = {
    o.map(x => List(x)) ::: acc ::: prepend(o, acc)
  }                                               //> smoosh: (o: testing2.Occurrences, acc: List[testing2.Occurrences])List[testi
                                                  //| ng2.Occurrences]
  prepend(ms, List(hs))                           //> res0: List[testing2.Occurrences] = List(List((m,2), (h,1)), List((m,1), (h,1
                                                  //| )))
  smoosh(ms, List())                              //> res1: List[testing2.Occurrences] = List(List((m,2)), List((m,1)))
  smoosh(ms, List(hs))                            //> res2: List[testing2.Occurrences] = List(List((m,2)), List((m,1)), List((h,1
                                                  //| )), List((m,2), (h,1)), List((m,1), (h,1)))
                                                  
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def combinations(occurrences: Occurrences, acc: List[Occurrences]): List[Occurrences] = {
    println("occ = "+occurrences+", acc = "+acc)
      occurrences match {
        case List() => List(List()) ::: acc
        case (x :: xs) => {
          val sl = getSublists(List(x))
          println("x = "+x+", sl = "+sl+", acc = "+acc+ ", smoosh = "+smoosh(sl, acc))
          combinations(xs, smoosh(sl, acc))
        }
      }
    }
    combinations(occurrences, List()).toSet.toList
  }                                               //> combinations: (occurrences: testing2.Occurrences)List[testing2.Occurrences]
                                                  //| 
  
  wordOccurrences("tat")                          //> res3: testing2.Occurrences = List((t,2), (a,1))
  combinations(wordOccurrences("baba"))           //> occ = List((b,2), (a,2)), acc = List()
                                                  //| x = (b,2), sl = List((b,2), (b,1)), acc = List(), smoosh = List(List((b,2))
                                                  //| , List((b,1)))
                                                  //| occ = List((a,2)), acc = List(List((b,2)), List((b,1)))
                                                  //| x = (a,2), sl = List((a,2), (a,1)), acc = List(List((b,2)), List((b,1))), s
                                                  //| moosh = List(List((a,2)), List((a,1)), List((b,2)), List((b,1)), List((a,2)
                                                  //| , (b,2)), List((a,1), (b,2)), List((a,2), (b,1)), List((a,1), (b,1)))
                                                  //| occ = List(), acc = List(List((a,2)), List((a,1)), List((b,2)), List((b,1))
                                                  //| , List((a,2), (b,2)), List((a,1), (b,2)), List((a,2), (b,1)), List((a,1), (
                                                  //| b,1)))
                                                  //| res4: List[testing2.Occurrences] = List(List((a,1)), List((b,1)), List((b,2
                                                  //| )), List((a,1), (b,1)), List(), List((a,2), (b,1)), List((a,2)), List((a,1)
                                                  //| , (b,2)), List((a,2), (b,2)))

 
}