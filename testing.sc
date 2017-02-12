object testing {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]
  
  def wordOccurrences(w: Word): Occurrences =
    w.toLowerCase
    .filter(_.isLetter)
    .groupBy(_.charValue())
    .toList.map(p => (p._1, p._2.length))
    .sortBy(_._1)                                 //> wordOccurrences: (w: testing.Word)testing.Occurrences

  def getSublists(x:Occurrences): Occurrences =
    x match {
      case List((_, 1)) => x
      case List((l, n)) => (l, n) :: getSublists(List((l, n-1)))
    }                                             //> getSublists: (x: testing.Occurrences)testing.Occurrences
  
  def prepend(o: Occurrences, acc:List[Occurrences]): List[Occurrences] =
    acc.flatMap(a => for { ci <- o } yield (ci :: a).sortBy(_._1))
                                                  //> prepend: (o: testing.Occurrences, acc: List[testing.Occurrences])List[testin
                                                  //| g.Occurrences]
  
  def smoosh(o: Occurrences, acc:List[Occurrences]): List[Occurrences] =
    o.map(x => List(x)) ::: acc ::: prepend(o, acc)
                                                  //> smoosh: (o: testing.Occurrences, acc: List[testing.Occurrences])List[testing
                                                  //| .Occurrences]
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def combinations(occurrences: Occurrences, acc: List[Occurrences]): List[Occurrences] =
      occurrences match {
        case List() => List(List()) ::: acc
        case (x :: xs) => combinations(xs, smoosh(getSublists(List(x)), acc))
      }

    combinations(occurrences, List())
  }                                               //> combinations: (occurrences: testing.Occurrences)List[testing.Occurrences]
  
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def subSubtract(o: (Char, Int), y: Occurrences): (Char, Int) = {
      val y2 = y.filter(o._1 == _._1)
      if (0 == y2.length) o else  (o._1, o._2 - y.filter(o._1 == _._1).head._2)
    }

    x match {
      case List() => List()
      case (x1 :: xs) => {
        val ss = subSubtract(x1, y)
        if (ss._2 == 0) subtract(xs, y) else ss :: subtract(xs, y)
      }
    }
  }                                               //> subtract: (x: testing.Occurrences, y: testing.Occurrences)testing.Occurrenc
                                                  //| es
  
  def sentenceOccurrences(s: Sentence): Occurrences =
  wordOccurrences(s.foldLeft(List[Char]())((a,b) => a ++ b).mkString)
                                                  //> sentenceOccurrences: (s: testing.Sentence)testing.Occurrences
  
  val dictionary: List[Word] = forcomp.loadDictionary
                                                  //> dictionary  : List[testing.Word] = List(Aarhus, Aaron, Ababa, aback, abaft,
                                                  //|  abandon, abandoned, abandoning, abandonment, abandons, abase, abased, abas
                                                  //| ement, abasements, abases, abash, abashed, abashes, abashing, abasing, abat
                                                  //| e, abated, abatement, abatements, abater, abates, abating, Abba, abbe, abbe
                                                  //| y, abbeys, abbot, abbots, Abbott, abbreviate, abbreviated, abbreviates, abb
                                                  //| reviating, abbreviation, abbreviations, Abby, abdomen, abdomens, abdominal,
                                                  //|  abduct, abducted, abduction, abductions, abductor, abductors, abducts, Abe
                                                  //| , abed, Abel, Abelian, Abelson, Aberdeen, Abernathy, aberrant, aberration, 
                                                  //| aberrations, abet, abets, abetted, abetter, abetting, abeyance, abhor, abho
                                                  //| rred, abhorrent, abhorrer, abhorring, abhors, abide, abided, abides, abidin
                                                  //| g, Abidjan, Abigail, Abilene, abilities, ability, abject, abjection, abject
                                                  //| ions, abjectly, abjectness, abjure, abjured, abjures, abjuring, ablate, abl
                                                  //| ated, ablates, ablating
                                                  //| Output exceeds cutoff limit.
  lazy val dbo: Map[Occurrences, List[Word]] =
    dictionary.groupBy(wordOccurrences(_))        //> dbo: => Map[testing.Occurrences,List[testing.Word]]

  val cutoff = List("cut","off")                  //> cutoff  : List[String] = List(cut, off)
  combinations(sentenceOccurrences(cutoff)).flatMap(c => dbo.get(c))
                                                  //> res0: List[List[testing.Word]] = List(List(off), List(of), List(to), List(o
                                                  //| ft), List(cot), List(cuff), List(out), List(cut), List(cutoff))
  sentenceOccurrences(cutoff)                     //> res1: testing.Occurrences = List((c,1), (f,2), (o,1), (t,1), (u,1))
  
/*  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def sa2(words: Option[List[Word]], so: List[Occurrences], accS: List[Sentence], accW: Sentence): List[Sentence] =
      words match {
        case None          => accS
        case Some(List())  => accS
        case Some(x :: xs) => sentenceAnagrams(subtract(so, wordOccurrences(x)), accS, accW)
      }
  
    def sentenceAnagrams(so: List[Occurrences], accS: List[Sentence], accW: Sentence): List[Sentence] =
      so match {
        case List()    => accW :: accS
        case (x :: xs) => sa2(dbo.get(x), so, accS, accW)
      }
    
    sentenceAnagrams(combinations(sentenceOccurrences(sentence)), List(), List())
  }   */
    
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def sentenceAnagrams(occurrences: Occurrences): List[Sentence] =
      occurrences match {
      case List() => List(Nil)
      case (x :: xs) => for {
        combination <- combinations(occurrences)
        word <- dbo.getOrElse(combination, Nil)
        sentence <- sentenceAnagrams(subtract(occurrences, wordOccurrences(word)))
      } yield word :: sentence
    }
    
    sentenceAnagrams(sentenceOccurrences(sentence))
  }                                               //> sentenceAnagrams: (sentence: testing.Sentence)List[testing.Sentence]
  
  sentenceAnagrams(List("olive","you"))           //> res2: List[testing.Sentence] = List(List(Io, Lev, you), List(Io, you, Lev),
                                                  //|  List(Lev, Io, you), List(Lev, you, Io), List(olive, you), List(you, Io, Le
                                                  //| v), List(you, Lev, Io), List(you, olive))
  sentenceAnagrams(List("scala", "is", "fun"))    //> res3: List[testing.Sentence] = List(List(if, Al, can, Sus), List(if, Al, ca
                                                  //| ns, us), List(if, Al, scan, us), List(if, Al, Sus, can), List(if, Al, us, c
                                                  //| ans), List(if, Al, us, scan), List(if, Lac, an, Sus), List(if, Lac, ass, nu
                                                  //| ), List(if, Lac, as, sun), List(if, Lac, San, us), List(if, Lac, Sus, an), 
                                                  //| List(if, Lac, us, San), List(if, Lac, nu, ass), List(if, Lac, sun, as), Lis
                                                  //| t(if, Lac, Susan), List(if, an, Lac, Sus), List(if, an, Sus, Lac), List(if,
                                                  //|  can, Al, Sus), List(if, can, Sal, us), List(if, can, Sus, Al), List(if, ca
                                                  //| n, us, Sal), List(if, canal, Sus), List(if, clan, as, us), List(if, clan, u
                                                  //| s, as), List(if, ass, Lac, nu), List(if, ass, nu, Lac), List(if, as, Lac, s
                                                  //| un), List(if, as, clan, us), List(if, as, us, clan), List(if, as, sun, Lac)
                                                  //| , List(if, Sal, can, us), List(if, Sal, us, can), List(if, Scala, sun), Lis
                                                  //| t(if, San, Lac, us), List(if, San, us, Lac), List(if, San, Claus), List(if,
                                                  //|  San, Lucas), List(if, 
                                                  //| Output exceeds cutoff limit.
}