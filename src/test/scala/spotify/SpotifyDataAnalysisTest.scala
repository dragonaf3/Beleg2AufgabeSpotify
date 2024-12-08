package spotify

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import spotify.models.Song
import spotify.utils.UtilsSpotifyData

class SpotifyDataAnalysisTest extends AnyFunSuite with BeforeAndAfterAll {

  val epsilon = 0.00001
  val songs= UtilsSpotifyData.loadData("SpotifyMostStreamedSongs.csv")
  val s1 = "This is the 88 Test! The result !!!should be: 9 Words"
  val words_s1 = List("be", "is", "result", "should", "test", "the", "the", "this", "words")
  val s2 = "This is another test. It contains a lot of words which are also in string 1."
  val words_s2 = List("a", "also", "another", "are", "contains", "in", "is", "it", "lot", "of", "string", "test", "this", "which", "words")
  val song1= Song(1L, s1, "artist1", 1, 2019, 11, 29, 43899, 69, 3703895074L, 672, 199, 3421, 20, -1, 171, "C#", "Major", 50, 50, 80, 0, 0, 9, 7, "")
  val song2= Song(2L, s2, "artist2", 1, 2019, 11, 29, 43899, 69, 3703895074L, 672, 199, 3421, 20, -1, 171, "C#", "Major", 50, 50, 80, 0, 0, 9, 7, "")
  val wordlist = List("a", "also", "another", "are", "be", "contains", "in", "is", "is", "it", "lot", "of", "result", "should", "string",
    "test", "test", "the", "the", "this", "this", "which", "words", "words")
  val music_words= List("track", "feat", "version", "remix", "music", "vol", "sessions", "edit")
  val important_words= List("love", "christmas", "spider", "like", "man", "taylor", "let", "snow", "know", "baby", "metro", "verse", "time", "future", "night", "scott", "die", "take")

  def getWordResources(filename:String):Set[String]=
    val url = getClass.getResource("/"+filename).getPath
    val src = scala.io.Source.fromFile(url)
    val result = src.getLines().foldLeft(Set(): Set[String])((set, el) => set + el)
    src.close()
    result

  // get stopwords from resources in test folder
  val stopwords: Set[String] = getWordResources("stopwords.txt")
  // get wordlist from resources in main folder
  val words: Set[String] = getWordResources("words.txt")

  test("Get most successful song") :
    val result= SpotifyDataAnalysis.getSongWithMostStreams(songs)
    assert(result===Song(56L, "Blinding Lights", "The Weeknd", 1, 2019, 11, 29, 43899, 69, 3703895074L, 672, 199, 3421, 20, -1, 171, "C#", "Major", 50, 50, 80, 0, 0, 9, 7, "https://i.scdn.co/image/ab67616d0000b2738863bc11d2aa12b54f5aeb36"))

  test("Test getNameAndNumberOfTheArtistWithMostSongsInList") :
    val result=SpotifyDataAnalysis.getNameAndNumberOfTheArtistWithMostSongsInList(songs)
    assert(result===("Taylor Swift",34))

  test("Test getArtistWithMostStreams"):
    val result = SpotifyDataAnalysis.getArtistWithMostStreams(songs)
    assert(result === ("The Weeknd", 14185552870L))

  test("Get min/max/avg-BPM") :
    val result= SpotifyDataAnalysis.getMinAndMaxAndAvgBPM(songs)
    assert(math.abs(result._3-122.54039874081847)<epsilon)
    assert (result._2==206 && result._1==65)

  test("Get Percental Modes by Month") :
    val expected= List((6, 0.5), (2, 0.4918032786885246), (5, 0.4765625), (1, 0.4701492537313433))
    val result= SpotifyDataAnalysis.getThe4MonthWithMostMinorSongs(songs)
    val z= result.zip(expected).map(x=> math.abs(x._1._2-x._2._2))
    assert(z.forall(_<epsilon))

  test("Test Word Extraction 1") :
    val r = SpotifyDataAnalysis.getWords(s1)
    assert(r.length === 9)
    assert(r.sorted === words_s1)

  test("Test Word Extraction 2") :
    val r = SpotifyDataAnalysis.getWords(s2)
    assert(r.length === 15)
    assert(r.sorted === words_s2)

  test("get All Words Extraction") :

    val test_list= List(song1,song2)
    val result = SpotifyDataAnalysis.getAllWords(test_list)
    assert(result.length === 24)
    assert(result.sorted === wordlist)

  test("count all Words in song titles") :

    val result = SpotifyDataAnalysis.getAllWords(songs)
    assert(result.length === 2954)

  test("The 4 most frequent words in title") :

    val result= SpotifyDataAnalysis.getThe4MostFrequentWordsInTitle(songs)
    assert(result===List(("the", 78), ("feat", 61), ("with", 46), ("you", 44)))

  test("The 20 most frequent words in title without stopwords"):

    val result = SpotifyDataAnalysis.getThe20MostFrequentWordsInTitleWithFilter(songs, word => !stopwords.contains(word))
    assert(result === List(("feat",61), ("love",24), ("la",20), ("christmas",17), ("remix",17), ("version",16), ("spider",14), ("like",14), ("music",13), ("de",12), ("man",11), ("el",10), ("vol",10), ("bts",9), ("remastered",9), ("sessions",9), ("bzrp",9), ("taylor",9), ("let",9), ("snow",8)))

  test("The 20 most frequent words in title with wordlist and stopwords"):

    val result = SpotifyDataAnalysis.getThe20MostFrequentWordsInTitleWithFilter(songs, word=>(words.contains(word) && !stopwords.contains(word) && !music_words.contains(word) && word.length>2))
    assert(result === List(("love", 24), ("christmas", 17), ("spider", 14), ("like", 14), ("man", 11), ("taylor", 9), ("let", 9), ("snow", 8), ("know", 8), ("baby", 8), ("metro", 7), ("verse", 7), ("time", 7), ("future", 6), ("night", 6), ("scott", 6), ("lil", 6), ("die", 6), ("take", 6), ("travis", 6)))

  test("get All Words Extraction with Indizees") :

    val expected= List((1,"be"), (1,"is"), (1,"result"), (1,"should"), (1,"test"), (1,"the"), (1,"this"), (1,"words"),
      (2,"a"), (2,"also"), (2,"another"), (2,"are"), (2,"contains"), (2,"in"), (2,"is"), (2,"it"), (2,"lot"), (2,"of"),
      (2,"string"), (2,"test"), (2,"this"), (2,"which"), (2,"words"))
    val result= SpotifyDataAnalysis.getAllWordsWithIndex(List(song1,song2)).toList.sorted
    assert (result.length===23)
    assert (result.sorted===expected)

  test("create Inverse Indizees") :

    val expected = Map("test" -> Set(2, 1), "another" -> Set(2), "result" -> Set(1), "it" -> Set(2), "a" -> Set(2),
      "string" -> Set(2), "also" -> Set(2), "should" -> Set(1), "which" -> Set(2), "be" -> Set(1),
      "contains" -> Set(2), "of" -> Set(2), "the" -> Set(1), "this" -> Set(1, 2), "in" -> Set(2), "are" -> Set(2),
      "is" -> Set(1, 2), "lot" -> Set(2), "words" -> Set(2, 1))
    val result = SpotifyDataAnalysis.createInverseIndex(SpotifyDataAnalysis.getAllWordsWithIndex(List(song1,song2)))
    assert(result === expected)

  test("test or Conjunction 1") :

      val invInd: Map[String, Set[Long]] = SpotifyDataAnalysis.createInverseIndex(SpotifyDataAnalysis.getAllWordsWithIndex(List(song1,song2)))
      val result = SpotifyDataAnalysis.orConjunction(List("hello", "test"), invInd)
      assert(result === Set(1L, 2L))

  test("test or Conjunction 2") :

      val invInd = SpotifyDataAnalysis.createInverseIndex(SpotifyDataAnalysis.getAllWordsWithIndex(List(song1,song2)))
      val result = SpotifyDataAnalysis.orConjunction(List("hello", "contains"), invInd)
      assert(result === Set(2))

  test("test or Conjunction 3") :

      val invInd = SpotifyDataAnalysis.createInverseIndex(SpotifyDataAnalysis.getAllWordsWithIndex(List(song1,song2)))
      val result = SpotifyDataAnalysis.orConjunction(List("hello", "bang"), invInd)
      assert(result === Set())

    test("Find Songs with more than two words from wordlist") :

      val res= SpotifyDataAnalysis.findSongsWithAtLeast2WordsFromWordlist(songs,important_words)
      val result= Set((449,"Let It Snow! Let It Snow! Let It Snow!",Set("let", "snow")), (467,"Let It Snow! Let It Snow! Let It Snow!",Set("let", "snow")),
        (850,"Only Love Can Hurt Like This",Set("love", "like")), (446,"It's Beginning To Look A Lot Like Christmas",Set("like", "christmas")),
        (136,"Calling (Spider-Man: Across the Spider-Verse) (Metro Boomin & Swae Lee, NAV, feat. A Boogie Wit da Hoodie)",Set("spider", "man", "verse", "metro")),
        (492,"You Make It Feel Like Christmas (feat. Blake Shelton)",Set("like", "christmas")),
        (239,"Link Up (Metro Boomin & Don Toliver, Wizkid feat. BEAM & Toian) - Spider-Verse Remix (Spider-Man: Across the Spider-Verse )",Set("metro", "spider", "verse", "man")),
        (42,"Sunflower - Spider-Man: Into the Spider-Verse",Set("spider", "man", "verse")), (472,"Christmas (Baby Please Come Home)",Set("christmas", "baby")),
        (209,"Self Love (Spider-Man: Across the Spider-Verse) (Metro Boomin & Coi Leray)",Set("metro", "man", "spider", "love", "verse")),
        (824,"This Love (Taylorï¿½ï¿½ï¿½s Ve",Set("love","taylor")), (477,"It's Beginning to Look a Lot Like Christmas (with Mitchell Ayres & His Orchestra)",Set("like", "christmas")),
        (463,"Do They Know It's Christmas? - 1984 Version",Set("know", "christmas")),
        (215,"All The Way Live (Spider-Man: Across the Spider-Verse) (Metro Boomin & Future, Lil Uzi Vert)",Set("metro", "future", "man", "spider", "verse")),
        (716,"this is what falling in love feels like",Set("love", "like")), (201,"Annihilate (Spider-Man: Across the Spider-Verse) (Metro Boomin & Swae Lee, Lil Wayne, Offset)",Set("spider", "man", "verse", "metro")))

      assert(res===result)

  test("test and Conjunction 1") :

      val invInd = SpotifyDataAnalysis.createInverseIndex(SpotifyDataAnalysis.getAllWordsWithIndex(List(song1,song2)))
      val result = SpotifyDataAnalysis.andConjunction(List("this", "test", "contains"), invInd)
      assert(result === Set(2))

  test("test and Conjunction 2") :

      val invInd = SpotifyDataAnalysis.createInverseIndex(SpotifyDataAnalysis.getAllWordsWithIndex(List(song1,song2)))
      val result = SpotifyDataAnalysis.andConjunction(List("this", "is"), invInd)
      assert(result === Set(1, 2))


  test("test and Conjunction 3") :

      val invInd = SpotifyDataAnalysis.createInverseIndex(SpotifyDataAnalysis.getAllWordsWithIndex(List(song1,song2)))
      val result = SpotifyDataAnalysis.andConjunction(List("this", "hello"), invInd)
      assert(result === Set())

  test("Test getSongWithHighestEnergyAndDanceability") {
      val result = SpotifyDataAnalysis.getSongWithHighestEnergyAndDanceability(songs)
      assert(result === Song(796, "That That (prod. & feat. SUGA of BTS)", "PSY, Suga", 2, 2022, 4, 29, 802, 0, 212109195, 16, 81, 23, 0, 0, 130, "E", "Major", 91, 91, 96, 3, 0, 3, 9, "https://i.scdn.co/image/ab67616d0000b273b5c128b71507ef309ff4912e")) // Song1 hat die höchste Summe aus Energy + Danceability
  }

  test("Test getSongsWithLivenessAndInstrumentalness") {
    val result = SpotifyDataAnalysis.getSongsWithLivenessAndInstrumentalness(songs)
    assert(result === Song(5, "WHERE SHE GOES", "Bad Bunny", 1, 2023, 5, 18, 3133, 50, 303236322, 84, 133, 87, 15, 425, 144, "A", "Minor", 65, 65, 80, 14, 63, 11, 6, "https://i.scdn.co/image/ab67616d0000b273ab5c9cd818ad6ed3e9b79cd1")) // Erwartung: Keine Songs passen zum Filter
}
}
