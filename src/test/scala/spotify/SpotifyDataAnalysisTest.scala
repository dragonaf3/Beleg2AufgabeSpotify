package spotify

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import spotify.models.Song
import spotify.utils.UtilsSpotifyData

class SpotifyDataAnalysisTest extends AnyFunSuite with BeforeAndAfterAll {

  val epsilon = 0.00001
  val songs = UtilsSpotifyData.loadData("SpotifyMostStreamedSongs.csv")
  val s1 = "This is the 88 Test! The result !!!should be: 9 Words"
  val words_s1 = List("be", "is", "result", "should", "test", "the", "the", "this", "words")
  val s2 = "This is another test. It contains a lot of words which are also in string 1."
  val words_s2 = List("a", "also", "another", "are", "contains", "in", "is", "it", "lot", "of", "string", "test", "this", "which", "words")
  val song1 = Song(1L, s1, "artist1", 1, 2019, 11, 29, 43899, 69, 3703895074L, 672, 199, 3421, 20, -1, 171, "C#", "Major", 50, 50, 80, 0, 0, 9, 7, "")
  val song2 = Song(2L, s2, "artist2", 1, 2019, 11, 29, 43899, 69, 3703895074L, 672, 199, 3421, 20, -1, 171, "C#", "Major", 50, 50, 80, 0, 0, 9, 7, "")
  val wordlist = List("a", "also", "another", "are", "be", "contains", "in", "is", "is", "it", "lot", "of", "result", "should", "string",
    "test", "test", "the", "the", "this", "this", "which", "words", "words")
  val music_words = List("track", "feat", "version", "remix", "music", "vol", "sessions", "edit")
  val important_words = List("love", "christmas", "spider", "like", "man", "taylor", "let", "snow", "know", "baby", "metro", "verse", "time", "future", "night", "scott", "die", "take")

  def getWordResources(filename: String): Set[String] =
    val url = getClass.getResource("/" + filename).getPath
    val src = scala.io.Source.fromFile(url)
    val result = src.getLines().foldLeft(Set(): Set[String])((set, el) => set + el)
    src.close()
    result

  // get stopwords from resources in test folder
  val stopwords: Set[String] = getWordResources("stopwords.txt")
  // get wordlist from resources in main folder
  val words: Set[String] = getWordResources("words.txt")

  test("Get most successful song"):
    val result = SpotifyDataAnalysis.getSongWithMostStreams(songs)
    assert(result === Song(56L, "Blinding Lights", "The Weeknd", 1, 2019, 11, 29, 43899, 69, 3703895074L, 672, 199, 3421, 20, -1, 171, "C#", "Major", 50, 50, 80, 0, 0, 9, 7, "https://i.scdn.co/image/ab67616d0000b2738863bc11d2aa12b54f5aeb36"))

  test("Test getNameAndNumberOfTheArtistWithMostSongsInList"):
    val result = SpotifyDataAnalysis.getNameAndNumberOfTheArtistWithMostSongsInList(songs)
    assert(result === ("Taylor Swift", 34))

  test("Test getArtistWithMostStreams"):
    val result = SpotifyDataAnalysis.getArtistWithMostStreams(songs)
    assert(result === ("The Weeknd", 14185552870L))

  test("Get min/max/avg-BPM"):
    val result = SpotifyDataAnalysis.getMinAndMaxAndAvgBPM(songs)
    assert(math.abs(result._3 - 122.54039874081847) < epsilon)
    assert(result._2 == 206 && result._1 == 65)

  test("Get Percental Modes by Month"):
    val expected = List((6, 0.5), (2, 0.4918032786885246), (5, 0.4765625), (1, 0.4701492537313433))
    val result = SpotifyDataAnalysis.getThe4MonthWithMostMinorSongs(songs)
    val z = result.zip(expected).map(x => math.abs(x._1._2 - x._2._2))
    assert(z.forall(_ < epsilon))

  test("Test Word Extraction 1"):
    val r = SpotifyDataAnalysis.getWords(s1)
    assert(r.length === 9)
    assert(r.sorted === words_s1)

  test("Test Word Extraction 2"):
    val r = SpotifyDataAnalysis.getWords(s2)
    assert(r.length === 15)
    assert(r.sorted === words_s2)

  test("get All Words Extraction"):

    val test_list = List(song1, song2)
    val result = SpotifyDataAnalysis.getAllWords(test_list)
    assert(result.length === 24)
    assert(result.sorted === wordlist)

  test("count all Words in song titles"):

    val result = SpotifyDataAnalysis.getAllWords(songs)
    assert(result.length === 2954)

  test("The 4 most frequent words in title"):

    val result = SpotifyDataAnalysis.getThe4MostFrequentWordsInTitle(songs)
    assert(result === List(("the", 78), ("feat", 61), ("with", 46), ("you", 44)))

  test("The 20 most frequent words in title without stopwords"):

    val result = SpotifyDataAnalysis.getThe20MostFrequentWordsInTitleWithFilter(songs, word => !stopwords.contains(word))
    assert(result === List(("feat", 61), ("love", 24), ("la", 20), ("christmas", 17), ("remix", 17), ("version", 16), ("spider", 14), ("like", 14), ("music", 13), ("de", 12), ("man", 11), ("el", 10), ("vol", 10), ("bts", 9), ("remastered", 9), ("sessions", 9), ("bzrp", 9), ("taylor", 9), ("let", 9), ("snow", 8)))

  test("The 20 most frequent words in title with wordlist and stopwords"):

    val result = SpotifyDataAnalysis.getThe20MostFrequentWordsInTitleWithFilter(songs, word => (words.contains(word) && !stopwords.contains(word) && !music_words.contains(word) && word.length > 2))
    assert(result === List(("love", 24), ("christmas", 17), ("spider", 14), ("like", 14), ("man", 11), ("taylor", 9), ("let", 9), ("snow", 8), ("know", 8), ("baby", 8), ("metro", 7), ("verse", 7), ("time", 7), ("future", 6), ("night", 6), ("scott", 6), ("lil", 6), ("die", 6), ("take", 6), ("travis", 6)))

  test("get All Words Extraction with Indizees"):

    val expected = List((1, "be"), (1, "is"), (1, "result"), (1, "should"), (1, "test"), (1, "the"), (1, "this"), (1, "words"),
      (2, "a"), (2, "also"), (2, "another"), (2, "are"), (2, "contains"), (2, "in"), (2, "is"), (2, "it"), (2, "lot"), (2, "of"),
      (2, "string"), (2, "test"), (2, "this"), (2, "which"), (2, "words"))
    val result = SpotifyDataAnalysis.getAllWordsWithIndex(List(song1, song2)).toList.sorted
    assert(result.length === 23)
    assert(result.sorted === expected)

  test("create Inverse Indizees"):

    val expected = Map("test" -> Set(2, 1), "another" -> Set(2), "result" -> Set(1), "it" -> Set(2), "a" -> Set(2),
      "string" -> Set(2), "also" -> Set(2), "should" -> Set(1), "which" -> Set(2), "be" -> Set(1),
      "contains" -> Set(2), "of" -> Set(2), "the" -> Set(1), "this" -> Set(1, 2), "in" -> Set(2), "are" -> Set(2),
      "is" -> Set(1, 2), "lot" -> Set(2), "words" -> Set(2, 1))
    val result = SpotifyDataAnalysis.createInverseIndex(SpotifyDataAnalysis.getAllWordsWithIndex(List(song1, song2)))
    assert(result === expected)

  test("test or Conjunction 1"):

    val invInd: Map[String, Set[Long]] = SpotifyDataAnalysis.createInverseIndex(SpotifyDataAnalysis.getAllWordsWithIndex(List(song1, song2)))
    val result = SpotifyDataAnalysis.orConjunction(List("hello", "test"), invInd)
    assert(result === Set(1L, 2L))

  test("test or Conjunction 2"):

    val invInd = SpotifyDataAnalysis.createInverseIndex(SpotifyDataAnalysis.getAllWordsWithIndex(List(song1, song2)))
    val result = SpotifyDataAnalysis.orConjunction(List("hello", "contains"), invInd)
    assert(result === Set(2))

  test("test or Conjunction 3"):

    val invInd = SpotifyDataAnalysis.createInverseIndex(SpotifyDataAnalysis.getAllWordsWithIndex(List(song1, song2)))
    val result = SpotifyDataAnalysis.orConjunction(List("hello", "bang"), invInd)
    assert(result === Set())

  test("Find Songs with more than two words from wordlist"):

    val res = SpotifyDataAnalysis.findSongsWithAtLeast2WordsFromWordlist(songs, important_words)
    val result = Set((449, "Let It Snow! Let It Snow! Let It Snow!", Set("let", "snow")), (467, "Let It Snow! Let It Snow! Let It Snow!", Set("let", "snow")),
      (850, "Only Love Can Hurt Like This", Set("love", "like")), (446, "It's Beginning To Look A Lot Like Christmas", Set("like", "christmas")),
      (136, "Calling (Spider-Man: Across the Spider-Verse) (Metro Boomin & Swae Lee, NAV, feat. A Boogie Wit da Hoodie)", Set("spider", "man", "verse", "metro")),
      (492, "You Make It Feel Like Christmas (feat. Blake Shelton)", Set("like", "christmas")),
      (239, "Link Up (Metro Boomin & Don Toliver, Wizkid feat. BEAM & Toian) - Spider-Verse Remix (Spider-Man: Across the Spider-Verse )", Set("metro", "spider", "verse", "man")),
      (42, "Sunflower - Spider-Man: Into the Spider-Verse", Set("spider", "man", "verse")), (472, "Christmas (Baby Please Come Home)", Set("christmas", "baby")),
      (209, "Self Love (Spider-Man: Across the Spider-Verse) (Metro Boomin & Coi Leray)", Set("metro", "man", "spider", "love", "verse")),
      (824, "This Love (Taylorï¿½ï¿½ï¿½s Ve", Set("love", "taylor")), (477, "It's Beginning to Look a Lot Like Christmas (with Mitchell Ayres & His Orchestra)", Set("like", "christmas")),
      (463, "Do They Know It's Christmas? - 1984 Version", Set("know", "christmas")),
      (215, "All The Way Live (Spider-Man: Across the Spider-Verse) (Metro Boomin & Future, Lil Uzi Vert)", Set("metro", "future", "man", "spider", "verse")),
      (716, "this is what falling in love feels like", Set("love", "like")), (201, "Annihilate (Spider-Man: Across the Spider-Verse) (Metro Boomin & Swae Lee, Lil Wayne, Offset)", Set("spider", "man", "verse", "metro")))

    assert(res === result)

  test("test and Conjunction 1"):

    val invInd = SpotifyDataAnalysis.createInverseIndex(SpotifyDataAnalysis.getAllWordsWithIndex(List(song1, song2)))
    val result = SpotifyDataAnalysis.andConjunction(List("this", "test", "contains"), invInd)
    assert(result === Set(2))

  test("test and Conjunction 2"):

    val invInd = SpotifyDataAnalysis.createInverseIndex(SpotifyDataAnalysis.getAllWordsWithIndex(List(song1, song2)))
    val result = SpotifyDataAnalysis.andConjunction(List("this", "is"), invInd)
    assert(result === Set(1, 2))


  test("test and Conjunction 3"):

    val invInd = SpotifyDataAnalysis.createInverseIndex(SpotifyDataAnalysis.getAllWordsWithIndex(List(song1, song2)))
    val result = SpotifyDataAnalysis.andConjunction(List("this", "hello"), invInd)
    assert(result === Set())

  test("Test getSongWithHighestEnergyAndDanceability") {
    val result = SpotifyDataAnalysis.getSongWithHighestEnergyAndDanceability(songs)
    assert(result === Song(796, "That That (prod. & feat. SUGA of BTS)", "PSY, Suga", 2, 2022, 4, 29, 802, 0, 212109195, 16, 81, 23, 0, 0, 130, "E", "Major", 91, 91, 96, 3, 0, 3, 9, "https://i.scdn.co/image/ab67616d0000b273b5c128b71507ef309ff4912e")) // Song1 hat die höchste Summe aus Energy + Danceability
  }

  test("Test getSongsWithLivenessAndInstrumentalness") {
    var expected = List(
      Song(5, "WHERE SHE GOES", "Bad Bunny", 1, 2023, 5, 18, 3133, 50, 303236322, 84, 133, 87, 15, 425, 144, "A", "Minor", 65, 65, 80, 14, 63, 11, 6, "https://i.scdn.co/image/ab67616d0000b273ab5c9cd818ad6ed3e9b79cd1"),
      Song(16, "Kill Bill", "SZA", 1, 2022, 12, 8, 8109, 77, 1163093654, 183, 162, 161, 12, 187, 89, "G#", "Major", 64, 64, 73, 5, 17, 16, 4, "https://i.scdn.co/image/ab67616d0000b2730c471c36970b9406233842a5"),
      Song(23, "I Wanna Be Yours", "Arctic Monkeys", 1, 2013, 1, 1, 12859, 110, 1297026226, 24, 98, 582, 2, 73, 135, "", "Minor", 48, 48, 42, 12, 2, 11, 3, "https://i.scdn.co/image/ab67616d0000b2734ae1c4c5c45aabe565499163"),
      Song(54, "(It Goes Like) Nanana - Edit", "Peggy Gou", 1, 2023, 6, 15, 2259, 59, 57876440, 0, 0, 109, 17, 0, 130, "G", "Minor", 67, 67, 88, 12, 19, 8, 4, "https://i.scdn.co/image/ab67616d0000b2739a8459318ff1a68ecfd74522"),
      Song(61, "Tï¿½ï¿", "dennis, MC Kevin o Chris", 2, 2023, 5, 4, 731, 15, 111947664, 27, 17, 73, 4, 167, 130, "B", "Major", 86, 86, 96, 50, 1, 9, 5, "Not Found"),
      Song(73, "golden hour", "JVKE", 1, 2022, 7, 15, 4511, 36, 751134527, 70, 58, 109, 18, 230, 94, "C#", "Minor", 51, 51, 59, 65, 18, 25, 3, "https://i.scdn.co/image/ab67616d0000b273c2504e80ba2f258697ab2954"),
      Song(74, "Sweater Weather", "The Neighbourhood", 1, 2012, 5, 14, 16413, 61, 2282771485L, 166, 87, 1056, 1, -1, 124, "A#", "Major", 61, 61, 81, 5, 2, 10, 3, "https://i.scdn.co/image/ab67616d0000b2738265a736a1eb838ad5a0b921"),
      Song(75, "Quevedo: Bzrp Music Sessions, Vol. 52", "Bizarrap, Quevedo", 2, 2022, 7, 6, 8506, 45, 1356565093, 94, 65, 164, 14, 176, 128, "D", "Major", 62, 62, 78, 1, 3, 23, 4, "https://i.scdn.co/image/ab67616d0000b2731630dd349221a35ce03a0ccf"),
      Song(86, "El Merengue", "Marshmello, Manuel Turizo", 2, 2023, 3, 3, 2114, 44, 223633238, 80, 75, 110, 11, 323, 124, "G#", "Minor", 78, 78, 68, 3, 1, 11, 4, "https://i.scdn.co/image/ab67616d0000b273f404676577626a87d92cf33f"),
      Song(89, "Makeba", "Jain", 1, 2015, 6, 22, 6060, 53, 165484133, 150, 148, 2703, 22, 1451, 116, "D", "Major", 82, 82, 66, 39, 51, 25, 7, "https://i.scdn.co/image/ab67616d0000b27364ba66f8a81c52364e55db50"),
      Song(90, "MONTAGEM - FR PUNK", "Ayparia, unxbected", 2, 2012, 6, 20, 641, 50, 58054811, 1, 52, 8, 0, 1170, 129, "A", "Major", 63, 63, 82, 70, 8, 9, 7, "Not Found"),
      Song(97, "Say Yes To Heaven", "Lana Del Rey", 1, 2023, 3, 17, 2000, 46, 127567540, 49, 105, 63, 1, 0, 100, "F#", "Minor", 49, 49, 35, 71, 9, 11, 3, "https://i.scdn.co/image/ab67616d0000b273aa27708d07f49c82ff0d0dae"),
      Song(113, "LAGUNAS", "Jasiel Nuï¿½ï¿½ez, Peso P", 2, 2023, 6, 22, 58, 18, 39058561, 2, 106, 4, 2, 184, 116, "B", "Major", 77, 77, 62, 33, 1, 15, 3, "Not Found"),
      Song(122, "Miracle (with Ellie Goulding)", "Calvin Harris, Ellie Goulding", 2, 2023, 3, 10, 5120, 48, 211050784, 161, 115, 246, 9, 638, 143, "A", "Major", 64, 64, 87, 4, 4, 8, 4, "https://i.scdn.co/image/ab67616d0000b273c58e22815048f8dfb1aa8bd0"),
      Song(140, "Romantic Homicide", "d4vd", 1, 2022, 7, 20, 2335, 23, 681583126, 82, 55, 50, 0, 9, 132, "F#", "Major", 56, 56, 55, 45, 1, 32, 3, "https://i.scdn.co/image/ab67616d0000b273bd1a52b3d5903ee01c216da0"),
      Song(146, "Stargirl Interlude", "The Weeknd, Lana Del Rey", 2, 2016, 11, 24, 1275, 32, 611700552, 13, 8, 5, 0, 1, 90, "F", "Minor", 59, 59, 48, 38, 5, 10, 11, "https://i.scdn.co/image/ab67616d0000b273a048415db06a5b6fa7ec4e1a"),
      Song(168, "The Night We Met", "Lord Huron", 1, 2015, 2, 2, 18515, 35, 1410088830, 70, 82, 939, 1, 162, 174, "D", "Major", 45, 45, 37, 97, 25, 64, 4, "https://i.scdn.co/image/ab67616d0000b2739d2efe43d5b7ebc7cb60ca81"),
      Song(185, "Apocalypse", "Cigarettes After Sex", 1, 2017, 3, 21, 13091, 17, 841749534, 61, 96, 790, 2, 116, 94, "F", "Major", 37, 37, 47, 2, 46, 11, 3, "https://i.scdn.co/image/ab67616d0000b27312b69bf576f5e80291f75161"),
      Song(229, "Hummingbird (Metro Boomin & James Blake)", "James Blake, Metro Boomin", 2, 2023, 6, 2, 277, 1, 39666245, 1, 20, 5, 0, 1, 81, "F#", "Major", 59, 59, 60, 46, 1, 25, 13, "https://i.scdn.co/image/ab67616d0000b2736ed9aef791159496b286179f"),
      Song(239, "Link Up (Metro Boomin & Don Toliver, Wizkid feat. BEAM & Toian) - Spider-Verse Remix (Spider-Man: Across the Spider-Verse )", "WizKid, Toian, Metro Boomin, Don Toliver, Beam", 5, 2023, 6, 2, 197, 0, 32761689, 3, 10, 3, 0, 0, 101, "F", "Major", 92, 92, 51, 41, 51, 26, 8, "https://i.scdn.co/image/ab67616d0000b2736ed9aef791159496b286179f"),
      Song(272, "PROVENZA", "Karol G", 1, 2022, 4, 21, 6587, 34, 885093467, 114, 104, 147, 11, 20, 111, "C#", "Major", 87, 87, 52, 66, 1, 11, 5, "https://i.scdn.co/image/ab67616d0000b27382de1ca074ae63cb18fce335"),
      Song(281, "Watch This - ARIZONATEARS Pluggnb Remix", "sped up nightcore, ARIZONATEARS, Lil Uzi Vert", 3, 2023, 2, 5, 1638, 10, 207033255, 0, 0, 21, 0, 0, 130, "B", "Minor", 69, 69, 90, 1, 10, 15, 4, "Not Found"),
      Song(284, "Lovers Rock", "TV Girl", 1, 2014, 6, 5, 6339, 13, 466231982, 3, 1, 36, 1, 37, 105, "F", "Minor", 56, 56, 87, 0, 1, 10, 4, "https://i.scdn.co/image/ab67616d0000b273e1bc1af856b42dd7fdba9f84"),
      Song(285, "METAMORPHOSIS", "INTERWORLD", 1, 2021, 11, 25, 1561, 24, 357580552, 18, 78, 24, 0, 30, 175, "G", "Minor", 59, 59, 64, 43, 90, 12, 10, "https://i.scdn.co/image/ab67616d0000b273b852a616ae3a49a1f6b0f16e"),
      Song(308, "I Know - PR1SVX Edit", "Kanii, PR1ISVX", 2, 2023, 3, 24, 407, 0, 77377503, 16, 15, 5, 0, 1, 134, "B", "Minor", 67, 67, 76, 8, 47, 30, 7, "https://i.scdn.co/image/ab67616d0000b273efae10889cd442784f3acd3d"),
      Song(313, "VOID", "Melanie Martinez", 1, 2023, 3, 29, 596, 0, 67070410, 29, 9, 12, 0, 52, 100, "A", "Major", 72, 72, 66, 18, 4, 19, 4, "https://i.scdn.co/image/ab67616d0000b2733c6c534cdacc9cf53e6d2977"),
      Song(327, "Heart To Heart", "Mac DeMarco", 1, 2019, 5, 10, 1640, 0, 244658767, 27, 27, 29, 1, 1, 150, "G#", "Minor", 90, 90, 14, 67, 35, 11, 10, "https://i.scdn.co/image/ab67616d0000b273fa1323bb50728c7489980672"),
      Song(346, "SPIT IN MY FACE!", "ThxSoMch", 1, 2022, 10, 31, 629, 14, 303216294, 32, 3, 9, 0, 0, 94, "G#", "Major", 73, 73, 79, 5, 2, 11, 6, "https://i.scdn.co/image/ab67616d0000b27360ddc59c8d590a37cf2348f3"),
      Song(364, "Vista Al Mar", "Quevedo", 1, 2022, 9, 8, 1769, 34, 362361576, 16, 19, 21, 3, 4, 105, "", "Minor", 76, 76, 56, 80, 12, 10, 13, "https://i.scdn.co/image/ab67616d0000b273efc1b8f6beda4abe848a84e0"),
      Song(372, "After Hours", "The Weeknd", 1, 2020, 2, 19, 8084, 6, 698086140, 45, 115, 218, 1, 221, 109, "F", "Minor", 66, 66, 57, 10, 1, 12, 3, "https://i.scdn.co/image/ab67616d0000b27360884bc925e0ca47e8006996"),
      Song(387, "Shirt", "SZA", 1, 2022, 10, 28, 3469, 0, 309653982, 71, 95, 31, 0, 2, 120, "D#", "Minor", 82, 82, 45, 15, 3, 9, 10, "https://i.scdn.co/image/ab67616d0000b27370dbc9f47669d120ad874ec1"),
      Song(390, "Sex, Drugs, Etc.", "Beach Weather", 1, 2016, 11, 4, 3006, 16, 480507035, 47, 60, 87, 12, 124, 144, "E", "Minor", 57, 57, 84, 1, 1, 52, 4, "https://i.scdn.co/image/ab67616d0000b273a03e3d24ccee1c370899c342"),
      Song(396, "Space Song", "Beach House", 1, 2015, 1, 1, 17852, 4, 789753877, 69, 76, 335, 0, -1, 147, "", "Minor", 51, 51, 79, 22, 13, 14, 3, "https://i.scdn.co/image/ab67616d0000b2739b7190e673e46271b2754aab"),
      Song(400, "TV", "Billie Eilish", 1, 2022, 7, 21, 3009, 2, 338564981, 68, 89, 65, 0, 25, 141, "E", "Minor", 41, 41, 25, 84, 1, 14, 4, "https://i.scdn.co/image/ab67616d0000b2737a4781629469bb83356cd318"),
      Song(409, "After Dark", "Mr.Kitty", 1, 2014, 8, 8, 1776, 14, 646886885, 1, 9, 3, 0, 0, 140, "G#", "Major", 58, 58, 60, 7, 41, 8, 3, "https://i.scdn.co/image/ab67616d0000b273b492477206075438e0751176"),
      Song(414, "PUNTO 40", "Baby Rasta, Rauw Alejandro", 2, 2022, 9, 22, 3006, 12, 304079786, 54, 32, 66, 2, 0, 107, "", "Major", 87, 87, 83, 0, 4, 31, 9, "Not Found"),
      Song(418, "Die For You", "Joji", 1, 2022, 11, 4, 1703, 0, 246390068, 38, 45, 36, 16, 0, 148, "G#", "Major", 47, 47, 52, 38, 13, 29, 5, "https://i.scdn.co/image/ab67616d0000b273eaac2a7955f5b8967991cacb"),
      Song(420, "Gatita", "Bellakath", 1, 2022, 10, 3, 1054, 0, 168684524, 9, 0, 15, 0, 1, 101, "G", "Major", 90, 90, 81, 15, 24, 33, 6, "https://i.scdn.co/image/ab67616d0000b273070c919f062e9fbfc03ca16b"),
      Song(421, "Rumble", "Skrillex, Flowdan, Fred again..", 3, 2022, 1, 17, 2849, 0, 78489819, 39, 45, 27, 0, 1, 140, "C#", "Minor", 81, 81, 84, 5, 23, 6, 6, "https://i.scdn.co/image/ab67616d0000b273352f154c54727bc8024629bc"),
      Song(427, "Limbo", "Freddie Dredd", 1, 2022, 8, 11, 688, 0, 199386237, 14, 1, 17, 0, 2, 75, "B", "Minor", 80, 80, 62, 3, 6, 11, 46, "https://i.scdn.co/image/ab67616d0000b27369b381d574b329409bd806e6"),
      Song(432, "Evergreen (You Didnï¿½ï¿½ï¿½t Deserve Me A", "Omar Apollo", 1, 2022, 4, 8, 2499, 0, 227918678, 70, 0, 49, 0, 0, 82, "A", "Major", 70, 70, 34, 60, 1, 11, 4, "Not Found"),
      Song(440, "Agudo Mï¿½ï¿½gi", "Styrx, utku INC, Thezth", 3, 1930, 1, 1, 323, 0, 90598517, 4, 0, 14, 0, 0, 130, "F#", "Minor", 65, 65, 80, 22, 4, 7, 5, "Not Found"),
      Song(456, "Seek & Destroy", "SZA", 1, 2022, 12, 9, 1007, 0, 98709329, 5, 31, 1, 0, 0, 152, "C#", "Major", 65, 65, 65, 44, 18, 21, 7, "https://i.scdn.co/image/ab67616d0000b27370dbc9f47669d120ad874ec1"),
      Song(463, "Do They Know It's Christmas? - 1984 Version", "Band Aid", 1, 1984, 11, 25, 14169, 0, 481697415, 209, 30, 449, 0, 0, 115, "", "Major", 60, 60, 57, 0, 2, 27, 3, "https://i.scdn.co/image/ab67616d0000b273d549b09f0264901929eaa6e8"),
      Song(464, "Ghost in the Machine (feat. Phoebe Bridgers)", "SZA, Phoebe Bridgers", 2, 2022, 12, 9, 1634, 0, 110073250, 16, 20, 4, 0, 0, 125, "F#", "Major", 62, 62, 43, 84, 3, 11, 5, "Not Found"),
      Song(469, "Open Arms (feat. Travis Scott)", "SZA, Travis Scott", 2, 2022, 12, 8, 1420, 4, 155653938, 13, 87, 17, 0, 46, 78, "A", "Major", 67, 67, 59, 76, 1, 15, 16, "https://i.scdn.co/image/ab67616d0000b27370dbc9f47669d120ad874ec1"),
      Song(483, "SPIT IN MY FACE!", "ThxSoMch", 1, 2022, 10, 31, 573, 0, 301869854, 1, 0, 18, 0, 24, 166, "C#", "Major", 70, 70, 57, 9, 20, 11, 7, "https://i.scdn.co/image/ab67616d0000b27360ddc59c8d590a37cf2348f3"),
      Song(506, "Do It To It", "Cherish, ACRAZE", 2, 2021, 8, 20, 12403, 0, 674772936, 183, 63, 465, 0, 11, 125, "B", "Minor", 85, 85, 81, 2, 5, 7, 9, "Not Found"),
      Song(549, "Todo De Ti", "Rauw Alejandro", 1, 2020, 11, 2, 11975, 8, 1168642797, 188, 75, 268, 6, 16, 128, "D#", "Minor", 81, 81, 63, 40, 1, 10, 4, "https://i.scdn.co/image/ab67616d0000b2734801828052a610b910cff795"),
      Song(550, "Love Nwantiti - Remix", "Ckay, AX'EL, Dj Yo!", 3, 2019, 8, 30, 2696, 0, 540539717, 42, 2, 57, 0, -1, 120, "G#", "Major", 58, 58, 60, 44, 9, 5, 6, "https://i.scdn.co/image/ab67616d0000b27339bb326b58346f99b8692745"),
      Song(564, "pushin P (feat. Young Thug)", "Young Thug, Future, Gunna", 3, 2022, 1, 7, 3517, 0, 311395144, 54, 28, 43, 0, 0, 78, "C#", "Minor", 77, 77, 42, 1, 1, 13, 19, "Not Found"),
      Song(568, "TO THE MOON", "Jnr Choi", 1, 2021, 11, 5, 2979, 0, 245095641, 44, 0, 159, 0, 0, 144, "D", "Major", 74, 74, 65, 5, 1, 11, 35, "https://i.scdn.co/image/ab67616d0000b2739af8418ffb638b3306a07714"),
      Song(578, "Phantom Regret by Jim", "The Weeknd", 1, 2022, 1, 7, 768, 0, 31959571, 1, 1, 3, 0, 0, 108, "A", "Minor", 46, 46, 48, 75, 30, 14, 4, "https://i.scdn.co/image/ab67616d0000b2734ab2520c2c77a1d66b9ee21d"),
      Song(580, "Freaks", "Surf Curse", 1, 2015, 5, 10, 3006, 3, 824420218, 23, 21, 121, 0, 13, 180, "A", "Major", 35, 35, 94, 0, 63, 5, 5, "https://i.scdn.co/image/ab67616d0000b2739efda673310de265a2c1cf1f"),
      Song(582, "Daddy Issues", "The Neighbourhood", 1, 2015, 10, 30, 9771, 4, 1127468248, 42, 70, 384, 0, 3, 85, "A#", "Major", 59, 59, 52, 7, 15, 12, 3, "https://i.scdn.co/image/ab67616d0000b2733066581d697fbdee4303d685"),
      Song(599, "The Business", "Tiï¿½ï¿", 1, 2020, 1, 1, 14311, 0, 1062345656, 255, 32, 582, 0, 14, 120, "G#", "Minor", 80, 80, 62, 41, 2, 11, 23, "Not Found"), Song(607, "Pope Is a Rockstar", "SALES", 1, 2016, 4, 20, 1966, 0, 156658366, 4, 2, 50, 0, 0, 90, "E", "Minor", 73, 73, 45, 85, 24, 11, 3, "https://i.scdn.co/image/ab67616d0000b2731138eea74b6d7e06289bedaa"), Song(634, "Streets", "Doja Cat", 1, 2019, 11, 7, 5728, 0, 865640097, 85, 87, 179, 0, 12, 90, "B", "Major", 75, 75, 46, 21, 4, 34, 8, "https://i.scdn.co/image/ab67616d0000b273f14aa81116510d3a6df8432b"), Song(664, "Formula", "Labrinth", 1, 2019, 10, 4, 3444, 7, 554875730, 24, 85, 102, 0, 1, 145, "B", "Major", 57, 57, 66, 4, 6, 15, 4, "https://i.scdn.co/image/ab67616d0000b27389c39ba1acdf33ed7acd3867"), Song(665, "Mount Everest", "Labrinth", 1, 2019, 6, 21, 5443, 0, 467727006, 45, 1, 80, 0, 12, 89, "", "Minor", 46, 46, 43, 23, 8, 8, 22, "https://i.scdn.co/image/ab67616d0000b273e4c03429788f0aff263a5fc6"), Song(685, "Alien Blues", "Vundabar", 1, 2015, 7, 24, 1930, 0, 370068639, 3, 0, 28, 0, 1, 82, "D#", "Major", 47, 47, 76, 8, 91, 9, 3, "https://i.scdn.co/image/ab67616d0000b273578b0e6109b76bad0821ca71"), Song(687, "Still Don't Know My Name", "Labrinth", 1, 2019, 10, 4, 6332, 0, 563902868, 47, 116, 266, 0, 0, 88, "F", "Major", 31, 31, 63, 47, 27, 21, 12, "https://i.scdn.co/image/ab67616d0000b27389c39ba1acdf33ed7acd3867"), Song(692, "Forever", "Labrinth", 1, 2019, 10, 4, 3618, 0, 282883169, 21, 86, 138, 0, 2, 80, "E", "Minor", 56, 56, 46, 92, 72, 11, 3, "https://i.scdn.co/image/ab67616d0000b27389c39ba1acdf33ed7acd3867"), Song(698, "Something In The Way - Remastered 2021", "Nirvana", 1, 1991, 9, 24, 9514, 0, 368646862, 45, 27, 1197, 0, 43, 106, "G#", "Major", 44, 44, 20, 74, 42, 11, 3, "https://i.scdn.co/image/ab67616d0000b2739aa37e5baca62ca6cc98d056"), Song(704, "Hati-Hati di Jalan", "Tulus", 1, 2022, 3, 3, 200, 2, 202677468, 12, 4, 0, 0, 0, 72, "F#", "Major", 64, 64, 44, 70, 9, 12, 4, "https://i.scdn.co/image/ab67616d0000b273b55d26c578e30129b0a7e86e"), Song(792, "Dua Lipa", "Jack Harlow", 1, 2022, 5, 6, 1992, 0, 150500965, 35, 0, 3, 0, 0, 158, "B", "Major", 83, 83, 65, 0, 10, 11, 8, "https://i.scdn.co/image/ab67616d0000b2738e55edb69ca44a25b52b17bb"), Song(803, "Auntie Diaries", "Kendrick Lamar", 1, 2022, 5, 13, 1545, 0, 37778188, 1, 0, 4, 0, 0, 78, "G", "Major", 43, 43, 38, 76, 1, 48, 38, "https://i.scdn.co/image/ab67616d0000b2732e02117d76426a08ac7c174f"), Song(829, "Music For a Sushi Restaurant", "Harry Styles", 1, 2022, 5, 20, 4449, 1, 334733572, 80, 11, 66, 0, 1, 107, "B", "Major", 72, 72, 72, 26, 6, 11, 4, "https://i.scdn.co/image/ab67616d0000b2732e8ed79e177ff6011076f5f0"), Song(844, "Grapejuice", "Harry Styles", 1, 2022, 5, 20, 1986, 0, 199587884, 7, 1, 15, 0, 0, 183, "F", "Major", 65, 65, 72, 36, 14, 20, 3, "https://i.scdn.co/image/ab67616d0000b2732e8ed79e177ff6011076f5f0"), Song(848, "Cinema", "Harry Styles", 1, 2022, 5, 20, 2171, 0, 189236868, 18, 1, 28, 0, 0, 106, "A", "Minor", 83, 83, 64, 35, 5, 9, 4, "https://i.scdn.co/image/ab67616d0000b2732e8ed79e177ff6011076f5f0"), Song(852, "Daydreaming", "Harry Styles", 1, 2022, 5, 20, 1900, 1, 187703102, 15, 1, 23, 0, 0, 114, "E", "Minor", 71, 71, 81, 31, 2, 13, 3, "https://i.scdn.co/image/ab67616d0000b2732e8ed79e177ff6011076f5f0"), Song(853, "Numb", "Marshmello, Khalid", 2, 2022, 6, 10, 3879, 2, 295307001, 107, 76, 86, 1, 9, 120, "", "Minor", 91, 91, 77, 12, 1, 10, 5, "https://i.scdn.co/image/ab67616d0000b2732ff34dbc50313f8cea7b5db5"), Song(855, "Me and Your Mama", "Childish Gambino", 1, 2016, 11, 10, 8775, 0, 445590495, 33, 60, 107, 1, 0, 118, "F", "Major", 56, 56, 45, 1, 3, 7, 3, "https://i.scdn.co/image/ab67616d0000b2737582716b3666a5235d5af4ea"), Song(857, "SLOW DANCING IN THE DARK", "Joji", 1, 2018, 9, 12, 10211, 0, 1122364376, 38, 79, 65, 0, 1, 89, "D#", "Major", 52, 52, 48, 54, 1, 19, 3, "https://i.scdn.co/image/ab67616d0000b27360ba1d6104d0475c7555a6b2"), Song(863, "Master of Puppets (Remastered)", "Metallica", 1, 1986, 3, 3, 6080, 0, 704171068, 112, 198, 406, 1, 0, 105, "E", "Minor", 54, 54, 83, 0, 44, 20, 4, "https://i.scdn.co/image/ab67616d0000b273cad4832cb7b5844343278daa"), Song(866, "Massive", "Drake", 1, 2022, 6, 17, 5263, 0, 195628667, 66, 89, 61, 0, 11, 125, "E", "Minor", 51, 51, 68, 12, 2, 15, 6, "https://i.scdn.co/image/ab67616d0000b2738dc0d801766a5aa6a33cbe37"), Song(872, "Sweet Child O' Mine", "Guns N' Roses", 1, 1987, 1, 1, 41231, 1, 1553497987, 228, 151, 6720, 3, 99, 125, "F#", "Major", 45, 45, 90, 9, 11, 10, 5, "https://i.scdn.co/image/ab67616d0000b27321ebf49b3292c3f0f575f0f5"), Song(896, "Siempre Pendientes", "Peso Pluma, Luis R Conriquez", 2, 2022, 8, 15, 685, 5, 295152154, 15, 79, 4, 2, 0, 136, "", "Major", 77, 77, 75, 33, 1, 13, 4, "https://i.scdn.co/image/ab67616d0000b273093a2563368d844b2a91306f"), Song(904, "B.O.T.A. (Baddest Of Them All) - Edit", "Interplanetary Criminal, Eliza Rose", 2, 2022, 6, 15, 5153, 6, 244585109, 102, 53, 113, 12, 0, 137, "", "Major", 74, 74, 89, 24, 61, 15, 5, "Not Found"), Song(910, "Static", "Steve Lacy", 1, 2022, 7, 15, 1613, 0, 202452860, 21, 15, 13, 0, 0, 79, "C#", "Major", 34, 34, 31, 43, 63, 10, 7, "https://i.scdn.co/image/ab67616d0000b27368968350c2550e36d96344ee"), Song(912, "Sparks", "Coldplay", 1, 2000, 7, 10, 10826, 4, 624101957, 24, 0, 805, 0, 0, 103, "C#", "Major", 37, 37, 27, 75, 5, 10, 3, "https://i.scdn.co/image/ab67616d0000b2733d92b2ad5af9fbc8637425f0"), Song(916, "Sin Seï¿½ï", "Ovy On The Drums, Quevedo", 2, 2022, 7, 22, 1097, 2, 209106362, 18, 10, 13, 1, 1, 118, "B", "Minor", 82, 82, 85, 33, 1, 11, 4, "Not Found"), Song(918, "Poland", "Lil Yachty", 1, 2022, 6, 23, 1584, 0, 115331792, 38, 0, 24, 0, 0, 150, "F", "Minor", 70, 70, 56, 14, 83, 11, 5, "https://i.scdn.co/image/ab67616d0000b273615d6910181bc514d4c4b011"),
      Song(924, "we fell in love in october", "girl in red", 1, 2018, 11, 21, 6858, 0, 723043854, 31, 21, 15, 0, 4, 130, "G", "Major", 57, 57, 37, 11, 18, 16, 3, "https://i.scdn.co/image/ab67616d0000b273d6839051c4760457e1a60b2a"), Song(939, "Labyrinth", "Taylor Swift", 1, 2022, 10, 21, 1597, 0, 187339835, 6, 3, 15, 0, 0, 110, "", "Major", 48, 48, 31, 80, 22, 12, 4, "https://i.scdn.co/image/ab67616d0000b273bb54dde68cd23e2a268ae0f5"), Song(943, "Con La Brisa", "Ludwig Goransson, Foudeqush", 2, 2022, 11, 4, 486, 0, 71095708, 8, 1, 7, 0, 0, 114, "D", "Minor", 62, 62, 44, 51, 33, 14, 3, "Not Found"), Song(950, "Bigger Than The Whole Sky", "Taylor Swift", 1, 2022, 10, 21, 1180, 0, 121871870, 4, 0, 8, 0, 0, 166, "F#", "Major", 42, 42, 24, 83, 1, 12, 6, "https://i.scdn.co/image/ab67616d0000b273e0b60c608586d88252b8fbc0"))

    val result = SpotifyDataAnalysis.getSongsWithLivenessAndInstrumentalness(songs)
    assert(result === expected)
  }


  test("Get Songs In Shazam Chart By Year") {
    val expected = Map(
      1982 -> 1,
      2014 -> 11,
      1999 -> 2,
      1963 -> 1,
      2015 -> 10,
      1968 -> 1,
      1983 -> 1,
      2023 -> 146,
      1991 -> 2,
      2016 -> 13,
      2022 -> 201,
      1958 -> 1,
      2012 -> 5,
      1987 -> 1,
      2013 -> 7,
      2004 -> 4,
      2019 -> 25,
      2021 -> 61,
      2018 -> 8,
      2020 -> 25,
      2010 -> 4,
      1985 -> 1,
      1992 -> 1,
      2007 -> 1,
      2017 -> 15,
      2002 -> 2,
      1975 -> 1,
      2011 -> 5,
      2000 -> 1,
      2003 -> 1,
      2008 -> 1)

    val result = SpotifyDataAnalysis.getSongsInShazamChartByYear(songs)

    assert(result === expected)
  }

}