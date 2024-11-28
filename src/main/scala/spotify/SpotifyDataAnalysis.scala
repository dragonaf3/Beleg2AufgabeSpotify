package spotify

import spotify.models.Song

object SpotifyDataAnalysis {

  def getSongWithMostStreams(l:List[Song]):Song=
    /*
      The functions gets the most streamed song
      Use only an aggregation function
     */

   ???

  def getNameAndNumberOfTheArtistWithMostSongsInList(l:List[Song]):(String,Int)=

    /*
      Get the Name of the Artist with the most Songs in the streaming list
      and the number of occurences
     */

    ???

  def getArtistWithMostStreams(l: List[Song]): (String, BigInt) =

    /*
       Gets the Artist with the most streams in total
       and the number of streams
     */

    ???

  def getMinAndMaxAndAvgBPM(l:List[Song]):(Int,Int,Double)=

    /*
      Gets the minimum, maximum and avg BPM of all songs
      Use only one aggregation function to calculate the values
     */
    ???

  def getThe4MonthWithMostMinorSongs(l:List[Song]):List[(Int,Double)]=

    /*
      Gets the 4 months (release date) with the most songs written in minor
      (use the relative values not the absolute)
     */
    ???

  def getWords(line: String): List[String] =
    /*
     * Extracts all words from a line
     *
     * 1. Removes all characters which are not letters (A-Z or a-z)
     * 2. Shifts all words to lower case
     * 3. Extracts all words and put them into a list of strings
     */
    ???


  def getAllWords(l: List[Song]): List[String] =
    /*
     * Extracts all words from a List of Songs
     * The words should be in the same order as they occur in the source document
     *
     * Hint: Use the flatMap function
     */
    ???

  def getThe4MostFrequentWordsInTitle(l:List[Song]):List[(String,Int)]=
    /*
      Gets the 4 most frequent words that occur in the titles of a songlist.
     */
   ???


  def getThe20MostFrequentWordsInTitleWithFilter(l:List[Song], predicate: String=>Boolean):List[(String,Int)]=
    /*
      Get the 20 most frequent words occurring in the titles of a songlist
      Integrate a filter predicate that decides whether a word is in the resulting list or not.
      Give back tuples with the word and the number of occurrences.
     */

    ???

  def getAllWordsWithIndex(l:List[Song]):Set[(Long,String)] =
    /*
      Êxtract all words of the titles of a songlist and give back tuples containing
      the ID of the song and the word
     */

    ???

  def createInverseIndex(wwi:Set[(Long, String)]):Map[String,Set[Long]]=

    /*
      Create an inverse index consisting of all words as the key element and
      the IDs of all songs that contains the certain word as the value.
      The function gets all words with the index (see function above)
      and returns a map.
     */
    ???

  def orConjunction(words: List[String], invInd: Map[String, Set[Long]]): Set[Long] =
    /*
    * The Functions gets a list of words and returns a set of tweet ids where at least one
    * of the word occurs
    * Use the inverse index for calculating the or-Operation.
     */
    ???

  def andConjunction(words: List[String], invInd: Map[String, Set[Long]]): Set[Long] =

    /*
   * The Functions gets a list of words and returns a set of tweet ids where all of the words occur
  * Use the inverse index for calculating the and-Operation.
    */
    ???

  def findSongsWithAtLeast2WordsFromWordlist(l:List[Song], words:List[String]):Set[(Long,String,Set[String])]=

    /*
      Find all songs with at least two words of the wordlist. Return triples containing the ID of the song, the title and
      the words that occur in the title.
    */
    ???


    /*

    Write three own functions that analysis the dataset! Write the functions with some explanations and corresponding tests!

     */
}


