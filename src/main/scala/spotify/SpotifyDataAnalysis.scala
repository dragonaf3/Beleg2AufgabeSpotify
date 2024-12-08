package spotify

import spotify.models.Song

object SpotifyDataAnalysis {

  /**
   * Gibt das Lied mit den meisten Streams zurück.
   * Benutzt nur eine Aggregationsfunktion.
   */
  def getSongWithMostStreams(l: List[Song]): Song = {
    l.maxBy(_.streams)
  }

  /**
   * Gibt den Namen des Künstlers mit den meisten Songs und die Anzahl der Songs zurück.
   */
  def getNameAndNumberOfTheArtistWithMostSongsInList(l: List[Song]): (String, Int) = {
    l.groupBy(_.artist) // Gruppiert Songs nach Künstler
      .view.mapValues(_.size) // Zählt die Anzahl der Songs pro Künstler
      .toMap.maxBy(_._2) // Findet den Künstler mit den meisten Songs
  }

  /**
   * Gibt den Künstler mit den meisten Streams insgesamt zurück.
   * Rückgabewert ist ein Tupel mit dem Künstlernamen und der Anzahl der Streams.
   */
  def getArtistWithMostStreams(l: List[Song]): (String, BigInt) = {
    l.groupBy(_.artist) // Gruppiert Songs nach Künstler
      .view.mapValues(songs => songs.map(_.streams).sum) // Summiert die Streams für jeden Künstler
      .toMap.maxBy(_._2) // Findet den Künstler mit der höchsten Gesamtanzahl an Streams
  }


  /**
   * Gibt das Minimum, Maximum und den Durchschnitt der BPM-Werte zurück.
   */
  def getMinAndMaxAndAvgBPM(l: List[Song]): (Int, Int, Double) = {
    val bpms = l.map(_.bpm) // Extrahiert die BPM-Werte aus der Songliste
    (bpms.min, bpms.max, bpms.sum.toDouble / bpms.size) // Berechnet Min, Max und Durchschnitt
  }

  /**
   * Gibt die 4 Monate mit den meisten Songs im Moll-Modus (relativ) zurück.
   */
  def getThe4MonthWithMostMinorSongs(l: List[Song]): List[(Int, Double)] = {
    val totalSongsPerMonth = l.groupBy(_.released_month).view.mapValues(_.size).toMap
    val minorSongsPerMonth = l.filter(_.mode.toLowerCase == "minor").groupBy(_.released_month).view.mapValues(_.size).toMap

    minorSongsPerMonth.map {
        case (month, count) => (month, count.toDouble / totalSongsPerMonth.getOrElse(month, 1))
      }
      .toList.sortBy(-_._2).take(4) // Sortiert absteigend und nimmt die Top 4
  }

  /**
   * Extrahiert alle Wörter aus einer Zeile.
   * - Entfernt alle Nicht-Buchstaben
   * - Wandelt Wörter in Kleinbuchstaben um
   * - Gibt eine Liste mit den extrahierten Wörtern zurück
   */
  def getWords(line: String): List[String] = {
    line.toLowerCase.replaceAll("[^a-z]", " ").split("\\s+").filter(_.nonEmpty).toList
  }

  /**
   * Extrahiert alle Wörter aus einer Liste von Songs.
   * Die Wörter behalten ihre Reihenfolge aus der ursprünglichen Quelle.
   */
  def getAllWords(l: List[Song]): List[String] = {
    l.flatMap(song => getWords(song.track)) // Wendet getWords auf alle Titel an und flacht die Liste
  }


  /**
   * Gibt die 4 häufigsten Wörter in den Titeln einer Songliste zurück.
   */
  def getThe4MostFrequentWordsInTitle(l: List[Song]): List[(String, Int)] = {
    getAllWords(l)
      .groupBy(identity) // Gruppiert Wörter
      .view.mapValues(_.size) // Zählt die Häufigkeit
      .toMap.toList.sortBy(-_._2).take(4) // Sortiert absteigend und nimmt die Top 4
  }


  /**
   * Gibt die 20 häufigsten Wörter in Titeln zurück, gefiltert durch eine Bedingung.
   */
  def getThe20MostFrequentWordsInTitleWithFilter(l: List[Song], predicate: String => Boolean): List[(String, Int)] = {
    getAllWords(l)
      .filter(predicate) // Filtert die Wörter basierend auf der Bedingung
      .groupBy(identity) // Gruppiert Wörter
      .view.mapValues(_.size) // Zählt die Häufigkeit
      .toMap.toList.sortBy(-_._2).take(20) // Sortiert absteigend und nimmt die Top 20
  }

  /**
   * Extrahiert alle Wörter der Titel und gibt Paare mit Song-ID zurück.
   */
  def getAllWordsWithIndex(l: List[Song]): Set[(Long, String)] = {
    l.flatMap(song => getWords(song.track).map(word => (song.id, word))).toSet
  }

  /**
   * Erstellt einen inversen Index: Wörter als Schlüssel und Song-IDs als Werte.
   */
  def createInverseIndex(wwi: Set[(Long, String)]): Map[String, Set[Long]] = {
    wwi.groupBy(_._2).view.mapValues(_.map(_._1).toSet).toMap
  }

  /**
   * Gibt die Song-IDs zurück, in denen mindestens eines der Wörter vorkommt.
   */
  def orConjunction(words: List[String], invInd: Map[String, Set[Long]]): Set[Long] = {
    words.flatMap(word => invInd.getOrElse(word, Set.empty)).toSet
  }

  /**
   * Gibt die Song-IDs zurück, in denen alle Wörter vorkommen.
   */
  def andConjunction(words: List[String], invInd: Map[String, Set[Long]]): Set[Long] = {
    words.map(word => invInd.getOrElse(word, Set.empty)).reduce(_ intersect _)
  }

  /**
   * Findet alle Lieder mit mindestens zwei Wörtern der Wortliste. Gibt Tripel zurück, die die ID des Liedes, den Titel und
   * die Wörter, die im Titel vorkommen.
   */
  def findSongsWithAtLeast2WordsFromWordlist(l: List[Song], words: List[String]): Set[(Long, String, Set[String])] = {
    l.flatMap { song =>
      val wordsInTitle = getWords(song.track).toSet
      val matchingWords = wordsInTitle.intersect(words.toSet)
      if (matchingWords.size >= 2) Some((song.id, song.track, matchingWords))
      else None
    }.toSet
  }

  def getSongWithHighestEnergyAndDanceability(songs: List[Song]): Song = {
    songs.maxBy(song => song.energy.toDouble + song.danceability.toDouble)
  }

  def getSongsWithLivenessAndInstrumentalness(songs: List[Song]): List[Song] = {
       songs.filter(song => song.liveness > 0 && song.instrumentalness > 0)
  }

  /*

  Write three own functions that analysis the dataset! Write the functions with some explanations and corresponding tests!

   */
}


