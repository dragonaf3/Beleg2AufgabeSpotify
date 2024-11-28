package spotify.utils

import spotify.models.Song

import java.io.{InputStream, PrintWriter}
import scala.io.{BufferedSource, Source}
import scala.util.Try

object UtilsSpotifyData {

  // Reading text file
  // Stores the information in a map consisting of a property name (key) and its value
  def loadData(filename: String): List[Song] =

    val src =Source.fromFile(filename)
    val iter: Iterator[(String, Int)] = src.getLines().zipWithIndex
    iter.next // skip first line
    val result: List[Option[Song]] = (for (row <- iter) yield readData(row._1, row._2)).toList
    src.close
    result.flatMap(_ match { case p: Option[Song] => p })

  // Extracting all information storing it into the model Song
  def readData(line: String, id: Int): Option[Song] =

    val (song, rest) = readString(line)
    val (artist,rest2)= readString(rest)
    val result: Array[String] = rest2.split(",")
    if result.size != 23 then
      println("Number of data does not fit! "+ result.size); None
    else try
      Some(
        Song(
          id= id.toLong,
          track = song,
          artist = artist,
          artist_count = readIntWithDefault(result(0)),
          released_year = readIntWithDefault(result(1)),
          released_month = readIntWithDefault(result(2)),
          released_day = readIntWithDefault(result(3)),
          in_spotify_playlists = readIntWithDefault(result(4)),
          in_spotify_charts = readIntWithDefault(result(5)),
          streams = BigInt(result(6)),
          in_apple_playlists = readIntWithDefault(result(7)),
          in_apple_charts = readIntWithDefault(result(8)),
          in_deezer_playlists = readIntWithDefault(result(9)),
          in_deezer_charts = readIntWithDefault(result(10)),
          in_shazam_charts = readIntWithDefault(result(11)),
          bpm = readIntWithDefault(result(12)),
          key = result(13),
          mode = result(14),
          danceability = readIntWithDefault(result(15)),
          valence = readIntWithDefault(result(15)),
          energy = readIntWithDefault(result(17)),
          acousticness = readIntWithDefault(result(18)),
          instrumentalness = readIntWithDefault(result(19)),
          liveness = readIntWithDefault(result(20)),
          speechiness = readIntWithDefault(result(21)),
          cover_url = result(22))
      )
    catch
      case e => println("Data Failure: " +e.toString); None


  def readIntWithDefault(s:String):Int= if (s.isEmpty) -1 else s.toInt
  def readString(line:String):(String,String)={

    // splits the first string in the line
    if line.startsWith(""""""") then
      val (start,end)=line.tail.splitAt(line.tail.indexOf("""","""))
      val rest= if (end.length<2) "" else end.tail.tail // eliminates next ','
      (start,rest)
    else
      val (start,end)= line.splitAt(line.indexOf(','))
      val rest = if (end.isEmpty) "" else end.tail // eliminates next ','
      (start,rest)
  }
}
  