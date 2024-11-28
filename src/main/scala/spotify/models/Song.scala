package spotify.models

import java.sql.Timestamp

/**
  * @param track    name of the track
  * @param artist   artist name
  * @param userName number of artists performing under a certain name
  * @param released_year release year
  * @param released_month release month
  * @param released_day release day
  * @param in_spotify_playlists 
  * @param in_spotify_charts
  * @param streams
  * @param in_apple_playlists
  * @param in_apple_charts
  * @param in_deezer_playlists
  * @param in_deezer_charts
  * @param in_shazam_charts
  * @param bpm
  * @param key 
  * @param mode
  * @param danceability
  * @param valence
  * @param energy
  * @param acousticness
  * @param instrumentalness
  * @param liveness
  * @param speechiness 
  * @param cover_url
  *  
  */

case class Song(
                 id: Long,
                 track:String,
                 artist:String,
                 artist_count:Int,
                 released_year:Int,
                 released_month:Int,
                 released_day:Int,
                 in_spotify_playlists: Int,
                 in_spotify_charts: Int,
                 streams: BigInt,
                 in_apple_playlists:Int,
                 in_apple_charts:Int,
                 in_deezer_playlists: Int,
                 in_deezer_charts:Int,
                 in_shazam_charts:Int,
                 bpm:Int,
                 key:String,
                 mode:String,
                 danceability:Int,
                 valence:Int,
                 energy:Int,
                 acousticness:Int,
                 instrumentalness:Int,
                 liveness:Int,
                 speechiness:Int,
                 cover_url:String)



