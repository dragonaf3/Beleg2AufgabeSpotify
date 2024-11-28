package spotify

import org.scalatest.funsuite.AnyFunSuite
import spotify.models.Song
import spotify.utils.UtilsSpotifyData
class SpotifyUtilsTest extends AnyFunSuite :

  test("Read Song Data") :
    val s= """LALA,Myke Towers,1,2023,3,23,1474,48,133716286,48,126,58,14,382,92,C#,Major,71,61,74,7,0,10,4,https://i.scdn.co/image/ab67616d0000b2730656d5ce813ca3cc4b677e05"""
    val result= UtilsSpotifyData.readData(s,1)
    assert(result===Some(Song(1,"LALA","Myke Towers",1,2023,3,23,1474,48,133716286,48,126,58,14,382,92,"C#","Major",71,71,74,7,0,10,4,"https://i.scdn.co/image/ab67616d0000b2730656d5ce813ca3cc4b677e05")))


  test("Read Song Data 2") :

    val s = """"Peso Pluma: Bzrp Music Sessions, Vol. 55","Bizarrap, Peso Pluma",2,2023,5,31,1313,40,200647221,17,152,32,11,139,133,F,Minor,85,81,67,26,0,12,5,https://i.scdn.co/image/ab67616d0000b27315583045b2fdb7d7bab10e81"""
    val result = UtilsSpotifyData.readData(s, 1)
    assert(result===Some(Song(1,"Peso Pluma: Bzrp Music Sessions, Vol. 55","Bizarrap, Peso Pluma",2,2023,5,31,1313,40,200647221,17,152,32,11,139,133,"F","Minor",85,85,67,26,0,12,5,"https://i.scdn.co/image/ab67616d0000b27315583045b2fdb7d7bab10e81")))


  test("Read Song Data 3") :

    val s = """As It Was,Harry Styles,1,2022,3,31,23575,130,2513188493,403,198,863,46,,174,F#,Minor,52,66,73,34,0,31,6,https://i.scdn.co/image/ab67616d0000b2732e8ed79e177ff6011076f5f0"""
    val result = UtilsSpotifyData.readData(s, 1)
    assert(result===Some(Song(1,"As It Was","Harry Styles",1,2022,3,31,23575,130,2513188493L,403,198,863,46,-1,174,"F#","Minor",52,52,73,34,0,31,6,"https://i.scdn.co/image/ab67616d0000b2732e8ed79e177ff6011076f5f0")))


  test("Read Song Data 4") :

    val s = """"What Was I Made For? [From The Motion Picture ""Barbie""]",Billie Eilish,1,2023,7,13,873,104,30546883,80,227,95,24,1173,78,,Major,44,14,9,96,0,10,3,Not Found"""
    val result = UtilsSpotifyData.readData(s, 1)
    assert(result===Some(Song(1,"""What Was I Made For? [From The Motion Picture ""Barbie""]""","Billie Eilish",1,2023,7,13,873,104,30546883,80,227,95,24,1173,78,"","Major",44,44,9,96,0,10,3,"Not Found")))


  test("Read Song Data 5") :

    val s = """Love Grows (Where My Rosemary Goes),Edison Lighthouse,1,1970,1,1,2877,0,-1,16,0,54,0,0,110,A,Major,53,75,69,7,0,17,3,https://i.scdn.co/image/ab67616d0000b2739a0011cc9d31cf969b656905"""
    val result = UtilsSpotifyData.readData(s, 1)
    assert(result===Some(Song(1,"Love Grows (Where My Rosemary Goes)","Edison Lighthouse",1,1970,1,1,2877,0,-1,16,0,54,0,0,110,"A","Major",53,53,69,7,0,17,3,"https://i.scdn.co/image/ab67616d0000b2739a0011cc9d31cf969b656905")))


  test("Read all Songs") :
    val songs= UtilsSpotifyData.loadData("SpotifyMostStreamedSongs.csv")
    assert(songs.size===953)
