package scalix

import org.json4s._
import org.json4s.native.JsonMethods._

import scala.io.Source

implicit val formats: Formats = DefaultFormats

import java.io.PrintWriter


val TMDB_key = "5403e7cc8fc7f3e6c2b4d08888ce6cd8"
val auth = "api_key=" + TMDB_key
val TMB_URL = "https://api.themoviedb.org/3"


def request(path: String,queries:Array[String]=Array()): Option[JValue] =
  val args = queries.mkString("&")
  var query = TMB_URL + path + "?" + auth
  if queries.length > 0 then query += "&" + args

  println("Making request to " + query)

  try
    val source = Source.fromURL(query)
    Some(parse(source.mkString))
  catch
    case _:Exception => None




def findActorId(name: String, surname : String): Option[Int] =
  val response = request("/search/person",queries=Array(s"query=${name}+${surname}"))
  if response.isEmpty then return None

  val jvalue = response.get
  val results = (jvalue \ "results").extract[JArray]
  if results.values.nonEmpty then Some((results(0) \ "id").extract[Int]) else None

def findMovieDirector(movieId: Int): Option[(Int, String)] =
  val response = request(path=s"/movie/${movieId}/credits")
  if response.isEmpty then return None

  val jvalue = response.get
  val crew = (jvalue \ "crew")
  val director = crew.filter {
    case s => println(s);true
  }
  if director.isEmpty then return None

  None
//  val d_id = (director \ "id").extract[Int]
//  val d_name = (director \ "name").extract[String]
//  Some((d_id,d_name))



//def collaboration(actor1: String, actor2: String): Set[(String, String)] =
//  val response = request("/search/multi?query")

object Scalix extends App :
  println(findActorId(name="Brad",surname="Pitt"))
  println(findMovieDirector(movieId=523))
