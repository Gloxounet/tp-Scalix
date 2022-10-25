package scalix

import org.json4s._
import org.json4s.native.JsonMethods._

import scala.io.Source

implicit val formats: Formats = DefaultFormats

import java.io.PrintWriter


val TMDB_key = "5403e7cc8fc7f3e6c2b4d08888ce6cd8"
val auth = "api_key=" + TMDB_key
val TMB_URL = "https://api.themoviedb.org/3"

// ---------------------------------------------------------------- MODELS ----------------------------------------------------------------
case class Crew(
                adult: Boolean,
                gender: Int,
                id: Int,
                known_for_department: String,
                name: String,
                original_name: String,
                popularity: Double,
                profile_path: String,
                credit_id: String,
                department: String,
                job: String
                )

case class Actor(
                adult: Boolean,
                gender: Int,
                id: Int,
                known_for_department: String,
                name: String,
                popularity: Double,
                profile_path: String
                )

case class Cast (
                 id: Int,
                 original_title: String,
                 title: String,
               )

// ---------------------------------------------------------------- REQUEST ----------------------------------------------------------------
def request(path: String,queries:Array[String]=Array()): Option[JValue] =
  val args = queries.mkString("&")
  var query = TMB_URL + path + "?" + auth
  if queries.length > 0 then query += "&" + args

  try
    val source = Source.fromURL(query)
    Some(parse(source.mkString))
  catch
    case _:Exception => None

// ------------------------------------------------------------ QUERY --------------------------------------------------------------------------

def findActorId(name: String, surname : String): Option[Int] =
  val response = request("/search/person",queries=Array(s"query=${name}+${surname}"))
  if response.isEmpty then return None

  val jvalue = response.get

  val results = (jvalue \ "results").extract[List[Actor]]
  if results.nonEmpty then return Some(results.head.id);
  None

def findActorMovies(actorId: Int): Set[(Int, String)] =
  val response = request(path=s"/person/${actorId}/movie_credits")
  if response.isEmpty then return Set()

  val jvalue = response.get
  val cast = (jvalue \\ "cast").extract[List[Cast]]
  cast.map(x => (x.id, x.title)).toSet

def findMovieDirector(movieId: Int): Option[(Int, String)] =
  val response = request(path=s"/movie/${movieId}/credits")
  if response.isEmpty then return None

  val jvalue = response.get
  val crew = (jvalue \ "crew").extract[List[Crew]]

  val director = crew.filter(c => c.job.equals("Director"))

  if (director.nonEmpty) {
    return Some((director.head.id,director.head.name))
  }
  return None

case class FullName (name: String, surname: String)


def collaboration(actor1: FullName, actor2: FullName): Set[(Int, String)] =
  val actor1_id:Int = findActorId(actor1.name,actor1.surname) match { case Some(id) => id; case None => return Set((404,"actor1 not found"))};
  val actor2_id:Int = findActorId(actor2.name,actor2.surname) match { case Some(id) => id; case None => return Set((404,"actor2 not found"))};

  val actor1_movies = findActorMovies(actor1_id)
  val actor2_movies = findActorMovies(actor2_id)

  actor1_movies & actor2_movies



//def collaboration(actor1: String, actor2: String): Set[(String, String)] =
//  val response = request("/search/multi?query")

object Scalix extends App :
  println("---Get Brad Pit ID---")
  println(findActorId(name="Brad",surname="Pitt"))

  println("---Get Brad Pit Movies---")
  println(findActorMovies(287))

  println("---Get Movie Director of movie 523---")
  println(findMovieDirector(movieId=523))

  println("---Get Collaboration Between Brad Pit And George Clooney---")
  println(collaboration(FullName(name="Brad",surname="Pit"),FullName(name="George",surname="Clooney")))
