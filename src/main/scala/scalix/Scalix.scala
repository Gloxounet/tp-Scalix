package scalix

import com.fasterxml.jackson.databind.json.JsonMapper
import org.json4s.*
import org.json4s.native.JsonMethods.*

import scala.::
import scala.collection.mutable.ListBuffer
import scala.io.Source
import java.io.File
import scala.collection.mutable
import scala.reflect.{ClassTag, classTag}

import scala.reflect._


implicit val formats: Formats = DefaultFormats

import os.{GlobSyntax, /}

import com.fasterxml.jackson.annotation.JsonSubTypes.Type
import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo, JsonTypeName}
import com.fasterxml.jackson.annotation.JsonTypeInfo.Id
import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.databind.annotation.JsonDeserialize
import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper, SerializationFeature}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.ClassTagExtensions

import java.io.PrintWriter


val TMDB_key = "5403e7cc8fc7f3e6c2b4d08888ce6cd8"
val auth = "api_key=" + TMDB_key
val TMB_URL = "https://api.themoviedb.org/3"


val actorCache = Cache[Actor]("actorCache")
val crewCache = Cache[Crew]("crewCache")
val movieCache = Cache[Movie]("movieCache")

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

case class Movie(
                  id: Int,
                  original_title: String,
                  title: String,
                )

case class FullName(name: String, surname: String)


// ---------------------------------------------------------------- REQUEST ----------------------------------------------------------------
def request(path: String, queries: Array[String] = Array()): Option[JValue] =
  val args = queries.mkString("&")
  var query = TMB_URL + path + "?" + auth
  if queries.length > 0 then query += "&" + args

  try
    val source = Source.fromURL(query)
    Some(parse(source.mkString))
  catch
    case _: Exception => None
// ------------------------------------------------------------ QUERY --------------------------------------------------------------------------

def findActorId(name: FullName): Option[Int] =
  val cachedResults = actorCache.cache.filter(a =>
    a.name.equals(name.name + " " + name.surname))
  if cachedResults.nonEmpty then
    print("found in cache " + cachedResults.mkString)
    Some(cachedResults.head.id)

  val response = request("/search/person", queries = Array(s"query=${name.name}+${name.surname}"))
  if response.isEmpty then return None

  val jvalue = response.get

  val results = (jvalue \ "results").extract[List[Actor]]

  val actorId = results.filter(a =>
    a.name.equals(name.name + " " + name.surname))

  if actorId.nonEmpty then
    actorCache.cache(actorId.head)
    Some(actorId.head.id)
  else None

def findActorMovies(actorId: Int): Set[(Int, String)] =
  val response = request(path = s"/person/${actorId}/movie_credits")
  if response.isEmpty then return Set()

  val jvalue = response.get
  val cast = (jvalue \\ "cast").extract[List[Movie]]
  cast.foreach(m => movieCache.cache(m))
  cast.map(x => (x.id, x.title)).toSet

def findMovieDirector(movieId: Int): Option[(Int, String)] =
  val response = request(path = s"/movie/${movieId}/credits")
  if response.isEmpty then return None

  val jvalue = response.get
  val crew = (jvalue \ "crew").extract[List[Crew]]

  val director = crew.filter(c => c.job.equals("Director"))

  if director.nonEmpty then
    crewCache.cache(director.head)
    Some((director.head.id, director.head.name))
  else None


def collaboration(actor1: FullName, actor2: FullName): Set[(Int, String)] =
  val actor1_id: Int = findActorId(actor1) match {
    case Some(id) => id;
    case None => return Set((404, "actor1 not found"))
  };
  val actor2_id: Int = findActorId(actor2) match {
    case Some(id) => id;
    case None => return Set((404, "actor2 not found"))
  };

  val actor1_movies = findActorMovies(actor1_id)
  val actor2_movies = findActorMovies(actor2_id)

  actor1_movies & actor2_movies


object Scalix extends App :

  actorCache.loadCache()
  crewCache.loadCache()
  movieCache.loadCache()

  println("---Get Brad Pit ID---")
  println(findActorId(FullName(name = "Brad", surname = "Pitt")))

  println("---Get Brad Pit Movies---")
  println(findActorMovies(287))

  println("---Get Movie Director of movie 523---")
  println(findMovieDirector(movieId = 523))

  println("---Get Collaboration Between Brad Pit And George Clooney---")
  println(collaboration(FullName(name = "Brad", surname = "Pitt"), FullName(name = "George", surname = "Clooney")))


  actorCache.writeCache()
  crewCache.writeCache()
  movieCache.writeCache()

class Cache[T: ClassTag](name: String) {

  val cache: ListBuffer[T] = ListBuffer[T]()
  val folder = "data"

  private val mapper = new ObjectMapper().registerModule(DefaultScalaModule)

  def cache(value: T): Unit = {
    if !cache.contains(value) then
      cache.append(value)
  }

  def loadCache(): Unit = {
    if File(folder + name).exists() then {
      //val cacheReader = Source.fromFile(folder + name).reader()
      //print(os.pwd / folder / name)
      val jsonString = os.read(os.pwd / folder / name)
      val data = ujson.read(jsonString)

      val mapper = JsonMapper.builder().addModule(DefaultScalaModule).build() :: ClassTagExtensions

      for(line <- data.arr) {
        cache.append(mapper.readValue[T](line.toString, classTag[T].runtimeClass.asInstanceOf[Class[T]]))
      }
      //val values = mapper.readValue[T](values.get(), classTag[T].runtimeClass.asInstanceOf[Class[T]])

      //val values = upickle.default.read[List[T]](data)
      //implicit val authorRW = upickle.default.macroRW[Author]
      //val mapper = JsonMapper.builder().addModule(DefaultScalaModule).build()
      //val values = mapper.readValue[ListBuffer[T]](cacheReader,  new TypeReference[ListBuffer[T]] {})

      //values.foreach(e => cache.addOne(mapper.convertValue(e, new TypeReference[T])))
      //print(values)
      //cache.foreach(e => e = mapper.convertValue(e, classTag[T].runtimeClass))
      //cacheReader.close()
    } else {
      println("No cache file found for " + name)
    }
  }

  def writeCache(): Unit = {
    val out = new PrintWriter(folder + name)
    val mapper = JsonMapper.builder().addModule(DefaultScalaModule).build()
    mapper.writeValue(out, cache.toList)
    out.close()
  }
}
