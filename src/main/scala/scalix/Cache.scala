import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import java.io.PrintWriter
import scala.io.Source



object Cache extends App :
  var cache = Cache[(String, String), String]("test")

  cache.cache(("tesdzq", "dqdqdzqd"), "efsfsf")
  cache.cache(("dqdqdz", "dqdqdzqd"), "efsfsf")
  cache.cache(("tesdzq", "dqdkoij"), "dqfgdd")

  cache.writeCache()
  cache.loadCache()


class Cache[T, V](name: String) {

  val cacheMap = collection.mutable.Map[T, V]()

  def cache(key: T, value: V): Unit = {
    cacheMap.put(key, value)
  }

  def loadCache(): Unit = {
    val cacheReader = Source.fromFile(name).reader()
    val mapper = JsonMapper.builder().addModule(DefaultScalaModule).build()
    val map = mapper.readValue(cacheReader, new TypeReference[Map[T, V]]{})
    cacheReader.close()
    print(map)
  }

  def writeCache(): Unit = {
    val out = new PrintWriter(name)
    val mapper = JsonMapper.builder().addModule(DefaultScalaModule).build()

    //    for ((key,value) <- cacheMap) out.println(key.toString + ":" + value.toString)

    val map = mapper.writeValue(out, cacheMap)
    out.close()
  }
}
