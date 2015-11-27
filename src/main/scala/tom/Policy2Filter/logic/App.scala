package tom.Policy2Filter.logic


import spray.json._
import DefaultJsonProtocol._ 

/**
 * @authorimport tom.Policy2Filter.logic.PolicyFilter
 ${user.name}
 */
object App {

	def main(args : Array[String]) {
	  val x = """{
  "query": {
    "term": {
      "origin": "1"
    }
  }
}"""
	  val j = x.parseJson
	  val obj = j.asJsObject
	  val obj2 = obj.fields.get("query").get; // some
	  println(obj2)
	  val y = (Map("bool"->Map("must_not"->"lulz"))).toJson
	  println(y)
	  
	}
}
