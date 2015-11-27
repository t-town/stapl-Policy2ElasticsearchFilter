package tom.Policy2Filter.server

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Success, Failure}

import scala.util.parsing.json._

import akka.actor.ActorSystem
import akka.util.Timeout
import akka.pattern.ask

import spray.can.Http
import spray.http._
import HttpMethods._

import akka.io.IO
import akka.actor.ActorSystem

import akka.actor._
import spray.routing._

import tom.Policy2Filter.logic._
import spray.http.Uri._
/*

object ApiActor {
	def props = Props[ApiActor]
}

class ApiActor extends HttpServiceActor with ActorLogging {
	implicit val system: ActorSystem = ActorSystem()
			implicit val timeout: Timeout = Timeout(1500)

			import system.dispatcher // implicit execution context


			val elasticServer = "http://localhost:9200"
			
			def receive = {
	  
			case _: Http.Connected => sender ! Http.Register(self)

			case HttpRequest(requestType,url, headers, entity, _) =>
			sender ! handle(requestType, url,headers,entity)

	    }

	def handle(requestType: HttpMethod,url: Uri,headers: List[HttpHeader],entity:HttpEntity): HttpResponse = {
	  println("1")
		    val relUrl = url.toRelative
				if (! relUrl.toString().endsWith("_search")) {
				  	  println("2")

					return HttpResponse(status = StatusCodes.MethodNotAllowed, entity =relUrl.toString() + " Not Allowed")

				} else {
				  	  println("3")

					//Allowed to search.
					//construct the JSON
					val json:JSONObject = try {
						JSON.parseFull(entity.asString(HttpCharsets.`UTF-8`)) match {
						  case Some(a:Map[String,any]) => JSONObject(a)
						  case None => return HttpResponse(status = StatusCodes.MethodNotAllowed, entity = entity.asString(HttpCharsets.`UTF-8`) + " Not correctly formatted")
						}
					} catch {
					case e: Exception => return HttpResponse(status = StatusCodes.MethodNotAllowed, entity = entity.asString(HttpCharsets.`UTF-8`) + " Not correctly formatted")
					}
					
					val requiredJson:JSONObject = json.obj.get("query") match {
					  case Some(x) => x match {
					                          case x:Map[String,any] => JSONObject(x.get("query"))
					                          case _ => return HttpResponse(status = StatusCodes.MethodNotAllowed, entity = entity.asString(HttpCharsets.`UTF-8`) + " Not correctly formatted")
					                          }
					  case _ => return HttpResponse(status = StatusCodes.MethodNotAllowed, entity = entity.asString(HttpCharsets.`UTF-8`) + " Not correctly formatted: No json")
					}

		    //We have the original JsonQuery
		    //calculate the JSonQuery for the policy;
		    val filter:JSONObject = Policy2Filter.toFilter()
		    	  println("5")

				//val query = Map("query"->Map("filtered"->Map("query"->json,"filter"->filter))).toJson
				val query = JSONObject(Map("query"->JSONObject(Map("filtered"->JSONObject(Map("query"->requiredJson,"filter"->filter))))))
					  println("6")

				//Now we delegate this to the real elasticsearch server
				val server: Uri = Uri(elasticServer + relUrl.toString)
				println("Server URL: " + server)
				println(query)
				val response = (IO(Http) ? HttpRequest(requestType,server,headers.filter(filterHeader),entity=query.toString)).mapTo[HttpResponse]
				val result:HttpResponse = Await.result(response,10 seconds)
				return HttpResponse(entity = result.entity)

				}
	}
	
	def filterHeader(header:HttpHeader): Boolean = {
	  header match {
	    case x: HttpHeaders.Host => false
	    case x: HttpHeaders.`User-Agent` => false
	    case x: HttpHeaders.`Content-Length` => false
	    case x: HttpHeaders.`Content-Type` => false
	    case _ => true
	  }
	}
	
	def parsetoJson(json: Map[String,Any]):JSONType {
	  json match {
	    case x:Map[String,any] => JSONObject(parsetoJson())
	  }
	}
	


}
*/
