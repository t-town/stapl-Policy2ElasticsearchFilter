package tom.Policy2Filter.server

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Success, Failure}

import spray.json._
import DefaultJsonProtocol._ 

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
		    val relUrl = url.toRelative
				if (! relUrl.toString().endsWith("_search")) {
					return HttpResponse(status = StatusCodes.MethodNotAllowed, entity =relUrl.toString() + " Not Allowed")

				} else {

					//Allowed to search.
					//construct the JSON
					val json:JsObject = try {
						entity.asString(HttpCharsets.`UTF-8`).parseJson match {
						  case x:JsValue => x.asJsObject.fields.get("query").get.asJsObject
						  case _ => return HttpResponse(status = StatusCodes.MethodNotAllowed, entity = entity.asString(HttpCharsets.`UTF-8`) + " Not correctly formatted")
						}
					} catch {
					case e: Exception => return HttpResponse(status = StatusCodes.MethodNotAllowed, entity = entity.asString(HttpCharsets.`UTF-8`) + " Not correctly formatted")
					}
					
//					val requiredJson:JSONObject = json.obj.get("query") match {
//					  case Some(x) => x match {
//					                          case x:Map[String,any] => JSONObject(x.get("query"))
//					                          case _ => return HttpResponse(status = StatusCodes.MethodNotAllowed, entity = entity.asString(HttpCharsets.`UTF-8`) + " Not correctly formatted")
//					                          }
//					  case _ => return HttpResponse(status = StatusCodes.MethodNotAllowed, entity = entity.asString(HttpCharsets.`UTF-8`) + " Not correctly formatted: No json")
//					}

		    //We have the original JsonQuery
		    //calculate the JSonQuery for the policy;
		    val filter:JsValue = Policy2Filter.toFilter()
		    	  println("5")

				val query = Map("query"->Map("bool"->Map("must"->json,"filter"->filter))).toJson
				//val query = Map("query"->Map("filtered"->Map("query"->json,"filter"->filter))).

				//Now we delegate this to the real elasticsearch server
				val server: Uri = Uri(elasticServer + relUrl.toString)
				println("Server URL: " + server)
				val response = (IO(Http) ? HttpRequest(requestType,server,headers.filter(filterHeader),entity=query.prettyPrint)).mapTo[HttpResponse]
				val result:HttpResponse = Await.result(response,10 seconds)
				val resultJson = result.entity.asString.parseJson
				println(result.entity.asString)
				return HttpResponse(entity = resultJson.prettyPrint)

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
	
	


}

