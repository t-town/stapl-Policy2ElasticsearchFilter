package tom.Policy2Filter.server

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Success, Failure}

import akka.actor.ActorSystem
import akka.util.Timeout
import akka.pattern.ask
import akka.io.IO

import spray.can.Http
import spray.http._
import HttpMethods._

import akka.io.IO
import akka.actor.ActorSystem

import akka.actor._
import spray.routing._
import spray.json._
import spray.json.DefaultJsonProtocol._

import spray.http._
import spray.can.Http
import tom.Policy2Filter.logic._
import HttpMethods._
import spray.http.Uri._


object ApiActor {
	def props = Props[ApiActor]
}

class ApiActor extends HttpServiceActor with ActorLogging {
	implicit val system: ActorSystem = ActorSystem()
			implicit val timeout: Timeout = Timeout(15)

			import system.dispatcher // implicit execution context


			val elasticServer = "localhost:9300"

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
					val json:JsValue = try {
						entity.asString(HttpCharsets.`UTF-8`).parseJson
					} catch {
					case e: Exception => return HttpResponse(status = StatusCodes.MethodNotAllowed, entity = entity.asString(HttpCharsets.`UTF-8`) + " Not correctly formatted")
					}
		    //We have the original JsonQuery
		    //calculate the JSonQuery for the policy;
		    val filter:JsValue = Policy2Filter.toFilter()

				val query = Map("query"->Map("filtered"->Map("query"->json,"filter"->filter))).toJson
				//Now we delegate this to the real elasticsearch server
				val server: Uri = Uri(path = Path(elasticServer) + relUrl.toString)
				log.debug(query.compactPrint)
				
//				val response = (IO(Http) ? HttpRequest(requestType,server,headers,entity=query.compactPrint)).mapTo[HttpResponse]
//				response onComplete {
//		      case Success(x) => log.warning(x.entity.toString())
//		      case Failure(x) => log.warning("Fail")
//		       
//		    }
				HttpResponse(entity = query.prettyPrint)

				}
	}




}