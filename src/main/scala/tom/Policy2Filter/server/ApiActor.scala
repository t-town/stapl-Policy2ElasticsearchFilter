package tom.Policy2Filter.server
/**
 *    Copyright 2016 KU Leuven Research and Developement - iMinds - Distrinet
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 *    Administrative Contact: dnet-project-office@cs.kuleuven.be
 *    Technical Contact: maarten.decat@cs.kuleuven.be
 *    Author: tom.stappaerts@student.kuleuven.be
 */

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
import stapl.core.{Permit,Deny,NotApplicable,Decision}


object ApiActor {
	def props = Props[ApiActor]
}

/**
 * Class: ApiActor
 * Function: reverse proxy implementation
 */
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
    
    		//We have the original JsonQuery
    		//calculate the JSonQuery for the policy;

					val policyString = io.Source.fromFile("policyString").mkString
					val policyDecision: Either[JsValue,Decision] = Policy2Filter.toFilter(io.Source.fromFile("policyString").mkString,io.Source.fromFile("attributes").mkString)
					val query = policyDecision match {
					  case Left(filter) =>
					    {
					        Map("query"->Map("bool"->Map("must"->json,"filter"->filter))).toJson
					    }
					  case Right(Permit) =>
					    {
					      //Access always allowed: original query
					      Map("query"->json).toJson
					    }
					  case Right(Deny) =>
					    {
					      //Access never allowed, no need to consult elasticsearch
					      val returnString = """
                                  {
                                    "took": 0,
                                    "timed_out": false,
                                    "_shards": {
                                      "total": 0,
                                      "successful": 0,
                                      "failed": 0
                                    },
                                    "hits": {
                                      "total": 0,
                                      "max_score": null,
                                      "hits": []
                                    }
                                  }"""
					      return HttpResponse(entity = returnString)
					    }
					  case Right(NotApplicable) =>
					    {
					      println("POLICY NOT APPLICABLE")
					      val returnString = """
                                  {
                                    "took": 0,
                                    "timed_out": false,
                                    "_shards": {
                                      "total": 0,
                                      "successful": 0,
                                      "failed": 0
                                    },
                                    "hits": {
                                      "total": 0,
                                      "max_score": null,
                                      "hits": []
                                    }
                                  }"""
					      return HttpResponse(entity = returnString)
					    }
					}
					    //Now we delegate this to the real elasticsearch server
              val server: Uri = Uri(elasticServer + relUrl.toString)
              println("Server URL: " + server)
              println("The new query is:")
              println(query.prettyPrint)
              val response = (IO(Http) ? HttpRequest(requestType,server,headers.filter(filterHeader),entity=query.prettyPrint)).mapTo[HttpResponse]
            	val result:HttpResponse = Await.result(response,scala.concurrent.duration.Duration(10,scala.concurrent.duration.SECONDS))
            	val resultJson = result.entity.asString.parseJson
            	println(result.entity.asString)
					    //Return to the requestor
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

