package tom.Policy2Filter.logic

import Policy2Filter._
import stapl.core._
import stapl.core.pdp._

import spray.json._
import DefaultJsonProtocol._

import spray.http._
import HttpMethods._
import spray.can.Http
import akka.io.IO
import akka.actor.ActorSystem
import akka.actor._

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Success, Failure}
import akka.util.Timeout
import akka.pattern.ask

import scala.collection.mutable.Set

object End2End {
	implicit val system: ActorSystem = ActorSystem()
			implicit val timeout: Timeout = Timeout(1500)
			import system.dispatcher
			/**
			 * This method will execute the whole process.
			 * - Translate the policy to ElasticSearch Filter
			 * - Execute filter
			 * - Check all results
			 * - execute original query
			 * - check all results
			 */
			def evaluate(policyString: String, attributeString: String,searchJson: JsValue) = {
    		try {
    		  //Step 1: Search with a policyFilter -> evaluate results
    			    val filterDecision: Either[JsValue,Decision] = Policy2Filter.toFilter(policyString, attributeString)
    					val query = filterDecision match {
      					case Left(filter) =>
        					{
        						//We have to combine the filter with the original searchQuery
        						val queryPart = searchJson.asJsObject.fields.get("query").get.asJsObject
        								Map("query"->Map("bool"->Map("must"->queryPart,"filter"->filter))).toJson
        					}
      					case Right(Permit) =>
        					{
        						//Access always allowed: original query
        						Map("query"->searchJson).toJson
        					}
      					//We already know there will be no results
      					case _ => throw new ZeroResultsException
    		      }
    
          		//Execute query on Elasticsearch database
          		val server: Uri = Uri( "http://localhost:9200/thesis/resource/_search?scroll=1m")
          		val response = (IO(Http) ? HttpRequest(POST,server,entity=query.prettyPrint)).mapTo[HttpResponse]
          		val result:HttpResponse = Await.result(response,scala.concurrent.duration.Duration(10,scala.concurrent.duration.SECONDS))
          		val resultJson = result.entity.asString.parseJson
          		println(resultJson)
          		val searchEvaluationResult = new Result
          		val initialhits = resultJson.asJsObject.fields.get("hits").get.asJsObject.fields.get("hits").get
          		val scrollId = resultJson.asJsObject.fields.get("_scroll_id").get.toString
          		continueScroll(scrollId,searchEvaluationResult, policyString, attributeString, initialhits)
          		println(searchEvaluationResult)
          		
          		//Step 2: Evaluate with the original search query
          		val response2 = (IO(Http) ? HttpRequest(POST,server,entity=searchJson.prettyPrint)).mapTo[HttpResponse]
          		val result2:HttpResponse = Await.result(response2,scala.concurrent.duration.Duration(10,scala.concurrent.duration.SECONDS))
          		val resultJson2 = result2.entity.asString.parseJson
          		val originalEvaluationResult = new Result
          		val initialhits2 = resultJson2.asJsObject.fields.get("hits").get.asJsObject.fields.get("hits").get
          		val scrollId2 = resultJson2.asJsObject.fields.get("_scroll_id").get.toString
          		continueScroll(scrollId2,originalEvaluationResult, policyString, attributeString, initialhits2)
          		
          		println(searchEvaluationResult)
          		println(originalEvaluationResult)
          		println(searchEvaluationResult.wasRightTranslationOf(originalEvaluationResult))
          		
    		} catch {
    		      case e: ZeroResultsException => 
    		}

	}
	
	def continueScroll(scrollId: String, currentResult: Result, policyString: String, attributeString: String, initalHits: JsValue) {

	  var varScroll = scrollId
	  var hits: JsValue = initalHits
	      while(hits.toString != "[]") {
         evaluateResults(hits,policyString,attributeString, currentResult)
  	     val server: Uri = Uri( "http://localhost:9200/_search/scroll")
  	     val query = Map("scroll"->"1m","scroll_id"-> varScroll.filterNot { x => x == '\"'}).toJson
         val response = (IO(Http) ? HttpRequest(POST,server,entity=query.prettyPrint)).mapTo[HttpResponse]
         val result:HttpResponse = Await.result(response,scala.concurrent.duration.Duration(10,scala.concurrent.duration.SECONDS))
         val resultJson = result.entity.asString.parseJson
         println(resultJson)
         varScroll = resultJson.asJsObject.fields.get("_scroll_id").get.toString
         hits = resultJson.asJsObject.fields.get("hits").get.asJsObject.fields.get("hits").get
	      }

	}

	def getResultCtx(resultAttributes: JsValue, attributeString: String):EvaluationCtx = {
		//We get the subject, action, resource from the attributeString
		val req = new RequestCtx(Utility.readFromString(attributeString).get("SUBJECT.id").get,"view","")
		val find = new AttributeFinder
		//Adding the module that has our attributes to the finder.
		find.addModule(new ResourceAttributeFinder(resultAttributes,attributeString))
		val rem = new RemoteEvaluator
		return new BasicEvaluationCtx("evId",req,find,rem)

	}

	def evaluateResults(results: JsValue, policyString: String, attributeString: String, currentResult: Result){
		  val policy = Policy2Filter.parsePolicyString(policyString)
			evaluateResults(results,policy,attributeString,currentResult)
	}
	/**
	 * Evaluates each of the results, a result object is returned
	 */
	def evaluateResults(results: JsValue, policy: Policy, attributeString: String, currentResult: Result){
		val array = results match {
  		case x: JsArray => x
  		case _ => throw new IllegalArgumentException("Not a JsArray: " + results)
		}
		for(result <- array.elements) {
			val resultAttributes = result.asJsObject.fields.get("_source").get
			val ctx = getResultCtx(resultAttributes,attributeString)
			val decision = policy.evaluate(ctx)
			currentResult.addDecision(decision.decision,resultAttributes.asJsObject.fields.get("id_").get.toString())
		}
	}


	def main(args : Array[String]) {
		val policyString = """
				subject.organization_id = SimpleAttribute(String)			
				subject.department = SimpleAttribute(String)				
				subject.assigned_organizations = ListAttribute(String) 

				resource.creator = SimpleAttribute(String)
				resource.origin = SimpleAttribute(String)						
				resource.type = SimpleAttribute(String)
				resource.destination_organization = SimpleAttribute(String)

				action.id = SimpleAttribute(String)

				/*//Policy("Simple policy with ownership rule, origin and destination restriction") := when (action.id === "view") apply FirstApplicable to (
				//	Rule("Ownership rule") := permit iff (resource.creator === subject.id),
				//	Rule("Origin") := deny iff ((subject.organization_id === resource.destination_organization) & !(resource.origin in subject.assigned_organizations)),
				//	Rule("Organization restriction") := permit iff ((resource.origin === subject.organization_id) & ("Accounting" === subject.department)),
				//	Rule("Default deny") := deny
				)*/

				Policy("Simple Policy") := when  (action.id === "view") apply FirstApplicable to (
				Rule("Ownership rule") := permit iff (resource.creator === subject.id),
				Rule("Origin") := deny iff (!(subject.organization_id === resource.destination_organization)),
				Rule("Organization restriction") := permit iff ("Accounting" === subject.department),
				//Rule("permit") := permit
				Rule("Default deny") := deny
				)
				
				/*Policy("simple policy") := when (action.id === "view") apply FirstApplicable to (
				  Rule("one") := permit iff (resource.creator === subject.id),
				  Rule("default deny") := deny
				) */ 
				"""
				val attributeString = """
				ACTION.id view
				SUBJECT.id 1
				SUBJECT.organization_id 2
				SUBJECT.assigned_organizations 0;1;2
				SUBJECT.department Accounting 
				""".trim()
				val searchJson = """
				{
				"query": {
				"filtered": {
				"query": {
				"match_all": {}
				}
				}
				}
				}
				""".parseJson

				evaluate(policyString,attributeString,searchJson)

	}



	class ZeroResultsException extends RuntimeException {

	}

	class Result() {
	  val permits = Set.empty[String]
	  val denies = Set.empty[String]
	  val notApplicables = Set.empty[String]

	  def addDecision(decision: Decision, id: String) = {
	    decision match {
	      case Permit => permits.add(id)
	      case Deny => denies.add(id)
	      case NotApplicable => notApplicables.add(id)
	    }
	  }
	  
	  def wasRightTranslationOf(other: Result): Boolean = {
	    if(this.denies.size != 0 || this.notApplicables.size != 0) {
	      return false
	    }
	    return permits.equals(other.permits)
	  }
	  
	  override def toString: String = {
	    return "Permit: " + permits.size + " Deny: " + denies.size + " NotApplicable: " + notApplicables.size
	  }
	}


}