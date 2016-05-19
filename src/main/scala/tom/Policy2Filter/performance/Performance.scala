package tom.Policy2Filter.performance

import tom.Policy2Filter.logic.Policy2Filter
import tom.Policy2Filter.logic.End2End._
import spray.json._
import spray.json.DefaultJsonProtocol._
import stapl.core._
import spray.http._
import spray.http.HttpMethods._
import spray.can.Http
import akka.io.IO
import akka.actor.ActorSystem
import akka.actor._
import scala.concurrent._
import scala.concurrent.duration._
import akka.util.Timeout
import spray.http.HttpEntity.apply
import akka.pattern._

object Performance {
  implicit val system: ActorSystem = ActorSystem()
  implicit val timeout: Timeout = Timeout(15000)
  import system.dispatcher
  
  val globalServer = "http://localhost:9200"

  def getFilter(policyString: String, attributeString: String): Either[JsValue,Decision] = {
    Policy2Filter.toFilter(policyString, attributeString)
  }
  
  def executeSearch(filter: Either[JsValue,Decision], attributeString: String, searchJson: JsValue, qResult: QueryResult, tResult: TimeResult, server: String) {
	  var startTime = System.nanoTime()
			  //Step 1: Search with a policyFilter -> evaluate results
			  val query:JsValue = filter match {
  			  case Left(filter) =>
  			  {
  				  //We have to combine the filter with the original searchQuery
  				  val queryPart = searchJson.asJsObject.fields.get("query").get.asJsObject
  						  val nr:JsValue = JsNumber(10000)
  						  Map("query"->Map("bool"->Map("must"->queryPart,"filter"->filter)).toJson,"size"->nr).toJson
  			  }
  			  case Right(Permit) =>
  			  {
  				  //Access always allowed: original query
  				  val nr:JsValue = JsNumber(10000)
  						  Map("query"->searchJson,"size"->nr).toJson
  			  }
  			  //We already know there will be no results
  			  case _ => throw new ZeroResultsException
	  }

	  //Execute query on Elasticsearch database
	  val serverSearch = Uri(server + "/_search?scroll=1m")
	  val response = (IO(Http) ? HttpRequest(POST,serverSearch,entity=query.prettyPrint)).mapTo[HttpResponse]
	  val result:HttpResponse = Await.result(response,scala.concurrent.duration.Duration(30,scala.concurrent.duration.SECONDS))
	  val resultJson = result.entity.asString.parseJson
	  val initialhits = resultJson.asJsObject.fields.get("hits").get.asJsObject.fields.get("hits").get
	  val scrollId = resultJson.asJsObject.fields.get("_scroll_id").get.toString
	  continueScroll(scrollId,qResult, attributeString, initialhits,server)
		tResult.durationServer = (System.nanoTime() - startTime)/1000

  }
  
  def executeMinimalSearch(filter: Either[JsValue,Decision], searchJson: JsValue, tResult:TimeResult, server:String) {
        val startTime = System.nanoTime()
			  val query:JsValue = filter match {
  			  case Left(filter) =>
  			  {
  				  //We have to combine the filter with the original searchQuery
  				  val queryPart = searchJson.asJsObject.fields.get("query").get.asJsObject
  						  val nr:JsValue = JsNumber(10000)
  						  Map("query"->Map("bool"->Map("must"->queryPart,"filter"->filter)).toJson,"size"->nr).toJson
  			  }
  			  case Right(Permit) =>
  			  {
  				  //Access always allowed: original query
  				  val nr:JsValue = JsNumber(10000)
  						  Map("query"->searchJson,"size"->nr).toJson
  			  }
  			  //We already know there will be no results
  			  case _ => throw new ZeroResultsException
	  }
	  val serverSearch = Uri(server + "/_search")
	  val response = (IO(Http) ? HttpRequest(POST,serverSearch,entity=query.prettyPrint)).mapTo[HttpResponse]
	  val result:HttpResponse = Await.result(response,scala.concurrent.duration.Duration(30,scala.concurrent.duration.SECONDS))
	  tResult.durationServer = (System.nanoTime() - startTime)/1000
  }
  
  
  def executeOriginalQuery(searchJson: JsValue, policyString: String, attributeString: String, qResult: QueryResult, tResult: TimeResult, server: String) {
      val time = System.nanoTime
		  val serverSearch: Uri = Uri(server +  "/_search?scroll=1m")
		  val queryPart = searchJson.asJsObject.fields.get("query").get.asJsObject
		  val nr:JsValue = JsNumber(10000)
		  val newQuery = Map("query"->queryPart,"size"->nr).toJson
		  val response = (IO(Http) ? HttpRequest(POST,serverSearch,entity=newQuery.prettyPrint)).mapTo[HttpResponse]
  	  val result:HttpResponse = Await.result(response,scala.concurrent.duration.Duration(30,scala.concurrent.duration.SECONDS))
  	  val resultJson = result.entity.asString.parseJson
  	  val initialhits = resultJson.asJsObject.fields.get("hits").get.asJsObject.fields.get("hits").get
  	  val scrollId = resultJson.asJsObject.fields.get("_scroll_id").get.toString
  	  tResult.durationServer += (System.nanoTime - time)/1000
  	  continueScrollAndCheck(scrollId, policyString, attributeString, initialhits, server, qResult, tResult)
  }
  
  def executeMinimalSearchOriginalQuery(searchJson: JsValue, policyString: String, attributes: String, tResult: TimeResult, server:String) {
    val time = System.nanoTime
    val serverSearch:Uri = Uri(server + "/_search")
		val response = (IO(Http) ? HttpRequest(POST,serverSearch,entity=searchJson.prettyPrint)).mapTo[HttpResponse]
  	val result:HttpResponse = Await.result(response,scala.concurrent.duration.Duration(30,scala.concurrent.duration.SECONDS))
  	val resultJson = result.entity.asString.parseJson
  	val initialhits = resultJson.asJsObject.fields.get("hits").get.asJsObject.fields.get("hits").get
  	evaluateResults(initialhits,policyString,attributes,new QueryResult)
  }

  
  private def evaluateResults(results: JsValue, policyString: String, attributeString: String, qResult: QueryResult){
	    val policy = tom.Policy2Filter.logic.Utility.parsePolicyString(policyString)
		  evaluateResults(results,policy,attributeString,qResult)
  }
  
  /**
   * Evaluates each of the results, a result object is returned
   */
  def evaluateResults(results: JsValue, policy: Policy, attributeString: String, qResult: QueryResult){
	  val array = results match {
	  case x: JsArray => x
	  case _ => throw new IllegalArgumentException("Not a JsArray: " + results)
	  }
	  for(result <- array.elements) {
		  val resultAttributes = result.asJsObject.fields.get("_source").get
				  val ctx = tom.Policy2Filter.logic.Utility.getResultCtx(resultAttributes,attributeString)
				  val decision = policy.evaluate(ctx)
				  qResult.addDecision(decision.decision,resultAttributes.asJsObject.fields.get("id_").get.toString())
	  }
  }

  
  private def continueScrollAndCheck(scrollId: String, policyString: String, attributeString: String, initalHits: JsValue, server: String, qResult: QueryResult, tResult: TimeResult) {
	      var varScroll = scrollId
			  var hits: JsValue = initalHits
			  var time = System.nanoTime()
			  while(hits.toString != "[]") {
			    time = System.nanoTime()
          evaluateResults(hits, policyString, attributeString, qResult)
          tResult.durationRest += (System.nanoTime() - time)/1000
          time = System.nanoTime
    		  val searchServer: Uri = Uri( globalServer + "/_search/scroll")
    		  val query = Map("scroll"->"1m".toJson,"scroll_id"-> varScroll.filterNot { x => x == '\"'}.toJson).toJson
    		  val response = (IO(Http) ? HttpRequest(POST,searchServer,entity=query.prettyPrint)).mapTo[HttpResponse]
				  val result:HttpResponse = Await.result(response,scala.concurrent.duration.Duration(30,scala.concurrent.duration.SECONDS))
				  val resultJson = result.entity.asString.parseJson
				  varScroll = resultJson.asJsObject.fields.get("_scroll_id").get.toString
				  hits = resultJson.asJsObject.fields.get("hits").get.asJsObject.fields.get("hits").get
				  tResult.durationServer += (System.nanoTime() - time)/1000
		  }

  }
  
  private def continueScroll(scrollId: String, qResult: QueryResult, attributeString: String, initalHits: JsValue, server: String) {

	      var varScroll = scrollId
			  var hits: JsValue = initalHits
			  while(hits.toString != "[]") {
				  addPermitsToResult(hits,qResult)
				  val searchServer: Uri = Uri(globalServer + "/_search/scroll")
				  val query = Map("scroll"->"1m".toJson,"scroll_id"-> varScroll.filterNot { x => x == '\"'}.toJson).toJson
				  val response = (IO(Http) ? HttpRequest(POST,searchServer,entity=query.prettyPrint)).mapTo[HttpResponse]
						  val result:HttpResponse = Await.result(response,scala.concurrent.duration.Duration(30,scala.concurrent.duration.SECONDS))
						  val resultJson = result.entity.asString.parseJson
						  varScroll = resultJson.asJsObject.fields.get("_scroll_id").get.toString
						  hits = resultJson.asJsObject.fields.get("hits").get.asJsObject.fields.get("hits").get
			  }

  }

  
  def resetCache(server: String) {
    val serverCache: Uri = Uri(globalServer + "/_cache/clear")
    val response2 = (IO(Http) ? HttpRequest(POST,serverCache,entity="")).mapTo[HttpResponse]
    val result2:HttpResponse = Await.result(response2,scala.concurrent.duration.Duration(30,scala.concurrent.duration.SECONDS))
  }
  
  
  
  
}