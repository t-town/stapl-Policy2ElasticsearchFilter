package tom.Policy2Filter.logic

qimport Policy2Filter._
import stapl.core._
import stapl.core.pdp._
import spray.json._
import spray.json.DefaultJsonProtocol._
import spray.http._
import spray.http.HttpMethods._
import spray.can.Http
import akka.io.IO
import akka.actor.ActorSystem
import akka.actor._
import scala.concurrent._
import scala.concurrent.duration._
import akka.util.Timeout
import scala.collection.mutable.Set
import spray.http.HttpEntity.apply
import akka.pattern._
import tom.Policy2Filter.performance.Performance

/**
 * Object: End2end
 * Function: Provide functions for End2End (without server) functionality
 */

object End2End {
  implicit val system: ActorSystem = ActorSystem()
  implicit val timeout: Timeout = Timeout(1500)
  import system.dispatcher

  /**
   * This method will execute the whole process.
   * - Translate the policy to ElasticSearch Filter
   * - Execute filter
   * - Check all results
   * - execute original search query
   * - check all results
   * - compare results
   * @return : True | All results are compatible
   * 						False | Not all results were compatible, the filtered query was not correct
   */
  def executeSearchAndcheckCorrectness(policyString: String, attributeString: String, searchJson: JsValue, checkCorrectness: Boolean): Boolean = {
        Performance.resetCache("http://localhost:9200/thesis/resource")
        var startTime = System.nanoTime()
    	  val filterDecision: Either[JsValue,Decision] = Policy2Filter.toFilter(policyString, attributeString)
    	  println("Filter took:" + (System.nanoTime() - startTime )/1000000)
        val searchEvaluationResult = executeFilteredQuery(filterDecision, policyString, attributeString, searchJson,false)
        searchEvaluationResult.setElapsedTime(System.nanoTime() - startTime)
        Performance.resetCache("http://localhost:9200/thesis/resource")
			  //Step 2: Evaluate with the original search query
        startTime = System.nanoTime()
			  val originalEvaluationResult = executeOriginalQuery(searchJson, policyString, attributeString,checkCorrectness)
			  originalEvaluationResult.setElapsedTime(System.nanoTime() - startTime)
			  println(searchEvaluationResult)
			  println(originalEvaluationResult)
			  return searchEvaluationResult.wasRightTranslationOf(originalEvaluationResult)
	}

  def executeOriginalQuery(searchJson: spray.json.JsValue, policyString: String, attributeString: String, checkCorrectness: Boolean): QueryResult = {
    //Step 2: Evaluate with the original search query
    var startTime = System.nanoTime()
    val server: Uri = Uri( "http://localhost:9200/thesis/resource/_search?scroll=1m")
    val queryPart = searchJson.asJsObject.fields.get("query").get.asJsObject
    val nr:JsValue = JsNumber(10000)
    val newQuery = Map("query"->queryPart,"size"->nr).toJson
    val response2 = (IO(Http) ? HttpRequest(POST,server,entity=newQuery.prettyPrint)).mapTo[HttpResponse]
    val result2:HttpResponse = Await.result(response2,scala.concurrent.duration.Duration(10,scala.concurrent.duration.SECONDS))
    val resultJson2 = result2.entity.asString.parseJson
    val originalEvaluationResult = new QueryResult
    val initialhits2 = resultJson2.asJsObject.fields.get("hits").get.asJsObject.fields.get("hits").get
    val scrollId2 = resultJson2.asJsObject.fields.get("_scroll_id").get.toString
     println("Original Query: Result fetching took:" + (System.nanoTime() - startTime )/1000000)
    continueScroll(scrollId2,originalEvaluationResult, policyString, attributeString, initialhits2,checkCorrectness)
    originalEvaluationResult
  }

  def executeFilteredQuery(filterDecision: Either[JsValue,Decision], policyString: String, attributeString: String, searchJson: spray.json.JsValue, checkCorrectness:Boolean): QueryResult = {
    var startTime = System.nanoTime()
    try {
    		  //Step 1: Search with a policyFilter -> evaluate results
    					val query:JsValue = filterDecision match {
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
          		val server: Uri = Uri( "http://localhost:9200/thesis/resource/_search?scroll=1m")
          		val response = (IO(Http) ? HttpRequest(POST,server,entity=query.prettyPrint)).mapTo[HttpResponse]
          		val result:HttpResponse = Await.result(response,scala.concurrent.duration.Duration(10,scala.concurrent.duration.SECONDS))
          		val resultJson = result.entity.asString.parseJson
          		
          		println(resultJson)
          		val searchEvaluationResult = new QueryResult
          		val initialhits = resultJson.asJsObject.fields.get("hits").get.asJsObject.fields.get("hits").get
          		val scrollId = resultJson.asJsObject.fields.get("_scroll_id").get.toString
          		println("Filtered result fetching took: " +(System.nanoTime() - startTime)/1000000) 
          		continueScroll(scrollId,searchEvaluationResult, policyString, attributeString, initialhits,checkCorrectness)
          		println(searchEvaluationResult)
          		searchEvaluationResult
          		
    		} catch {
    		      case e: ZeroResultsException => new QueryResult()
    		}
  }
  
  def continueScroll(scrollId: String, currentResult: QueryResult, policyString: String, attributeString: String, initalHits: JsValue, checkCorrectness: Boolean) {

    var varScroll = scrollId
    var hits: JsValue = initalHits
    while(hits.toString != "[]") {
       if(checkCorrectness) {
         evaluateResults(hits,policyString,attributeString, currentResult)
       } else {
         addPermitsToResult(hits,currentResult)
       }
	     val server: Uri = Uri( "http://localhost:9200/_search/scroll")
	     val query = Map("scroll"->"1m".toJson,"scroll_id"-> varScroll.filterNot { x => x == '\"'}.toJson).toJson
       val response = (IO(Http) ? HttpRequest(POST,server,entity=query.prettyPrint)).mapTo[HttpResponse]
       val result:HttpResponse = Await.result(response,scala.concurrent.duration.Duration(10,scala.concurrent.duration.SECONDS))
       val resultJson = result.entity.asString.parseJson
       println(resultJson)
       varScroll = resultJson.asJsObject.fields.get("_scroll_id").get.toString
       hits = resultJson.asJsObject.fields.get("hits").get.asJsObject.fields.get("hits").get
    }

  }
  
  def addPermitsToResult(results: JsValue,currentResult: QueryResult) {
    val array = results match {
  		case x: JsArray => x
  		case _ => throw new IllegalArgumentException("Not a JsArray: " + results)
  	}
  	for(result <- array.elements) {
  		val resultAttributes = result.asJsObject.fields.get("_source").get
  		currentResult.addDecision(Permit,resultAttributes.asJsObject.fields.get("id_").get.toString())
  	}
  }



  def evaluateResults(results: JsValue, policyString: String, attributeString: String, currentResult: QueryResult){
  	  val policy = Utility.parsePolicyString(policyString)
  		evaluateResults(results,policy,attributeString,currentResult)
  }
  
  
  /**
   * Evaluates each of the results, a result object is returned
   */
  def evaluateResults(results: JsValue, policy: Policy, attributeString: String, currentResult: QueryResult){
  	val array = results match {
  		case x: JsArray => x
  		case _ => throw new IllegalArgumentException("Not a JsArray: " + results)
  	}
  	for(result <- array.elements) {
  		val resultAttributes = result.asJsObject.fields.get("_source").get
  		val ctx = Utility.getResultCtx(resultAttributes,attributeString)
  		val decision = policy.evaluate(ctx)
  		currentResult.addDecision(decision.decision,resultAttributes.asJsObject.fields.get("id_").get.toString())
  	}
  }


  def main(args : Array[String]) {
	  val policyString = io.Source.fromFile("EdocsPolicy").mkString

			  val attributeString = """
			  ACTION.id view
			  SUBJECT.id 1
			  SUBJECT.organization_id 2
			  SUBJECT.assigned_organizations 0;1;2
			  SUBJECT.department Accounting 
			  SUBJECT.role employee
			  SUBJECT.supervisees 5;7;8;2
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

			  println(executeSearchAndcheckCorrectness(policyString,attributeString,searchJson,true))
			  //val filterDecision: Either[JsValue,Decision] = Policy2Filter.toFilter(policyString, attributeString)


  }



  class ZeroResultsException extends RuntimeException {

  }

  class QueryResult() {
    val permits = Set.empty[String]
    val denies = Set.empty[String]
    val notApplicables = Set.empty[String]
    var elapsedTime: Long = 0
    def addDecision(decision: Decision, id: String) = {
      decision match {
        case Permit => permits.add(id)
        case Deny => denies.add(id)
        case NotApplicable => notApplicables.add(id)
      }
    }
    
    def wasRightTranslationOf(other: QueryResult): Boolean = {
      if(this.denies.size != 0 || this.notApplicables.size != 0) {
        return false
      }
      return permits.equals(other.permits)
    }
    
    override def toString: String = {
      return "Permit: " + permits.size + " Deny: " + denies.size + " NotApplicable: " + notApplicables.size + " Elapsed Time: " + elapsedTime/1000000 + " ms"
    }
    
       
    def setElapsedTime(time: Long) {
      elapsedTime = time    }
  }
}