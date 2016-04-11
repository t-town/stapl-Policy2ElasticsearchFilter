package tom.Policy2Filter.performance

import spray.json._
import scala.collection.mutable.ListBuffer
import tom.Policy2Filter.logic.End2End._
class ManyAttributes(nrEvaluations: Int, nrAttributes: Int, nrWarmups: Int, searchJson: JsValue, server: String) {
      val lst = ListBuffer[PerformanceResult]()
      
      for(i <- 1 to nrAttributes) {
        lst += new PerformanceResult
      }
      //timers: a list with timers for each nr of Attributes that we use to test.
      val filterTimers = lst.toList
      lst.clear()
      for(i <- 1 to nrAttributes) {
        lst += new PerformanceResult
      }
      val originalTimers = lst.toList
      val lstPolicy = ListBuffer[String]()
      for(i<- 1 to nrAttributes) {
        lstPolicy += getPolicyString(i)
      }
      val policies = lstPolicy.toList
      val attributeString = "SUBJECT.id 0 \n ACTION.id 0"
      
      //Execute: nrEvaluations * (1 -> nrAttributes)
      def execute {
    	  for(i <- 0 to nrWarmups) {
    		  print("+")
    		  for(j <- 1 to nrAttributes) {
    		      print("-")
    		      def try_(): Unit = {
    		        try {
      		        Performance.resetCache(server)
    	    			  val tResult1 = new TimeResult
          			  val qResult1 = new QueryResult
          				val startTime = System.nanoTime()
          				val filter = Performance.getFilter(policies(j-1), attributeString)
          				tResult1.durationRest = (System.nanoTime - startTime)/1000
          				Performance.executeSearch(filter, attributeString, searchJson, qResult1, tResult1, server)
          				Performance.resetCache(server)
          				val qResult2 = new QueryResult
          				val tResult2 = new TimeResult
          				Performance.executeOriginalQuery(searchJson, policies(j-1), attributeString, qResult2, tResult2, server)
          				//verify
          				assert(qResult1.wasRightTranslationOf(qResult2))
    		        } catch {
    		          case e: Exception => println("try"); try_
    		        }
    		      }
    		      
    		  }
    	  }
    	  
    	  for (i <- 0 to nrEvaluations) {
    	    print("-")
    	    for (j <- 1 to nrAttributes) {
    	      print("+")
    		      def try_(): Unit = {
    		        try {
    	    			  Performance.resetCache(server)
    	    			  val tResult1 = new TimeResult
          			  val qResult1 = new QueryResult
          				val startTime = System.nanoTime()
          				val filter = Performance.getFilter(policies(j-1), attributeString)
          				tResult1.durationRest = (System.nanoTime - startTime)/1000
          				Performance.executeSearch(filter, attributeString, searchJson, qResult1, tResult1, server)
          			  Performance.resetCache(server)
          				val qResult2 = new QueryResult
          				val tResult2 = new TimeResult
          				Performance.executeOriginalQuery(searchJson, policies(j-1), attributeString, qResult2, tResult2, server)
          				assert(qResult1.wasRightTranslationOf(qResult2))
          				filterTimers(j-1) += tResult1
          				originalTimers(j-1) += tResult2
    		        } catch {
    		          case e: Exception => println("try"); try_
    		        }
    	      }
    	    }
    	  }
      }
      
      
      
      
      def getPolicyString(nrAttributes: Int): String = {
        //resource.attr1 = SimpleAttribute(String) 
        //Policy("nrAttributes policy") := when (AlwaysTrue) apply PermitOverrides to (
        //Rule("attr1") := permit iff (resource.attr1 == 0),
    	  //...
        val returnString = new StringBuilder
    	  for(i <- 1 to nrAttributes) {
    	    returnString.append("resource.attr"+i+" = SimpleAttribute(String)\n")

    	  }
        returnString.append("""Policy("nrAttributes:"""+nrAttributes+"""") :=  when (AlwaysTrue) apply PermitOverrides to (""")
        for(i <- 1 to nrAttributes) {
          returnString.append("""Rule("attr"""+i+"""") := permit iff (resource.attr"""+i+""" === "0"),""")
        }
        returnString.append("""Rule("default deny") := deny)""")
        return returnString.toString()
      }
}