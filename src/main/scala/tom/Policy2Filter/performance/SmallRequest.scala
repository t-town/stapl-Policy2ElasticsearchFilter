package tom.Policy2Filter.performance

import spray.json._
import scala.collection.mutable.ListBuffer
import tom.Policy2Filter.logic.End2End._
class SmallRequest(nrEvaluations: Int, nrServers: Int, nrUsers: Int,nrOrganizations: Int, seed:Long, nrInLists:Int, nrWarmups: Int, searchJson: JsValue,server: String) {
      val serverlstBuffer = ListBuffer[String]()
      
      //make the list of servers
      for(i <- 1 to nrServers) {
        serverlstBuffer += server + (10000*i).toString + "/"
      }
      val servers = serverlstBuffer.toList
      //make the list of performance results
      val lst = ListBuffer[PerformanceResult]()
      
      for(i <- 1 to nrServers) {
        lst += new PerformanceResult
      }
      //timers: a list with timers for each server that we use to test.
      val filterTimers = lst.toList
      lst.clear()
      for(i <- 1 to nrServers) {
        lst += new PerformanceResult
      }
      val originalTimers = lst.toList
      
      //Execute: nrEvaluations * (1 -> nrServers) * nrUsers (=4)
      def execute {
    	  for(i <- 0 to nrWarmups) {
    		  print("+")
    		  for(j <- 1 to nrServers) {
			      for(userString <- EdocsPolicy.getUsers(nrUsers, nrOrganizations, seed, nrInLists)) {
    		      print("-")
    		      def try_(): Unit = {
    		        try {
      		        Performance.resetCache(server)
    	    			  val tResult1 = new TimeResult
          				val startTime = System.nanoTime()
          				val filter = Performance.getFilter(EdocsPolicy.getPolicyString, userString)
          				tResult1.durationRest = (System.nanoTime - startTime)/1000
          				Performance.executeMinimalSearch(filter, searchJson, tResult1, server)
          				Performance.resetCache(server)
          				val tResult2 = new TimeResult
          				Performance.executeMinimalSearchOriginalQuery(searchJson, EdocsPolicy.getPolicyString, userString, tResult2, server)
    		        } catch {
    		          case e: Exception => println("try" + e.toString()); try_
    		        }
    		      }
    		      try_
    		    }
    		      
    		  }
    	  }
    	  
    	  for (i <- 0 to nrEvaluations) {
    	    print("-")
    	    for(j <- 1 to nrServers) {
    	    	for(userString <- EdocsPolicy.getUsers(nrUsers, nrOrganizations, seed, nrInLists)) {
    	    		print("+")
    	    		def try_(): Unit = {
    	    				try {
    	    					Performance.resetCache(server)
    	    					val tResult1 = new TimeResult
    	    					val startTime = System.nanoTime()
    	    					val filter = Performance.getFilter(EdocsPolicy.getPolicyString, userString)
    	    					tResult1.durationRest = (System.nanoTime - startTime)/1000
    	    					Performance.executeMinimalSearch(filter, searchJson, tResult1, server)
    	    					Performance.resetCache(server)
    	    					val tResult2 = new TimeResult
    	    					Performance.executeMinimalSearchOriginalQuery(searchJson, EdocsPolicy.getPolicyString, userString, tResult2, server)
    	    					filterTimers(j-1) += tResult1
    	    					originalTimers(j-1) += tResult2
    	    				} catch {
    	    				case e: Exception => println("try" + e.toString()); try_
    	    				}
    	    		}
    	    		try_
    	    	}
    		  }
    	  }
      }
      
}