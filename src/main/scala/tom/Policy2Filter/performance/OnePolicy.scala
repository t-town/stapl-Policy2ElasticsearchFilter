package tom.Policy2Filter.performance

import stapl.core._
import spray.json._
import spray.http._
import tom.Policy2Filter.logic.End2End._
class OnePolicy(policy: PolicyForEval, nrEvaluationsPerUser: Int, nrWarmups: Int, searchJson: JsValue, server: String,nrUsers: Int, nrOrganizations: Int, initialSeed: Long, nrInLists: Int, filterEnabled: Boolean, otherEnabled: Boolean) {
  
    var seed = initialSeed
		val filterTimer: PerformanceResult = new PerformanceResult
		val originalTimer: PerformanceResult = new PerformanceResult
		
	def execute {
		//warmup
		for(n <- 0 to nrWarmups) {
		  print("+")
			for(userString <- policy.getUsers(nrUsers, nrOrganizations, seed, nrInLists)) {
  			def try_():Unit = {
  			  try{
    				Performance.resetCache(server)
    				val qResult1 = new QueryResult
    				val tResult1 = new TimeResult
    				val startTime = System.nanoTime()
    				if(filterEnabled) {
      				val filter = Performance.getFilter(policy.getPolicyString, userString)
      				tResult1.durationRest = (System.nanoTime - startTime)/1000
      				Performance.executeSearch(filter, userString, searchJson, qResult1, tResult1, server)
      				Performance.resetCache(server)
    				}
    				val qResult2 = new QueryResult
    				val tResult2 = new TimeResult
    				if(otherEnabled) {
    				  Performance.executeOriginalQuery(searchJson, policy.getPolicyString, userString, qResult2, tResult2, server)
    				}
    				//check validity:
    				if(otherEnabled && filterEnabled) {
    				  assert(qResult1.wasRightTranslationOf(qResult2))
    				}
  			  } catch {
  			    case e: Exception => println("try"); try_
  			  }
		    }
			}
		}
		
		//Actual Execution
		for(n <- 0 to nrEvaluationsPerUser) {
		  print("-")
		  for(userString <- policy.getUsers(nrUsers, nrOrganizations, seed, nrInLists)) {
		    print("+")
  			def try_():Unit = {
		      try {
    				Performance.resetCache(server)
    				val qResult1 = new QueryResult
    				val tResult1 = new TimeResult
    				val startTime = System.nanoTime()
    				if(filterEnabled) {
      				val filter = Performance.getFilter(policy.getPolicyString, userString)
      				tResult1.durationRest = (System.nanoTime - startTime)/1000
      				Performance.executeSearch(filter, userString, searchJson, qResult1, tResult1, server)
      				Performance.resetCache(server)
    				}
    				val qResult2 = new QueryResult
    				val tResult2 = new TimeResult
    				if(otherEnabled) {
    				  Performance.executeOriginalQuery(searchJson, policy.getPolicyString, userString, qResult2, tResult2, server)
    				}
    				//check validity:
    				if(otherEnabled && filterEnabled) {
    				  assert(qResult1.wasRightTranslationOf(qResult2))
    				}
    				if(filterEnabled) {
    				  filterTimer += tResult1
    				}
    				if(otherEnabled) {
    				  originalTimer += tResult2
    				}
		      } catch {
				    case e: Exception => println("try"); try_
		      }
		    }
		  
		  }
		}
	}
    
}