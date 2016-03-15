package tom.Policy2Filter.performance

import stapl.core._
import spray.json._
import spray.http._
import tom.Policy2Filter.logic.End2End._
class OnePolicy(policy: PolicyForEval, nrEvaluationsPerUser: Int, nrWarmups: Int, searchJson: JsValue, server: String,nrUsers: Int, nrOrganizations: Int, initialSeed: Long, nrInLists: Int) {
  
    var seed = initialSeed
		val filterTimer: performanceResult = new performanceResult
		val originalTimer: performanceResult = new performanceResult
		
	def execute {
		//warmup
		for(n <- 0 to nrWarmups) {
		  print("+")
			for(userString <- policy.getUsers(nrUsers, nrOrganizations, seed, nrInLists)) {
			  Performance.resetCache(server)
			  val qResult1 = new QueryResult
				val tResult1 = new TimeResult
				val startTime = System.nanoTime()
				val filter = Performance.getFilter(policy.getPolicyString, userString)
				tResult1.durationRest = (System.nanoTime - startTime)/10000
				Performance.executeSearch(filter, userString, searchJson, qResult1, tResult1, server)
				Performance.resetCache(server)
				val qResult2 = new QueryResult
				val tResult2 = new TimeResult
				Performance.executeOriginalQuery(searchJson, policy.getPolicyString, userString, qResult2, tResult2, server)
			}
		}
		
		//Actual Execution
		for(n <- 0 to nrEvaluationsPerUser) {
		  print("-")
		  for(userString <- policy.getUsers(nrUsers, nrOrganizations, seed, nrInLists)) {
		    print("+")
			  Performance.resetCache(server)
			  val qResult = new QueryResult
				val tResult = new TimeResult
				val startTime = System.nanoTime()
				val filter = Performance.getFilter(policy.getPolicyString, userString)
				tResult.durationRest = (System.nanoTime - startTime)/1000000
				Performance.executeSearch(filter, userString, searchJson, qResult, tResult, server)
				filterTimer += tResult
				Performance.resetCache(server)
				val qResult2 = new QueryResult
				val tResult2 = new TimeResult
				Performance.executeOriginalQuery(searchJson, policy.getPolicyString, userString, qResult2, tResult2, server)
				originalTimer += tResult2
		  }
		}
	}
    
}