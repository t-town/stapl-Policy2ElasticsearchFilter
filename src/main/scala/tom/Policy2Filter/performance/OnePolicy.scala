package tom.Policy2Filter.performance
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

import stapl.core._
import spray.json._
import spray.http._
import tom.Policy2Filter.logic.End2End._

/**
 * Class: OnePolicy
 * Function: Evaluation on one database
 */
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
  			try_
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
		    try_
		  
		  }
		}
	}
    
}