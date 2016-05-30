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

import spray.json._
import spray.json.DefaultJsonProtocol._
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

case class OneConfig(nrEvaluationsPerUser: Int = 100, nrWarmups: Int = 1, nrUsers: Int = 10, nrOrganizations: Int = 5, initialSeed: Long = 72, nrInLists: Int = 4, server: String = "http://localhost:9200/thesis/resource", filterEnabled: Boolean = true, otherEnabled: Boolean = true)
 
/**
 * Object: OnePolicyMain
 * Function: Main to input variables for OnePolicy test
 */
object OnePolicyMain {
    def main(args: Array[String]) {
      
    val parser = new scopt.OptionParser[OneConfig]("scopt") {
        opt[Int]("nrEvaluationsPerUser") action { (x, c) =>
          c.copy(nrEvaluationsPerUser = x)
        } text ("The number of evaluations Per User that will be performed.")
        opt[Int]("nrWarmups") action { (x, c) =>
          c.copy(nrWarmups = x)
        } text ("The number of times a warmup will be performed.")
        opt[Int]("nrUsers")  action { (x, c) =>
          c.copy(nrUsers = x)
        } text ("The number of Users in the database.")
        opt[Int]("nrOrganizations")  action { (x, c) =>
          c.copy(nrOrganizations = x)
        } text ("The number of Organizations in the database.")
        opt[Int]("initialSeed")  action { (x, c) =>
          c.copy(initialSeed = x)
        } text ("The initialSeed for the random generator.")
        opt[Int]("nrInLists")  action { (x, c) =>
          c.copy(nrInLists = x)
        } text ("The nr of elements in each list that is part of the attributes.")
        opt[String]("server") action { (x, c) =>
          c.copy(server = x)
        } text ("The server to connect to.")
        opt[Boolean]("filterEnabled")  action { (x, c) =>
          c.copy(filterEnabled = x)
        } text ("Is the filter enabled for this test.")
        opt[Boolean]("otherEnabled")  action { (x, c) =>
          c.copy(otherEnabled = x)
        } text ("Is the brute force method enabled.")

      }
    
      parser.parse(args, OneConfig()) map { config =>
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
  			  val start = System.nanoTime()
  			  val one = new OnePolicy(EdocsPolicy,config.nrEvaluationsPerUser,config.nrWarmups,searchJson,config.server,config.nrUsers,config.nrOrganizations,config.initialSeed,config.nrInLists,config.filterEnabled,config.otherEnabled)
  			  one.execute
  			  if(config.filterEnabled) {
  			      Files.write(Paths.get("FilterOutput.dat"), one.filterTimer.getJson.toString().getBytes(StandardCharsets.UTF_8))
  			      one.filterTimer.printHistogram
  			      println("------------------------------------------------------------")
  			  }
  			  if(config.otherEnabled) {
  			      Files.write(Paths.get("OriginalOutput.dat"), one.originalTimer.getJson.toString().getBytes(StandardCharsets.UTF_8))
  			      one.originalTimer.printHistogram
  			  }
  			  println("done")
  			  println("took: " + (System.nanoTime() - start)/1000000000 + "s")
  			  println("exiting")
  
        
      }
    }
    

}