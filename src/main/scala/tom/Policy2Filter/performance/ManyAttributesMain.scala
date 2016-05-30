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

case class Config(nrEvaluations: Int = 10, nrWarmups: Int = 1, nrAttributes: Int = 10,server: String = "http://localhost:9200/thesis/ManyAttributes", thresholdAllowed: Int = 0)
  /**
 * Object: ManyAttributesMain
 * Function: Main to input variables for ManyAttributes test
 */
object ManyAttributesMain {
    def main(args: Array[String]) {
      
    val parser = new scopt.OptionParser[Config]("scopt") {
      opt[Int]("nrEvaluations") action { (x, c) =>
        c.copy(nrEvaluations = x)
      } text ("The number of evaluations per length of the policy that will be performed.")
      opt[Int]("nrWarmups") action { (x, c) =>
        c.copy(nrWarmups = x)
      } text ("The number of times a warmup will be performed.")
      opt[Int]("nrAttributes")  action { (x, c) =>
        c.copy(nrAttributes = x)
      } text ("The number of attributes in the policy.")
      opt[String]("server") action { (x, c) =>
        c.copy(server = x)
      } text ("The server to connect to.")
      opt[Int]("thresholdAllowed")  action { (x, c) =>
          c.copy(thresholdAllowed = x)
      } text ("The threshold from where the attribute is allowed. 0-4. 0:All are allowed, 4: All are allowed")

    }
    parser.parse(args, Config()) map { config =>
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
			  val start = System.nanoTime
			  val one = new ManyAttributes(config.nrEvaluations,config.nrAttributes,config.nrWarmups,searchJson,config.server,config.thresholdAllowed)
			  println("starting")
			  one.execute
			  //writing the result
			  val jsonOne = one.filterTimers map {
			    x => x.getJson
			  }
			  val jsonTwo = one.originalTimers map {
			    x => x.getJson
			  }
			  Files.write(Paths.get("FilterOutput.dat"), jsonOne.toJson.toString().getBytes(StandardCharsets.UTF_8))
			  Files.write(Paths.get("OriginalOutput.dat"), jsonTwo.toJson.toString().getBytes(StandardCharsets.UTF_8))
			  println("done")
			  println("took: " + (System.nanoTime() - start)/1000000000 + " s")
			  println("exiting")

      
    }
    }
    

}