package tom.Policy2Filter.performance

import spray.json._
import spray.json.DefaultJsonProtocol._
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

case class SmallConfig(nrEvaluations: Int = 10, nrServers: Int = 10,nrWarmups: Int = 1, nrUsers: Int = 10, nrOrganizations: Int = 5, nrInLists: Int = 4,server: String = "http://localhost:9200/thesis/", initialSeed: Long = 72)
  
object SmallRequestMain {
    def main(args: Array[String]) {
      
    val parser = new scopt.OptionParser[SmallConfig]("scopt") {
      opt[Int]("nrEvaluations") action { (x, c) =>
        c.copy(nrEvaluations = x)
      } text ("The number of evaluations per length of the policy that will be performed.")
      opt[Int]("nrWarmups") action { (x, c) =>
        c.copy(nrWarmups = x)
      } text ("The number of times a warmup will be performed.")
      opt[Int]("nrServers") action { (x, c) =>
        c.copy(nrServers = x)
      } text ("The number of times a warmup will be performed.")
        opt[Int]("nrUsers")  action { (x, c) =>
          c.copy(nrUsers = x)
        } text ("The number of Users in the database.")
        opt[Int]("nrInLists")  action { (x, c) =>
          c.copy(nrInLists = x)
        } text ("The nr of elements in each list that is part of the attributes.")
        opt[Int]("nrOrganizations")  action { (x, c) =>
          c.copy(nrOrganizations = x)
      } text ("The number of organizations in the policy.")
      opt[String]("server") action { (x, c) =>
        c.copy(server = x)
      } text ("The server to connect to.")
        opt[Int]("initialSeed")  action { (x, c) =>
          c.copy(initialSeed = x)
      } text ("The initialSeed for the random generator.")
    }
    parser.parse(args, SmallConfig()) map { config =>
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
			  val one = new SmallRequest(config.nrEvaluations,config.nrServers,config.nrUsers,config.nrOrganizations,config.initialSeed,config.nrInLists,config.nrWarmups,searchJson,config.server)
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