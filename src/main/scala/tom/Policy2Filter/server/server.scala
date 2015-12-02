package tom.Policy2Filter.server

import spray.can.Http
import akka.io.IO
import akka.actor.ActorSystem



object server {
	def main(args : Array[String]) {
		    implicit val system = ActorSystem("elasticSearch-reverseProxy")

				val api = system.actorOf(ApiActor.props, "api-actor")

				IO(Http) ! Http.Bind(listener = api, interface = "localhost", port = 9000)

	}

}