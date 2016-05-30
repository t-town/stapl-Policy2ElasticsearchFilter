package tom.Policy2Filter.server
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

import spray.can.Http
import akka.io.IO
import akka.actor.ActorSystem


/**
 * Object: Server
 * Function: implementation of the reverse proxy
 */
object server {
	def main(args : Array[String]) {
		    implicit val system = ActorSystem("elasticSearch-reverseProxy")

				val api = system.actorOf(ApiActor.props, "api-actor")

				IO(Http) ! Http.Bind(listener = api, interface = "localhost", port = 9000)

	}

}