package tom.Policy2Filter.logic
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
import stapl.core.pdp._
import stapl.parser._
import scala.util.{Success,Failure}
import org.parboiled2.ParseError
import spray.json._

/**
 * Object: Utility
 * Function: Utilityfunctions: parse policy, get Ctx.
 * Options: debug = true: print intermediate steps
 */

object Utility {

  var debug = false
	/**
	 * Parses the policy string and returns the Policy
	 */
	def parsePolicyString(policyString:String):Policy = {
			val (s, a, r, e) = BasicPolicy.containers
					val parser = new CompleteParser(policyString, s,a,r,e)
			val parsedPolicy = parser.CompletePolicy.run() match {
			case Success(result: Policy) => result
			case Success(e) => sys.error(e.toString ++ "not a policy")
			case Failure(e: ParseError) => sys.error(parser.formatError(e))
			case Failure(e) => throw new RuntimeException(e)
			}
			if(debug) {
				println("original policy:")
				println(parsedPolicy)
			}
			return parsedPolicy
	}

	/**
	 * get BasicEvaluationCtx using this AttributeString
	 */
	def getSimpleCtx(attributeString: String): EvaluationCtx = {
			//We get the subject, action, resource from the attributeString
			val req = new RequestCtx(ResourceReader.readFromString(attributeString).get("SUBJECT.id").get,ResourceReader.readFromString(attributeString).get("ACTION.id").get,"")
			val find = new AttributeFinder
			//Adding the module that has our attributes to the finder.
			find.addModule(new SimpleAttributeFinderModule(attributeString))
			val rem = new RemoteEvaluator
			return new BasicEvaluationCtx("evId",req,find,rem)
	}

	/**
	 * get a BasicEvaluationCtx, using the given AttributeString and the attributes of the given Resource
	 */
	def getResultCtx(resultAttributes: JsValue, attributeString: String):EvaluationCtx = {
			//We get the subject, action, resource from the attributeString
			val req = new RequestCtx(ResourceReader.readFromString(attributeString).get("SUBJECT.id").get,"view","")
			val find = new AttributeFinder
			//Adding the module that has our attributes to the finder.
			find.addModule(new ResourceAttributeFinder(resultAttributes,attributeString))
			val rem = new RemoteEvaluator
			return new BasicEvaluationCtx("evId",req,find,rem)
	}



}