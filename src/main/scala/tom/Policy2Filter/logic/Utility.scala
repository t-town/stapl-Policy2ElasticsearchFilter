package tom.Policy2Filter.logic

import stapl.core._
import stapl.core.pdp._
import stapl.parser._
import scala.util.{Success,Failure}
import org.parboiled2.ParseError
import spray.json._
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