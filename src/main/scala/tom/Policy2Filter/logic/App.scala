package tom.Policy2Filter.logic


import spray.json._
import DefaultJsonProtocol._ 
import stapl.parser._
import stapl.core._
import scala.util.parsing.json._
import stapl.core.pdp._
import stapl.core.Result
import spray.json._
import spray.json.DefaultJsonProtocol._

/**
 * @authorimport tom.Policy2Filter.logic.PolicyFilter
 ${user.name}
 */
object App {

	def main(args : Array[String]) {
		val policyString = """Rule("Ownership rule") := permit iff (resource.creator === subject.id)"""

				val subject_id = SimpleAttribute(SUBJECT,"id",String)
				val resource_creator = SimpleAttribute(RESOURCE,"creator",String)
				val view = SimpleAttribute(ACTION,"view",String)
				val resource_bool = SimpleAttribute(RESOURCE,"boolThing",Bool)
				val subject_org = ListAttribute(RESOURCE,"assigned_organizations",String)

				val properties = Map("resource.creator"->resource_creator, "subject.id"->subject_id, "action.id"->view,"resource.boolThing"->resource_bool,"subject.assigned_organizations"->subject_org)

				val req = new RequestCtx("1","view","1",(subject_id,"1"),(view,"view"),(subject_org,List("1","2")))
		val find = new AttributeFinder
		val rem = new RemoteEvaluator
		val ctx = new BasicEvaluationCtx("test",req,find,rem)

		CompleteParser.parse(policyString, properties);
	}
}
