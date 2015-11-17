package tom.Policy2Filter.logic

import java.text.DateFormat._
import Policy1._
import stapl.parser._
import stapl.core._
import stapl.core.pdp._
import Expression2Filter._
/**
 * @authorimport tom.Policy2Filter.logic.PolicyFilter
 ${user.name}
 */
object App {


	def main(args : Array[String]) {
		println( "Hello World!" )
		//val policyString = """Rule("Ownership rule") := permit iff (resource.creator === subject.id)"""
		//val policyString = """Rule("Ownership rule") := permit iff (resource.boolThing)"""
		val policyString = """Rule("Ownership rule") := permit iff (resource.creator in subject.assigned_organizations)"""
		val subject_id = SimpleAttribute(SUBJECT,"id",String)
		val resource_creator = SimpleAttribute(RESOURCE,"creator",String)
		val view = SimpleAttribute(ACTION,"view",String)
		val resource_bool = SimpleAttribute(RESOURCE,"boolThing",Bool)
		val subject_list = ListAttribute(RESOURCE,"assigned_organizations",String)
		val x = Map("resource.creator"->resource_creator, "subject.id"->subject_id, "action.id"->view,"resource.boolThing"->resource_bool,"subject.assigned_organizations"->subject_list)
		
		val ctx = new RequestCtx("1","view","1",(subject_id,"1"),(view,"view"),(subject_list,List("1","2")))
		val find = new AttributeFinder
		val rem = new RemoteEvaluator
		//val comb = new CombinationAlgorithmImplementationBundle()
		val ev = new BasicEvaluationCtx("test",ctx,find,rem)
		
		val ru:Rule = CompleteParser.parse(policyString,x) match {
		  case x: Rule => x
		  case _ => null
		}

		val eq:Expression = ru.condition match {
		  case x:Expression => x
		  case _ => null
		}
		
		val json = toFilter(eq,ev);
		println(json);

	/*	val pol:Policy = CompleteParser.parse(policyString,x) match {
		  case x:Policy => x
		  case _ => null
		}
		
		//pol.evaluate(ev)
		println("---")
		pol.isApplicable(ev)
		val pol2:Rule = pol.subpolicies(0) match {
		  case x:Rule => x
		  case _ => null
		}
		println("-----")
		pol2.evaluate(ev)
	
		println(pol2.condition)
		*/
	}
}
