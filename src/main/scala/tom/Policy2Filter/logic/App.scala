package tom.Policy2Filter.logic


import stapl.core._
import stapl.parser._
import stapl.core.pdp._
import scala.util.{Success,Failure}
import org.parboiled2._
import Policy2Filter._

/**
 * @authorimport tom.Policy2Filter.logic.PolicyFilter
 ${user.name}
 */
object App {
  Success
	def main(args : Array[String]) {
    
       println("test")
		val subject_id = SimpleAttribute(SUBJECT,"id",String)
		val resource_companyId = SimpleAttribute(RESOURCE,"company_id",String)
		val subject_companyId = SimpleAttribute(SUBJECT,"company_id",String)
		val resource_recipient = SimpleAttribute(RESOURCE,"recipient",String)
		val subject_supervised = ListAttribute(RESOURCE,"supervised",String)
		
		val properties = Map("resource.company_id"->resource_companyId,"resource.recipient"->resource_recipient,"subject.supervisedClients"->subject_supervised,"subject.company_id"->subject_companyId)
		
		val req = new RequestCtx("1","view","1",(subject_id,"1"),(subject_companyId,"1"),(subject_supervised,List("1","2")))
		val find = new AttributeFinder
		val rem = new RemoteEvaluator
		//val comb = new CombinationAlgorithmImplementationBundle()
		val ctx = new BasicEvaluationCtx("test",req,find,rem)
	
		val policyString = """Rule("Edocs Rule") := permit iff
		  ((subject.company_id === resource.company_id) & (resource.recipient in subject.supervisedClients))"""
		
    val ru:stapl.core.Rule = CompleteParser.parse(policyString,properties) match {
		  case x: stapl.core.Rule => x
		  case _ => throw new IllegalArgumentException("Illegal policyString: " + policyString)
		}
		println(ru);
		println(Rule2Filter.toFilter(ru,ctx))


	/*	val policyString = """resource.creator = SimpleAttribute(String)

	Policy("Simple policy with ownership rule") := when (action.id === "view") apply PermitOverrides to (
			Rule("Ownership rule") := permit iff (resource.creator === subject.id),
			Rule("Default deny") := deny
	)"""

    val (s, a, r, e) = BasicPolicy.containers
    
	
		val parser = new CompleteParser(policyString, s, a, r, e)
		val temppolicy = parser.CompletePolicy.run() match {
		  case Success(result) => result
      case Failure(e: ParseError) => sys.error(parser.formatError(e))
      case Failure(e) => throw new RuntimeException(e)

		}
		val policy = temppolicy match {
		  case x: Policy => x;
		  case _ => throw new RuntimeException
		}
		
		val conv = new TreeConverter(policy);
		val one = conv.reduce(policy, PermitOverrides);
		println(one);
		val resource_creator = SimpleAttribute(RESOURCE,"creator",String)

		val req = new RequestCtx("1","view","1",(resource_creator,"1"));
		val find = new AttributeFinder
		val rem = new RemoteEvaluator

		val ctx = new BasicEvaluationCtx("evId",req,find,rem);

		
		//val two = conv.normalise(one);
		*/
	}
}
