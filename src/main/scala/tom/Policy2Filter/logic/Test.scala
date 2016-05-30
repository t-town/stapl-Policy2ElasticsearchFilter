package tom.Policy2Filter.logic


import stapl.core._
import stapl.parser._
import stapl.core.pdp._
import scala.util.{Success,Failure}
import org.parboiled2._
import Policy2Filter._
import spray.http._
import spray.json._
import scala.collection.mutable.HashMap
import stapl.core.dsl.log
import Thesis.Thesisbuild.Experiment._

/**
 * @author
 ${user.name}
 */
object App {

  def main(args : Array[String]) {
   /* 
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
*/

		val policyString = 
		  """
		  resource.creator = SimpleAttribute(String)
		  resource._id = SimpleAttribute(String)
		  resource.organization = SimpleAttribute(String)
		  subject.organization_id = SimpleAttribute(String)
		  resource.destinationorg = SimpleAttribute(String)
		  subject.assigned_organizations = ListAttribute(String)
		  
				Policy("assigned organisations") := when (action.id === "view") apply FirstApplicable to (
	        Rule("Edocs Rule") := permit iff ((resource.organization in subject.assigned_organizations) | (subject.id gt resource.creator)),
				  Rule("deny") := deny
				
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
		val req = new RequestCtx("1","view","");
		val find = new AttributeFinder()
		find.addModule(new SimpleAttributeFinderModule(io.Source.fromFile("attributes").mkString))
		val rem = new RemoteEvaluator

		val ctx = new BasicEvaluationCtx("evId",req,find,rem)

		val reduce1 = PolicyReduce.toResource(policy, ctx)
		println("reduced policy:")
		//Policy2Filter.extractRule(policy)
		val reducedPolicy = reduce1 match {
		  case Left(x) => x
		  case Right(x) => throw new UnsupportedOperationException("Not allowed:" + x)
		}
		println((reducedPolicy))
		val treeConverter = new TreeConverter(reducedPolicy)
		val one = treeConverter.reduce(reducedPolicy, PermitOverrides);
		println((one));
		//val resource_creator = SimpleAttribute(RESOURCE,"creator",String)

		val rule = one.subpolicies(0) match {
		  case x: stapl.core.Rule => x
		  case _ => throw new RuntimeException
		}
		
	
		println("Rule:")
		println((rule))
		println("Rule after translation to resource only:")
		val y = RuleReduce.toResource(rule, ctx) match {case Left(x) =>x}
		println((y))
		val x = Rule2Filter.toFilter(y, ctx)
		println("Rule after translation to query")
		println(x)
		
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

		End2End.executeSearchAndcheckCorrectness(policyString, io.Source.fromFile("attributes").mkString, searchJson, true)
    
    
		
	}
}
