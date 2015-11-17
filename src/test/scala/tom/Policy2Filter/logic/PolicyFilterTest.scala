package tom.Policy2Filter.logic

import org.junit._
import Assert._
import stapl.core._
import stapl.parser._
import stapl.core.pdp._
import Expression2Filter._
import org.scalatest.junit.AssertionsForJUnit
import scala.util.parsing.json.JSONObject
import spray.json._
import spray.json.DefaultJsonProtocol._

@Test
class PolicyFilterTest extends AssertionsForJUnit {
  var ctx: BasicEvaluationCtx = _
  var properties:Map[String,Attribute] = _
  
  @Before def setup() {
    //First we create a sufficient amount of attributes to evaluate all possible types of policies
    
		val subject_id = SimpleAttribute(SUBJECT,"id",String)
		val resource_creator = SimpleAttribute(RESOURCE,"creator",String)
		val view = SimpleAttribute(ACTION,"view",String)
		val resource_bool = SimpleAttribute(RESOURCE,"boolThing",Bool)
		val subject_org = ListAttribute(RESOURCE,"assigned_organizations",String)
		
		properties = Map("resource.creator"->resource_creator, "subject.id"->subject_id, "action.id"->view,"resource.boolThing"->resource_bool,"subject.assigned_organizations"->subject_org)
		
		val req = new RequestCtx("1","view","1",(subject_id,"1"),(view,"view"),(subject_org,List("1","2")))
		val find = new AttributeFinder
		val rem = new RemoteEvaluator
		//val comb = new CombinationAlgorithmImplementationBundle()
		ctx = new BasicEvaluationCtx("test",req,find,rem)
  }
  
  def evaluate(policyString: String): Expression = {
    val ru:stapl.core.Rule = CompleteParser.parse(policyString,properties) match {
		  case x: stapl.core.Rule => x
		  case _ => throw new IllegalArgumentException("Illegal policyString: " + policyString)
		}

		val exp:Expression = ru.condition match {
		  case x:Expression => x
		  case _ => throw new IllegalArgumentException("Illegal policyString: " + policyString)
		}
		return exp

  }
  
  @Test def EqualsValue2FilterTest() {
    	val policyString1 = """Rule("Ownership rule") := permit iff (resource.creator === subject.id)"""
    	val result1 = toFilter(evaluate(policyString1),ctx);
    	val policyString2 = """Rule("Ownership rule") := permit iff (subject.id === resource.creator)"""
    	val result2 = toFilter(evaluate(policyString2),ctx);
    	val solution = Map("term"->Map("creator"->"1")).toJson;
    	println(result1);
    	assertEquals(result1,result2)
    	assertEquals(result1,solution)
  }
  
  @Test def GreaterThanValue2FilterTest() {
    	val policyString1 = """Rule("Ownership rule") := permit iff (resource.creator gt subject.id)"""
    	val result1 = toFilter(evaluate(policyString1),ctx);
    	val policyString2 = """Rule("Ownership rule") := permit iff (subject.id lt resource.creator)"""
    	val result2 = toFilter(evaluate(policyString2),ctx);
    	val solution = Map("range"->Map("creator"->Map("gt"->"1"))).toJson;
    	println(result1);
    	assertEquals(result1,result2)
    	assertEquals(result1,solution)
  }
  
  @Test def BoolExpression2FilterTest() {
    	val policyString = """Rule("Ownership rule") := permit iff (resource.boolThing)"""
    	val result = toFilter(evaluate(policyString),ctx)
    	val solution = Map("term"->Map("boolThing"->"1")).toJson
    	println(result)
    	assertEquals(result,solution)
  }
  
  @Test def ValueIn2FilterTest {
		val policyString = """Rule("Ownership rule") := permit iff (resource.creator in subject.assigned_organizations)"""
		val result = toFilter(evaluate(policyString),ctx)
		val solution = Map("terms"->Map("creator"->List("1","2"))).toJson
		println(result)
    assertEquals(result,solution)
  }
  
  @Test def Not2FilterTest {
    val policyString = """Rule("Ownership rule") := permit iff (!(resource.creator === subject.id))"""
    val result = toFilter(evaluate(policyString),ctx)
    val solution = Map("bool"->Map("must_not"->Map("term"->Map("creator"->"1")))).toJson
    println(result)
    assertEquals(result,solution)
  }
  
  @Test def And2FilterTest {
    val policyString = """Rule("Ownership rule") := permit iff ((resource.creator === subject.id) & (subject.id === resource.creator))"""
    val result = toFilter(evaluate(policyString),ctx)
    val solution = Map("bool"->Map("must"->
        List(Map("term"->Map("creator"->"1")),Map("term"->Map("creator"->"1"))))).toJson
        
    println(result)
    assertEquals(result, solution)
  }
  
    @Test def Or2FilterTest {
    val policyString = """Rule("Ownership rule") := permit iff ((resource.creator === subject.id) | (subject.id === resource.creator))"""
    val result = toFilter(evaluate(policyString),ctx)
    val solution = Map("bool"->Map("should"->
        List(Map("term"->Map("creator"->"1")),Map("term"->Map("creator"->"1"))))).toJson
    println(result)
    assertEquals(result, solution)
  }

  
  
}