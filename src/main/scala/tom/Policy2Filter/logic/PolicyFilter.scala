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
import scala.util.parsing.json._
import stapl.core.pdp._
import stapl.parser._
import stapl.core.Result
import spray.json._
import spray.json.DefaultJsonProtocol._
import scala.util.{Success,Failure}
import org.parboiled2.ParseError
import Thesis.Thesisbuild.Experiment._

/**
 * Object: Policy2Filter
 * Function: Translate policy declared in STAPL to Elasticsearch filter
 * Options: debug = true: print intermediate steps
 */
object Policy2Filter {
  
  var debug = false
  
  /**
   * Extracts the permit rule from this policy.
   * Assumes the policy has exactly two rules, one permit and one deny.
   */
  private def extractPermitRule(policy: Policy): Rule = {
    val one: Rule = policy.subpolicies(0) match {
      case x: Rule => x
      case _ => throw new RuntimeException(policy.toString)
    }
    val two: Rule = policy.subpolicies(1) match {
      case x: Rule => x
      case _ => throw new RuntimeException(policy.toString)
    }
    if(one.effect == Permit) {
      one
    } else {
      two
    }
  }

  
    /**
   * This function will return either:
   * 	JSValue: the new Filter
   * 	Deny: no access allowed by default
   * 	Permit: All access allowed by default
   * NotApplicable: The rule was not Applicable
   */
  def toFilter(policyString: String, attributeString: String): Either[JsValue,Decision] = {
    
    val ctx: EvaluationCtx = Utility.getSimpleCtx(attributeString)
        //Step 1: Parse Policy
    val parsedPolicy = Utility.parsePolicyString(policyString)
    //Step 2: Reduce the policy
    val eitherReducedPolicy = PolicyReduce.toResource(parsedPolicy, ctx)
    if(eitherReducedPolicy.isRight) {
      //A decision has been reached.
      val filter:Either[JsValue,Decision] = eitherReducedPolicy match {case Right(x) => Right(x)}
      if(debug) {println("The reducedPolicy reached a decision based on nonresouce attributes" )}
      return filter
    } else {
      //Step 3: Normalise policy
      val reducedPolicy = eitherReducedPolicy match {case Left(x) => x}
            if(debug) {
            	println("reduced policy:")
            	println((reducedPolicy))
            }
      val treeConverter = new TreeConverter(null)      
      val normalisedPolicy = treeConverter.reduce(reducedPolicy, PermitOverrides)
            if(debug) {
              println("normalised policy:")
              println((normalisedPolicy))
            }
      val rule = extractPermitRule(normalisedPolicy)
          if(debug) {
            println("normalised Rule:")
            println((rule))
          }
        //Step 4: Reduce rule
        val filter =  RuleReduce.toResource(rule, ctx) match {
          case Left(rule) => {
        	  if(debug) {
        		  println("Rule without nonresource attributes:")
        		  println((rule))
        	  }

        	  val tempFilter = Rule2Filter.toFilter(rule,ctx)
    			  if(debug) {
    				  println("Filter:")
    				  println(tempFilter)
    			  }
        	  Left(tempFilter)
          }
          case Right(decision) => {
            if(debug) { println("a decision has been reached: " + decision) }
            Right(decision)
          }
        }
            return filter
    }
  }
  

  
}

/**
 * Object: Rule2Filter
 * Function: Translate Rule declared in STAPL to Elasticsearch filter
 */
private object Rule2Filter {
  
  def toFilter(rule: stapl.core.Rule, ctx: EvaluationCtx): JsValue = {
    if(Policy2Filter.debug) { println("The rule is: " ++ rule.condition.toString())}
    rule.effect match {
      case Permit => Expression2Filter.toFilter(rule.condition,ctx)
      case Deny => handleDeny(rule.condition,ctx) //optimize a deny - not scenario
    }
  }
  
  def handleDeny(exp: Expression, ctx: EvaluationCtx): JsValue = {
    exp match {
       case x: Not => Expression2Filter.toFilter(x.expression,ctx)
       case _ => Map("bool"->Map("must_not"->Expression2Filter.toFilter(exp,ctx))).toJson
    }
  }
}

/**
 * Object: Expression2Filter
 * Function: Translate Expression declared in STAPL to Elasticsearch filter
 */
private object Expression2Filter {

	def toFilter(exp: Expression, ctx: EvaluationCtx): JsValue = {
		exp match {
		case s: EqualsValue => EqualsValue2Filter(s, ctx)
		case s: GreaterThanValue => GreaterThanValue2Filter(s,ctx)
		case s: BoolExpression => BoolExpression2Filter(s,ctx)
		case s: ValueIn => ValueIn2Filter(s,ctx)
		case s: Not => Not2Filter(s,ctx)
		case s: And => And2Filter(s,ctx)
		case s: Or => Or2Filter(s,ctx)
		case _ => throw new IllegalStateException("Unsupported type " + exp.getClass);
		}
	}
	
	def extractResourceValue(val1: Value, val2: Value, ctx:EvaluationCtx): (String,ConcreteValue,Boolean) = {
	  val one: Option[ConcreteValue] = try {
	    Some(val1.getConcreteValue(ctx));
	  } catch {
	    case e: AttributeNotFoundException => None
	    case e: Exception => throw new IllegalStateException(val1.toString() + " not translatable to a concrete value");
	  }
	  
	  val two = try {
	    Some(val2.getConcreteValue(ctx));
	  } catch {
	    case e: AttributeNotFoundException => None
	    case e: Exception => throw new IllegalStateException(val1.toString() + " not translatable to a concrete value");

	  }
	  
	  if(one.isEmpty && two.isEmpty) {
	    throw new IllegalStateException("Not Supported operation: Resource occurs twice in EqualsValue");
	  } else if (! (one.isEmpty || two.isEmpty )) {
		  throw new IllegalStateException("Not Supported operation: Resource does not occur in EqualsValue");
	  }
	  
	  if (one.isEmpty) {
	    val r = val1 match {
	      case x: SimpleAttribute => x.name
	      case _ => throw new IllegalStateException("Unsupported: " + val1);
	    }
	    return(r,two.get,true)
	  } else {
	    val r = val2 match {
	      case x: SimpleAttribute => x.name
	      case _ => throw new IllegalStateException("Unsupported: " + val2);
	    }
	    return(r,one.get,false)
	  }
	}

	def EqualsValue2Filter(s: EqualsValue, ctx: EvaluationCtx): JsValue = {
	  val (r,v,_) = extractResourceValue(s.value1,s.value2,ctx);
	  //EqualsValue: term query:
	  /*“filter”: {
	 	*		“term”: {
	 	* 		“creator”:1
	 	*		}
	 	*	}
	 	*/
	  //Start building the JSON
	  Map("term"->Map(r.toString() -> v.toString())).toJson
	  
	}
	
	def GreaterThanValue2Filter(s: GreaterThanValue, ctx: EvaluationCtx): JsValue = {
	  
	  val (r,v,first) = extractResourceValue(s.value1, s.value2, ctx);
	  val compareString = if (first) {
	    //the resource is value1. >
	    "gt"
	  } else {
	    //the resource is value2. <
	    "lt"
	  }
	  val one = Map(compareString->v.toString()).toJson
	  val two = Map(r->one).toJson
	  val three = Map("range"->two).toJson
	  
	  return three;
	}
	
	def BoolExpression2Filter(s: BoolExpression, ctx: EvaluationCtx): JsValue = {
	  if (s.attribute.cType != RESOURCE) {
	    throw new IllegalStateException("BoolExpression not with Resource");
	  }
	  Map("term"->Map(s.attribute.name->"1")).toJson
	}
	
	def ValueIn2Filter(s: ValueIn, ctx: EvaluationCtx): JsValue = {
	  if((s.value).asInstanceOf[SimpleAttribute].cType != RESOURCE) {
	    throw new IllegalStateException("ValueIn not with Resource");
	  }
	  val list = try {
	    s.list.getConcreteValue(ctx) match {
	      case x: SeqValue => Some(x)
	    }
	  } catch {
	    case e: Exception => None
	  }
	  if(list.isEmpty) {
	    throw new IllegalStateException("ValueIn with invalid list");
	  }
	  	  
	  
	  val jsonList:List[String] = list.get.representation.asInstanceOf[List[String]]
	  
	  val one = Map((s.value).asInstanceOf[SimpleAttribute].name -> jsonList).toJson
	  val two = Map("terms"->one).toJson
	  return two;
	}
	
	def Not2Filter(s: Not, ctx: EvaluationCtx): JsValue = {
	  val inner: JsValue = toFilter(s.expression, ctx)
	  val outer = Map("bool"->Map("must_not"->inner)).toJson
	  return outer;
	  
	}
	
	def And2Filter(s: And, ctx: EvaluationCtx): JsValue = {
	  val one = toFilter(s.expression1,ctx);
	  val two = toFilter(s.expression2,ctx);
	  val outer = Map("bool"->Map("must"->List(one,two))).toJson
	  return outer
	}
	
	def Or2Filter(s: Or, ctx: EvaluationCtx): JsValue = {
	  val one = toFilter(s.expression1,ctx);
	  val two = toFilter(s.expression2,ctx);
	  val outer = Map("bool"->Map("should"->List(one,two))).toJson
	  return outer

	}

}