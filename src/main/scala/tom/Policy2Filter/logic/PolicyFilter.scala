package tom.Policy2Filter.logic

import stapl.core._
import scala.util.parsing.json._
import stapl.core.pdp._
import stapl.parser._
import stapl.core.Result
import spray.json._
import spray.json.DefaultJsonProtocol._
import scala.util.{Success,Failure}
import org.parboiled2.ParseError

object Policy2Filter {
  
  
  //We can init the policyString. The policy itself cannot be parsed beforehand.
  val policyString = io.Source.fromFile("policyString").mkString
  val (s, a, r, e) = BasicPolicy.containers
  val parser = new CompleteParser(policyString, s,a,r,e)
  val unparsedPolicy = parser.CompletePolicy.run() match {
    case Success(result: Policy) => result
    case Success(e) => sys.error(e.toString ++ "not a policy")
    case Failure(e: ParseError) => sys.error(parser.formatError(e))
    case Failure(e) => throw new RuntimeException(e)
  }
  val reducedPolicy = TreeConverter.reduce(unparsedPolicy, PermitOverrides);
  val rule = extractRule(reducedPolicy)
  
  val req = new RequestCtx("1","view","")
  val find = new AttributeFinder
  //Adding the module that has our attributes to the finder.
  find.addModule(new SimpleAttributeFinderModule)
  val rem = new RemoteEvaluator
  val ctx = new BasicEvaluationCtx("evId",req,find,rem)
  val noResourceRule: Either[Rule,Boolean] = RuleReduce.toResource(rule,ctx)
  
  val filter: Either[JsValue, Boolean] = noResourceRule match {
    case Left(rule) => Left(Rule2Filter.toFilter(rule, ctx))
    case Right(bool) => Right(bool)
  }
  
  
  def extractRule(policy: Policy):Rule = {
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
   * 	False: no access allowed by default
   * 	True: All access allowed by default
   */
  def toFilter():Either[JsValue,Boolean] =  {
    return filter
  }

}

private object Rule2Filter {
  
  def toFilter(rule: stapl.core.Rule, ctx: EvaluationCtx): JsValue = {
    
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