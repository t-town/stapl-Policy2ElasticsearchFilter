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
import toString._
object Policy2Filter {
  

    
  
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


  def parsePolicyString(policyString:String):Policy = {
      val (s, a, r, e) = BasicPolicy.containers
      val parser = new CompleteParser(policyString, s,a,r,e)
      val parsedPolicy = parser.CompletePolicy.run() match {
        case Success(result: Policy) => result
        case Success(e) => sys.error(e.toString ++ "not a policy")
        case Failure(e: ParseError) => sys.error(parser.formatError(e))
        case Failure(e) => throw new RuntimeException(e)
      }
          println("original policy:")
          println(AbstractPolicyToString(parsedPolicy))
      return parsedPolicy
  }

  def getSimpleCtx(attributeString: String): EvaluationCtx = {
       //We get the subject, action, resource from the attributeString
        val req = new RequestCtx(Utility.readFromString(attributeString).get("SUBJECT.id").get,"view","")
        val find = new AttributeFinder
        //Adding the module that has our attributes to the finder.
        find.addModule(new SimpleAttributeFinderModule(attributeString))
        val rem = new RemoteEvaluator
        return new BasicEvaluationCtx("evId",req,find,rem)

  }
  
    /**
   * This function will return either:
   * 	JSValue: the new Filter
   * 	Deny: no access allowed by default
   * 	Permit: All access allowed by default
   * NotApplicable: The rule was not Applicable
   */
  def toFilter(policyString: String, attributeString: String): Either[JsValue,Decision] = {
    
    val ctx: EvaluationCtx = getSimpleCtx(attributeString)
        //Step 1: Parse Policy
    val parsedPolicy = parsePolicyString(policyString)
    //Step 2: Reduce the policy
    val eitherReducedPolicy = PolicyReduce.toResource(parsedPolicy, ctx)
    if(eitherReducedPolicy.isRight) {
      //A decision has been reached.
      val filter:Either[JsValue,Decision] = eitherReducedPolicy match {case Right(x) => Right(x)}
      println("The reducedPolicy reached a decision based on nonresouce attributes" )
      return filter
    } else {
      //Step 3: Normalise policy
      val reducedPolicy = eitherReducedPolicy match {case Left(x) => x}
            println("reduced policy:")
            println(AbstractPolicyToString(reducedPolicy))
       val normalisedPolicy = TreeConverter.reduce(reducedPolicy, PermitOverrides)
            println("normalised policy:")
            println(AbstractPolicyToString(normalisedPolicy))
        val rule = extractRule(normalisedPolicy)
            println("normalised Rule:")
            println(AbstractPolicyToString(rule))
            
        //Step 4: Reduce rule
        val filter =  RuleReduce.toResource(rule, ctx) match {
          case Left(rule) => {
                println("Rule without nonresource attributes:")
                println(AbstractPolicyToString(rule))
    
            val tempFilter = Rule2Filter.toFilter(rule,ctx)
              println("Filter:")
              println(tempFilter)
            Left(tempFilter)
          }
          case Right(decision) => {
            println("a decision has been reached: " + decision)
            Right(decision)
          }
        }
            return filter
    }
  }
  

  
}


private object Rule2Filter {
  
  def toFilter(rule: stapl.core.Rule, ctx: EvaluationCtx): JsValue = {
    println("The rule is: " ++ rule.condition.toString())
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