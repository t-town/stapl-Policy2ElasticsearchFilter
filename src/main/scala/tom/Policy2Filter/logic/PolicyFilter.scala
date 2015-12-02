package tom.Policy2Filter.logic

import stapl.core._
import scala.util.parsing.json._
import stapl.core.pdp._
import stapl.parser._
import stapl.core.Result
import spray.json._
import spray.json.DefaultJsonProtocol._


object Policy2Filter {
  
  val policyString = """Rule("Ownership rule") := permit iff (resource.creator === subject.id)"""
  
  def toFilter():JsValue =  {
    toFilter(policyString)
  }

  def toFilter(policyString: String):JsValue =  {
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
    
    val policy: Rule = CompleteParser.parse(policyString,properties) match {
      case x:Rule => x
      case _ => throw new IllegalStateException("Policy type not allowed")
    }
    
		Rule2Filter.toFilter(policy, ctx)
    
  }
}

object Rule2Filter {
  
  def toFilter(rule: Rule, ctx: EvaluationCtx): JsValue = {
    
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


object Expression2Filter {

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