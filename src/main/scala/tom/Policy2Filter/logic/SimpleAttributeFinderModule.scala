package tom.Policy2Filter.logic

import stapl.core.pdp._
import stapl.core._
import scala.collection.mutable.HashMap
import stapl.core

//We need two finders -> one for the reform of the policy to non resource
// the other one when we are deciding.
class SimpleAttributeFinderModule(attributeString: String) extends AttributeFinderModule {

	  val attributes = Utility.readFromString(attributeString)

			protected def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType, multiValued: Boolean): Option[ConcreteValue] = {
		//ctx: contains the evaluationCtx
		//ctype: eg RESOURCE
		//name: eg creator
		//atype: type
		val key = cType.toString() ++ "." ++  name 
		val str = attributes.contains(key) match {
  		case true => attributes.get(key).get
  		case false =>  return None
		}
		
		if(multiValued) {
		  val lst = str.split(";").toList
		  Some(new StringSeqImpl(lst))
		} else if(aType == Bool) {
		  Some(new BoolImpl(stringToBool(str)))
		} else {
		  Some(new StringImpl(str))
		}
	}
	
	protected def stringToBool(s: String):Boolean = {
		if (s.equals("1") || s.equalsIgnoreCase("true")) {
		  true
		} else if (s.equals("0") || s.equalsIgnoreCase("false") || s.equalsIgnoreCase("")) {
			false
		} else {
		  throw new IllegalArgumentException(s+" is not a bool.");
		}

	}


}

object Utility {
  def readFromString(attributeString: String):HashMap[String,String] = {
			val res = HashMap.empty[String,String]
					try {
						for( line <- attributeString.lines) {
						  val lineTrim = line.trim()
							res += (lineTrim.split(" ")(0) -> lineTrim.split(" ")(1));
						}
					} catch {
					case ex: Exception => null
					}
			res
	}

}


