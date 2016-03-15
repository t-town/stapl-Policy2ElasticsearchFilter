package tom.Policy2Filter.logic


import stapl.core.pdp._
import stapl.core._
import scala.collection.mutable.HashMap
import stapl.core
import spray.json._
import DefaultJsonProtocol._ 

class ResourceAttributeFinder(resourceJson: JsValue, attributeString: String) extends SimpleAttributeFinderModule(attributeString) {
  
  val resourceAttributes: HashMap[String,String] = extractMap(resourceJson)
  
  override protected def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType, multiValued: Boolean): Option[ConcreteValue] = {
    if(cType != RESOURCE) {
      return super.find(ctx, cType, name, aType, multiValued)
    }
		val str = resourceAttributes.contains(name) match {
  		case true => resourceAttributes.get(name).get
  		case false =>  return super.find(ctx, cType, name, aType, multiValued)
		}
		
		if(multiValued) {
		  val lstval = str.filterNot { x => x == '[' || x == ']' }
		  val lst = lstval.split(",").toList
		  Some(new StringSeqImpl(lst))
		} else if(aType == Bool) {
		  Some(new BoolImpl(stringToBool(str)))
		} else if (aType == Number) {
		  Some(new NumberImpl(Left(str.toLong)))
		} else {
		  Some(new StringImpl(str))
		}

  }
  
  private def extractMap(resourceJson: JsValue): HashMap[String,String] = {
    val attributes = HashMap.empty[String,String]
    resourceJson.asJsObject.fields foreach {
      case (key,value) => {
        val valueString = value.toString.filterNot { x => x == '\"'}
        attributes.put(key, valueString)
      }
    }
    return attributes
  }
}