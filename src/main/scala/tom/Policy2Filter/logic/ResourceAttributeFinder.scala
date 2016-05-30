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

import stapl.core.pdp._
import stapl.core._
import scala.collection.mutable.HashMap
import stapl.core
import spray.json._
import DefaultJsonProtocol._ 

/**
 * Class: ResourceAttributeFinder
 * Function: Given a JSON containing the resource from Elasticsearch: STAPL attributefinder
 */
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