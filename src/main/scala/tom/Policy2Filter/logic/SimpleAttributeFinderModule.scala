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

/**
 * Class: SimpleAttributeFinderModule
 * Function: Interprets the given attributestring: AttributeFinderModule
 */
class SimpleAttributeFinderModule(attributeString: String) extends AttributeFinderModule {

	  val attributes = ResourceReader.readFromString(attributeString)

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
		} else if (aType == Number) {
		  Some(new NumberImpl(Left(str.toLong)))
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

object ResourceReader {
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


