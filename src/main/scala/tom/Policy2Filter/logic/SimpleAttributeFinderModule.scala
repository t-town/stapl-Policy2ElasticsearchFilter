package tom.Policy2Filter.logic

import stapl.core.pdp._
import stapl.core._
import scala.collection.mutable.HashMap
import stapl.core

//We need two finders -> one for the reform of the policy to non resource
// the other one when we are deciding.
class SimpleAttributeFinderModule extends AttributeFinderModule {

	val attributes = readFromFile()

			protected def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType, multiValued: Boolean): Option[ConcreteValue] = {
		//ctx: contains the evaluationCtx
		//ctype: eg RESOURCE
		//name: eg creator
		//atype: type
		//TODO: implementation
		val key = cType.toString() ++ "." ++  name 
		val str = attributes.contains(key) match {
  		case true => attributes.get(key).get
  		case false =>  return None
		}
		
		if(multiValued) {
		  val lst = str.split(";")
		  Some(new StringSeqImpl(lst))
		} else {
		  Some(new StringImpl(str))
		}
	}


	private def readFromFile():HashMap[String,String] = {
			val res = HashMap.empty[String,String]
					try {
						for( line <- io.Source.fromFile("attributes").getLines()) {
							res += (line.split(" ")(0) -> line.split(" ")(1));
						}
					} catch {
					case ex: Exception => null
					}
			println(io.Source.fromFile("attributes").getLines())
			res

	}
}


