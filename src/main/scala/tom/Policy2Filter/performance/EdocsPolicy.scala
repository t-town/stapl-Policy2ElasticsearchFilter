package tom.Policy2Filter.performance
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

import scala.collection.mutable.ListBuffer

/**
 * Trait: PolicyForEval
 * Function: Gives a policy that can be used to evaluate the approach. getUsers gives a full set of users as to evaluate the whole policy tree.
 * Options: debug = true: print intermediate steps
 */

trait PolicyForEval {
	  def getUsers(nrUsers: Int, nrOrganizations: Int, seed: Long, nrInLists: Int): List[String]
    def getPolicyString: String
}

/**
 * Object: EdocsPolicy
 * Function: Wrapper for the eDocs policy.
 * Options: debug = true: print intermediate steps
 */

object EdocsPolicy extends PolicyForEval {
  
	  val policyString: String = io.Source.fromFile("EdocsPolicy").mkString
	  
	  def getPolicyString = policyString
	  
	  //Randomly makes new users every time they are needed
	  def getUsers(nrUsers: Int, nrOrganizations: Int, seed: Long, nrInLists: Int): List[String] = {
	    val lstb = new ListBuffer[String]
	    //subject.role = admin
	    var bldr: AttributeStringBuilder = new AttributeStringBuilder(nrUsers,nrOrganizations,seed,nrInLists)
	    bldr.addAttribute("SUBJECT.ROLE", "admin")
	    lstb += bldr.build
	    bldr = new AttributeStringBuilder(nrUsers,nrOrganizations,seed,nrInLists)
	    bldr.addAttribute("SUBJECT.ROLE", "manager")
	    lstb += bldr.build
	    bldr = new AttributeStringBuilder(nrUsers,nrOrganizations,seed,nrInLists)
	    bldr.addAttribute("SUBJECT.ROLE", "employee")
	    bldr.addAttribute("SUBJECT.department", "accounting")
	    lstb += bldr.build
	    bldr = new AttributeStringBuilder(nrUsers,nrOrganizations,seed,nrInLists)
	    bldr.addAttribute("SUBJECT.ROLE", "employee")
	    lstb += bldr.build
	    return lstb.toList

	    
	    
	  }
}