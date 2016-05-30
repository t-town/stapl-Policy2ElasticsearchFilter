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
 * Class: AttributeStringBuilder
 * Function: Builds a random attribute string to be used when evaluating.
 */

class AttributeStringBuilder(nrUsers: Int, nrOrganizations: Int, seed: Long, nrInLists: Int) {
  val random = scala.util.Random
  random.setSeed(seed)

  val attributeString: StringBuilder = getBaseAttributeString

  
  private def getBaseAttributeString: StringBuilder = {
    val attributeString = new StringBuilder
    //Attributes: Action
    attributeString.append("ACTION.id view\n")
    //Attributes: Subject
    //subject.id
    attributeString.append("SUBJECT.id " + random.nextInt(nrUsers) + "\n")
    //subject.organization_id
    attributeString.append("SUBJECT.organization_id " + random.nextInt(nrOrganizations) + "\n")
    
    //List Attributes
    //subject.assigned_organizations
    attributeString.append("SUBJECT.assigned_organizations ")
    val assigned_orgs = new ListBuffer[String]()
    for(i <- 0 to nrInLists) {
      assigned_orgs += random.nextInt(nrOrganizations).toString
    }
    attributeString.append(assigned_orgs.toList.mkString(";") + "\n")
    
    //subject.supervisees
    val supervisees = new ListBuffer[String]()
    for(i <- 0 to nrInLists) {
      supervisees += random.nextInt(nrUsers).toString
    }
    attributeString.append("SUBJECT.supervisees " + supervisees.toList.mkString(";") + "\n")
    
    //fixed string attributes
    //subject.department :  Sales, Accounting, Audit en Board
    val departments = List("sales","accounting","audit","board")
    attributeString.append("SUBJECT.department " + departments(random.nextInt(departments.size)) + "\n")
    //subject.role : employee,manager,admin
    val roles = List("employee","manager","admin")
    attributeString.append("SUBJECT.role " + roles(random.nextInt(roles.size)) + "\n")
    attributeString
  }
  
  def addAttribute(attributeName: String, attributeValue: String) {
    attributeString.append(attributeName + " " + attributeValue + "\n")
  }
  
  def build: String = {
    attributeString.mkString
  }
  
  
  
}