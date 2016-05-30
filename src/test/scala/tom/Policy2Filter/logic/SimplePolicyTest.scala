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

import org.junit._
import Assert._
import stapl.core._
import stapl.parser._
import stapl.core.pdp._
import org.scalatest.junit.AssertionsForJUnit
import Thesis.Thesisbuild.Experiment._

/**
 * Test: Test all steps: transformation + reduction in a variety of scenarios.
 */
class SimplePolicyTest extends AssertionsForJUnit{
  val policyString1 = """
    		subject.organization_id = SimpleAttribute(String)			
				subject.department = SimpleAttribute(String)				
				subject.assigned_organizations = ListAttribute(String) 
				resource.creator = SimpleAttribute(String)
				resource.origin = SimpleAttribute(String)						
				resource.type = SimpleAttribute(String)
				resource.destination_organization = SimpleAttribute(String)
				action.id = SimpleAttribute(String)
    		Policy("Simple Policy") := when  (action.id === "view") apply FirstApplicable to (
    				Rule("Ownership rule") := permit iff (resource.creator === subject.id),
    				Rule("Origin") := deny iff (subject.organization_id === resource.destination_organization),
    				//Rule("permit") := permit,
    				Rule("Default deny") := deny
				)
    """
  val policy1 = Utility.parsePolicyString(policyString1)
  val policyString2 = """
    		subject.organization_id = SimpleAttribute(String)			
				subject.department = SimpleAttribute(String)				
				subject.assigned_organizations = ListAttribute(String) 

				resource.creator = SimpleAttribute(String)
				resource.origin = SimpleAttribute(String)						
				resource.type = SimpleAttribute(String)
				resource.destination_organization = SimpleAttribute(String)

				action.id = SimpleAttribute(String)

    		Policy("Simple Policy") := when  (action.id === "view") apply PermitOverrides to (
    				Rule("Ownership rule") := permit iff (resource.creator === subject.id),
    				Rule("Origin") := deny iff (subject.organization_id === resource.destination_organization),
    				//Rule("permit") := permit,
    				Rule("Default deny") := deny
				)
    """
  val policy2 = Utility.parsePolicyString(policyString2)
  val policyString3 = """
    		subject.organization_id = SimpleAttribute(String)			
				subject.department = SimpleAttribute(String)				
				subject.assigned_organizations = ListAttribute(String) 

				resource.creator = SimpleAttribute(String)
				resource.origin = SimpleAttribute(String)						
				resource.type = SimpleAttribute(String)
				resource.destination_organization = SimpleAttribute(String)

				action.id = SimpleAttribute(String)

    		Policy("Simple Policy") := when  (action.id === "view") apply DenyOverrides to (
    				Rule("Ownership rule") := permit iff (resource.creator === subject.id),
    				Rule("Origin") := deny iff (subject.organization_id === resource.destination_organization),
    				//Rule("permit") := permit,
    				Rule("Default deny") := deny
				)
    """
  val policy3 = Utility.parsePolicyString(policyString3)
  val policyString4 = """
    		subject.organization_id = SimpleAttribute(String)			
				subject.department = SimpleAttribute(String)				
				subject.assigned_organizations = ListAttribute(String) 

				resource.creator = SimpleAttribute(String)
				resource.origin = SimpleAttribute(String)						
				resource.type = SimpleAttribute(String)
				resource.destination_organization = SimpleAttribute(String)

				action.id = SimpleAttribute(String)

    		Policy("Simple Policy") := when  (action.id === "view") apply FirstApplicable to (
    				Rule("Ownership rule") := permit iff (resource.creator === subject.id),
    				Rule("Origin") := deny iff (!(subject.organization_id === resource.destination_organization)),
    				//Rule("permit") := permit,
    				Rule("Default deny") := deny
				)
    """
  val policy4 = Utility.parsePolicyString(policyString4)
    val policyString5 = """
    		subject.organization_id = SimpleAttribute(String)			
				subject.department = SimpleAttribute(String)				
				subject.assigned_organizations = ListAttribute(String) 

				resource.creator = SimpleAttribute(String)
				resource.origin = SimpleAttribute(String)						
				resource.type = SimpleAttribute(String)
				resource.destination_organization = SimpleAttribute(String)

				action.id = SimpleAttribute(String)

    		Policy("Simple Policy") := when  (action.id === "view") apply FirstApplicable to (
    				Rule("Ownership rule") := permit iff (resource.creator === subject.id),
    				Rule("Origin") := deny iff (!(subject.organization_id === resource.destination_organization)),
    				Rule("permit") := permit iff (subject.department === "Accounting"),
    				Rule("Default deny") := deny
				)
    """
  val policy5 = Utility.parsePolicyString(policyString5)

  
  def reduce(policy: Policy, ctx:EvaluationCtx): Decision = {
     val reduced = PolicyReduce.toResource(policy, ctx)
     if(reduced.isLeft) {
       val reducedPol = reduced match {case Left(x) => x}
       return reducedPol.evaluate(ctx).decision
     } else {
       return reduced match {case Right(x) => x}
     }
  }
  
  def evaluate(attributeString: String, policy: Policy) {
    val ctx = Utility.getSimpleCtx(attributeString)
    val treeConverter = new TreeConverter(null)
    val dec = policy.evaluate(ctx).decision
    
    //3 possible CombinationAlgorithms & 1 reduce
    val red = reduce(policy,ctx)
    var convertedPolicy = treeConverter.reduce(policy, PermitOverrides)
    val red1 = reduce(policy,ctx)
    val dec1 = convertedPolicy.evaluate(ctx).decision
    convertedPolicy = treeConverter.reduce(policy, DenyOverrides)
    val red2 = reduce(policy,ctx)
    val dec2 = convertedPolicy.evaluate(ctx).decision
    convertedPolicy = treeConverter.reduce(policy, FirstApplicable)
    val red3 = reduce(policy,ctx)
    val dec3 = convertedPolicy.evaluate(ctx).decision
    assertEquals(dec,dec1)
    assertEquals(dec,dec2)
    assertEquals(dec,dec3)
    assertEquals(dec,red)
    assertEquals(dec,red1)
    assertEquals(dec,red2)
    assertEquals(dec,red3)
  }
  
  @Test
  def NotApplicabletest() {
        val attributeString = """
    				ACTION.id NOTVIEW
    				SUBJECT.id 1
    				SUBJECT.organization_id 2
    				SUBJECT.assigned_organizations 0;1;2
    				SUBJECT.department Accounting
    				RESOURCE.creator 1
    				RESOURCE.destination_organization 1
    				""".trim()
    		evaluate(attributeString, policy1)
    		evaluate(attributeString, policy2)
    		evaluate(attributeString, policy3)
    		evaluate(attributeString, policy4)
    		evaluate(attributeString, policy5)
  }
  
  
  @Test
  def ApplicableTestFirstApplicableSecondNotApplicable() {
    //resource.creator === subject.id
    //subject.organization_id !== resource.destination_organization
        val attributeString = """
    				ACTION.id view
    				SUBJECT.id 1
    				SUBJECT.organization_id 0
    				SUBJECT.assigned_organizations 0;1;2
    				SUBJECT.department Accounting
    				RESOURCE.creator 1
    				RESOURCE.destination_organization 1
    				""".trim()
    		evaluate(attributeString, policy1)
    		evaluate(attributeString, policy2)
    		evaluate(attributeString, policy3)
    		evaluate(attributeString, policy4)
    		evaluate(attributeString, policy5)

  }
  
   @Test
  def ApplicableTestFirstApplicableSecondApplicable() {
    //resource.creator === subject.id
    //subject.organization_id === resource.destination_organization
        val attributeString = """
    				ACTION.id view
    				SUBJECT.id 1
    				SUBJECT.organization_id 1
    				SUBJECT.assigned_organizations 0;1;2
    				SUBJECT.department Accounting
    				RESOURCE.creator 1
    				RESOURCE.destination_organization 1
    				""".trim()
    		evaluate(attributeString, policy1)
    		evaluate(attributeString, policy2)
    		evaluate(attributeString, policy3)
    		evaluate(attributeString, policy4)
    		evaluate(attributeString, policy5)
  }
  
  @Test
  def ApplicableTestFirstNotApplicableSecondApplicable() {
    //resource.creator !== subject.id
    //subject.organization_id === resource.destination_organization
        val attributeString = """
    				ACTION.id view
    				SUBJECT.id 1
    				SUBJECT.organization_id 1
    				SUBJECT.assigned_organizations 0;1;2
    				SUBJECT.department Accounting
    				RESOURCE.creator 2
    				RESOURCE.destination_organization 1
    				""".trim()
    		evaluate(attributeString, policy1)
    		evaluate(attributeString, policy2)
    		evaluate(attributeString, policy3)
    		evaluate(attributeString, policy4)
    		evaluate(attributeString, policy5)
  }
  
    @Test
  def ApplicableTestFirstAndSecondNotApplicable() {
    //resource.creator !== subject.id
    //subject.organization_id !== resource.destination_organization
        val attributeString = """
    				ACTION.id view
    				SUBJECT.id 1
    				SUBJECT.organization_id w
    				SUBJECT.assigned_organizations 0;1;2
    				SUBJECT.department Accounting
    				RESOURCE.creator 2
    				RESOURCE.destination_organization 1
    				""".trim()
    		evaluate(attributeString, policy1)
    		evaluate(attributeString, policy2)
    		evaluate(attributeString, policy3)
    		evaluate(attributeString, policy4)
    		evaluate(attributeString, policy5)
  } 
    
    @Test
    def NotAccounting() {
        val attributeString = """
    				ACTION.id view
    				SUBJECT.id 1
    				SUBJECT.organization_id w
    				SUBJECT.assigned_organizations 0;1;2
    				SUBJECT.department NotAccounting
    				RESOURCE.creator 2
    				RESOURCE.destination_organization 1
    				""".trim()
    		evaluate(attributeString, policy1)
    		evaluate(attributeString, policy2)
    		evaluate(attributeString, policy3)
    		evaluate(attributeString, policy4)
    		evaluate(attributeString, policy5)
    }
}