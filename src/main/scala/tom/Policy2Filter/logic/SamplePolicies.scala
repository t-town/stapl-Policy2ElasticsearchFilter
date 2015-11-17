package tom.Policy2Filter.logic
/*******************************************************************************
 * Copyright 2014 KU Leuven Research and Developement - iMinds - Distrinet
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
 *    Technical Contact: jasper.bogaerts@cs.kuleuven.be
 *    Author: jasper.bogaerts@cs.kuleuven.be
 ******************************************************************************/


import stapl.core._
import stapl.core.dsl.Policy
import stapl.core.dsl.Rule
import stapl.core.dsl.when

/**
 * @author Jasper Bogaerts
 * @since Sep 29, 2015
 */

object Policy1 extends BasicPolicy {
	import stapl.core.dsl._

	//subject.id = SimpleAttribute(Number) // ID is already there
	resource.creator = SimpleAttribute(String)

	val policy = Policy("Simple policy with ownership rule") := when (action.id === "view") apply PermitOverrides to (
			Rule("Ownership rule") := permit iff (resource.creator === subject.id),
			Rule("Default deny") := deny
	)

}

object Policy2 extends BasicPolicy  {
	import stapl.core.dsl._

	//subject.id = SimpleAttribute(Number)
	subject.organization_id = SimpleAttribute(Number)			// The identifier that corresponds to the organization that the subject is affiliated to

	resource.creator = SimpleAttribute(Number)
	resource.origin = SimpleAttribute(Number)						// The identifier that corresponds to the organization at which the document was created

	val policy = Policy("Simple policy with ownership rule, extended with organization restriction") := when (action.id === "view") apply PermitOverrides to (
		Rule("Ownership rule") := permit iff (resource.creator === subject.id),
		Rule("Organization restriction") := permit iff (resource.origin === subject.organization_id),
		Rule("Default deny") := deny
	)
}

object Policy3 extends BasicPolicy  {
	import stapl.core.dsl._

	//subject.id = SimpleAttribute(Number)
	subject.organization_id = SimpleAttribute(Number)			// The identifier that corresponds to the organization that the subject is affiliated to
	subject.department = SimpleAttribute(String)				// Department associated with the subject (e.g., "Sales", "Accounting", "Audit", "Board")

	resource.creator = SimpleAttribute(Number)
	resource.origin = SimpleAttribute(Number)						// The identifier that corresponds to the organization at which the document was created

	val policy = Policy("Simple policy with ownership rule, extended with organization restriction 2") := when (action.id === "view") apply PermitOverrides to (
		Rule("Ownership rule") := permit iff (resource.creator === subject.id),
		Rule("Organization restriction") := permit iff ((resource.origin === subject.organization_id) & ("Accounting" === subject.department)),
		Rule("Default deny") := deny
	)
}

object Policy4 extends BasicPolicy {
	import stapl.core.dsl._

	//subject.id = SimpleAttribute(Number)
	subject.organization_id = SimpleAttribute(Number)			// The identifier that corresponds to the organization that the subject is affiliated to
	subject.organization_subscription = SimpleAttribute(String)	// The type of subscription the organization corresponding to the subject has (i.e., "Free", "Premium")
	subject.department = SimpleAttribute(String)				// Department associated with the subject (e.g., "Sales", "Accounting", "Audit", "Board")
	subject.assigned_organizations = ListAttribute(Number) // List of identifiers associated with organizations that are assigned to this subject

	resource.creator = SimpleAttribute(Number)
	resource.origin = SimpleAttribute(Number)						// The identifier that corresponds to the organization at which the document was created
	resource.type_ = SimpleAttribute(String)						// Document type (e.g., "Invoice", "Paycheck", "Contract")
	resource.destination_organization = SimpleAttribute(Number)

	action.id = SimpleAttribute(Number)

	val policy = Policy("Simple policy with ownership rule, origin and destination restriction") := when (action.id === "view") apply FirstApplicable to (
		Rule("Ownership rule") := permit iff (resource.creator === subject.id),
		Rule("Origin") := deny iff ((subject.organization_id === resource.destination_organization) & !(resource.origin in subject.assigned_organizations)),
		Rule("Organization restriction") := permit iff ((resource.origin === subject.organization_id) & ("Accounting" === subject.department)),
		Rule("Default deny") := deny
	)
}


