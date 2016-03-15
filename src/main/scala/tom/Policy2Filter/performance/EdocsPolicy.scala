package tom.Policy2Filter.performance

import scala.collection.mutable.ListBuffer

trait PolicyForEval {
	  def getUsers(nrUsers: Int, nrOrganizations: Int, seed: Long, nrInLists: Int): List[String]
    def getPolicyString: String
}
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