package tom.Policy2Filter.performance
import scala.collection.mutable.ListBuffer

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