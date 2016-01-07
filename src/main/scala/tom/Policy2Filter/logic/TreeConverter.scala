package tom.Policy2Filter.logic

import stapl.core._

/**
 * Code based on the master thesis of Bart Van Der Plancken
 * https://github.com/Herrantyr/Thesis-tree-converter	
 * 
**/
class TreeConverter(var root: Policy, val knownAttributes : Set[Attribute] = Set.empty) {
 
  /****************************************************************************
   ****************************************************************************
   ****************************MAIN  ALGORITHM*********************************
   ****************************************************************************
   ****************************************************************************/
  
	def reduce(policy : Policy,ca: CombinationAlgorithm) : Policy = {
	  var policyList = List[Policy]()
	  var reduceList:List[Policy] = Nil
	  while(hasPolicyChildren(policy)) {
	   policyList = getLowestPolicies(policy)
	   reduceList = List[Policy]()
	   for(p <- policyList){
	     reduceList ::= reduceLeaves(p)
	   }
	   for(p <- reduceList){
	     var parent= p.parent.getOrElse(null).asInstanceOf[Policy]
	     var conversion = convertCA(p,parent.pca)
	     combinePolicies(conversion,parent)
	   }
	  }
	  var result = policy
	  if(policy.subpolicies.length>2)
	    if(policy.pca == FirstApplicable)
	       result = reduce(getHighestParent(reduceLeaves(policy)),ca)
	    else
	    	result = reduceLeaves(policy)
	  if(result.pca != ca)
	    result = convertCA(result,ca)
	 return result
	}

	/***************************************************************************
	****************************************************************************
	***********************HELPER  FUNCTIONS REDUCTION**************************
	****************************************************************************
	****************************************************************************/
	
	def getHighestParent(policy:Policy): Policy = {
	  var continueLoop = true
	  var result = policy
	  while(continueLoop){
		result.parent match {
			case Some(x) => result = x
			case None => continueLoop = false
		}
	  }
	  return result
	}
	
	def hasPolicyChildren(policy:Policy) = policy.subpolicies.exists(_.isInstanceOf[Policy]) 
	
	def getLowestPolicies(policy:Policy) : List[Policy] = {
	  var resultList = List[Policy]()
	  var children = policy.subpolicies
	  var includethis = true
	  for(c <- children)
	  {
	    if(c.isInstanceOf[Policy]){
	      includethis = false
	      resultList :::= getLowestPolicies(c.asInstanceOf[Policy])
	    }
	  }
	  if(includethis) 
	    return policy :: resultList 
	  else return resultList
	}
	
	def reduceLeaves(policy : Policy) : Policy = {
	  var ca = policy.pca
	  var newPolicy:Policy = null
	  if(policy.subpolicies.length <= 2) {
	    return policy
	  }
	  if(ca == DenyOverrides || ca == PermitOverrides) {
	    //Transformation 3.7
	    var rule1 = combineEffect(policy,Permit)
	    var rule2 = combineEffect(policy,Deny)
	    var subpolicies: List[AbstractPolicy] = List(rule1,rule2)
	    subpolicies = subpolicies.filter(c => c != null)
	    newPolicy = new Policy(policy.id)(policy.target,ca,subpolicies,policy.obligations)
	    newPolicy.parent = policy.parent
	    newPolicy.parent match {
	    	case Some(x) => x.subpolicies = replace(x.subpolicies,policy,newPolicy)
	    	case None => 
	    }
	  }else {
	    //transformation 3.8
		  newPolicy = createFAChain(policy)
	  }
	  return newPolicy
	}
	
	def combineEffect(policy: Policy, effect:Effect) : Rule = {
	  var subpols = policy.subpolicies
	  var children = subpols.filter(c => (c.asInstanceOf[Rule]).effect == effect)
	  var conditions = List[Expression]()
	  var newRule:Rule = null
	  if(children.length > 0){
	    for(c <- children.reverse){
	    	conditions ::= c.asInstanceOf[Rule].condition
	    }
	    var newExpression = conditions(0)
	    for(i <- 1 to conditions.length-1){
	      newExpression = newExpression | conditions(i)
	    }
	    newRule = new Rule(children(0).id)(effect,newExpression,List[ObligationAction]())
	  }
	  return newRule
	}
	
	def createFAChain(policy: Policy) : Policy = {
	  var parent = policy.parent
	  var newPol = FAChain(policy.subpolicies.map(x => x.asInstanceOf[Rule]), 0 , policy.id,policy.target,true)
	  newPol.parent = policy.parent
	  newPol.parent match {
	    case Some(x) => x.subpolicies = replace(x.subpolicies,policy,newPol)
	    case None => 
	  }
	  return getLowestPolicies(newPol).head
	}
	
	def replace(policies : List[AbstractPolicy], policy:AbstractPolicy, newPolicy : AbstractPolicy) : List[AbstractPolicy] = {
	  var resList = List[AbstractPolicy]()
	  for(x <- policies.reverse){
	    if(x == policy) resList ::= newPolicy else resList ::= x
	  }
	  return resList
	}
	
	def FAChain(rules : List[Rule], id : Int, startstring : String, target: Expression, includeTarget : Boolean) : Policy = {
	    var Ca:CombinationAlgorithm = DenyOverrides
		if(rules(0).effect == Permit) {
		  Ca = PermitOverrides
		}
		var children = List[AbstractPolicy]()
		var leftChild = rules(0)
		var rightChild:AbstractPolicy = rules(1)
		if(rules.length > 2) 
		  rightChild = FAChain(rules.tail,id+1,startstring, target, false) 
		children = List(leftChild,rightChild)
		if(includeTarget)
		  return new Policy(startstring + id.toString)(target,Ca,children,List[Obligation]())
		else
		  return new Policy(startstring + id.toString)(true,Ca,children,List[Obligation]())
	}
	
	def convertCA(policy: Policy, ca: CombinationAlgorithm) : Policy = {
	  if(policy.pca == ca)
	   return policy
	   var newpolicy: Policy = null
	   if(ca == PermitOverrides && policy.pca == DenyOverrides){
	     if(policy.subpolicies.length == 1){
	       newpolicy = new Policy(policy.id)(policy.target,PermitOverrides,policy.subpolicies)
	     }else if(policy.subpolicies(0).asInstanceOf[Rule].effect == Permit){
	       var A:Rule = policy.subpolicies(0).asInstanceOf[Rule]
	       var B:Rule = policy.subpolicies(1).asInstanceOf[Rule]
	       var newrule = new Rule(A.id)(Permit,And(A.condition,Not(B.condition)),List[ObligationAction]())
	       var subpolicies:List[AbstractPolicy] = List(B, newrule)
	       newpolicy = new Policy(policy.id)(policy.target,PermitOverrides,subpolicies)
	     }else{
	       var A:Rule = policy.subpolicies(0).asInstanceOf[Rule]
	       var B:Rule = policy.subpolicies(1).asInstanceOf[Rule]
	       var newrule = new Rule(B.id)(Permit,And(Not(A.condition),B.condition),List[ObligationAction]())
	       var subpolicies:List[AbstractPolicy] = List(A, newrule)
	       newpolicy = new Policy(policy.id)(policy.target,PermitOverrides,subpolicies)
	     }
	   }else if(ca == PermitOverrides && policy.pca == FirstApplicable){
	     if(policy.subpolicies.length == 1 || policy.subpolicies(0).asInstanceOf[Rule].effect == Permit ){
	       newpolicy = new Policy(policy.id)(policy.target, PermitOverrides,policy.subpolicies)
	     }else{
	       newpolicy = convertCA(new Policy(policy.id)(policy.target,DenyOverrides,policy.subpolicies)
	           ,PermitOverrides)
	     }
	     
	   }else if(ca == DenyOverrides && policy.pca == FirstApplicable){
	      if(policy.subpolicies.length == 1 || policy.subpolicies(0).asInstanceOf[Rule].effect == Deny ){
	       newpolicy = new Policy(policy.id)(policy.target, DenyOverrides,policy.subpolicies)
	     }else{
	       newpolicy = convertCA(new Policy(policy.id)(policy.target,PermitOverrides,policy.subpolicies)
	           ,DenyOverrides)
	     }
	   }else if(ca == DenyOverrides && policy.pca == PermitOverrides){
	     if(policy.subpolicies.length == 1){
	       newpolicy = new Policy(policy.id)(policy.target,PermitOverrides,policy.subpolicies)
	     }else if(policy.subpolicies(0).asInstanceOf[Rule].effect == Permit){
	       var A:Rule = policy.subpolicies(0).asInstanceOf[Rule]
	       var B:Rule = policy.subpolicies(1).asInstanceOf[Rule]
	       var newrule = new Rule(B.id)(Deny,(!A.condition) & B.condition,List[ObligationAction]())	
	       var subpolicies:List[AbstractPolicy] = List(A, newrule)
	       newpolicy = new Policy(policy.id)(policy.target,PermitOverrides,subpolicies)
	     }else{
	       var A:Rule = policy.subpolicies(0).asInstanceOf[Rule]
	       var B:Rule = policy.subpolicies(1).asInstanceOf[Rule]
	       var newrule = new Rule(A.id)(Deny,A.condition & (!B.condition),List[ObligationAction]())
	       var subpolicies:List[AbstractPolicy] = List(newrule,B)
	       newpolicy = new Policy(policy.id)(policy.target,PermitOverrides,subpolicies)
	     }
	   }else if(ca == FirstApplicable && policy.pca == PermitOverrides){
	     if(policy.subpolicies(0).asInstanceOf[Rule].effect == Permit){
	       newpolicy = new Policy(policy.id)(policy.target,FirstApplicable,policy.subpolicies)
	     }else{
	       newpolicy = new Policy(policy.id)(policy.target,FirstApplicable,policy.subpolicies.reverse)
	     }
	     
	   }else if(ca == FirstApplicable && policy.pca == DenyOverrides){
	     if(policy.subpolicies(0).asInstanceOf[Rule].effect == Deny){
	       newpolicy = new Policy(policy.id)(policy.target,FirstApplicable,policy.subpolicies)
	     }else{
	       newpolicy = new Policy(policy.id)(policy.target,FirstApplicable,policy.subpolicies.reverse)
	     }
	   }
	  newpolicy.parent = policy.parent
	  newpolicy.parent match {
	    case Some(x) => x.subpolicies = replace(x.subpolicies,policy,newpolicy)
	    case None => 
	  }
	  return newpolicy
	}
	
	def combinePolicies(child : Policy, parent : Policy) : Policy = {
	  val target = child.target
	  var subpolicies = List[AbstractPolicy]()
	  for(c <- child.subpolicies.reverse){
	    var r = c.asInstanceOf[Rule]
	    r =  new Rule(c.id)(r.effect,target & r.condition, List[ObligationAction]())
	    r.parent = Some(parent)
	    subpolicies ::= r
	  }
	  subpolicies = combineChildren(parent,child,subpolicies)
      parent.subpolicies = subpolicies
	  return parent
	}
	
	def combineChildren(parent: Policy, child: Policy, rules : List[AbstractPolicy]) : List[AbstractPolicy] = {
	  var resultList = List[AbstractPolicy]()
	  for(c <- parent.subpolicies.reverse){
	    if(c == child)
	      resultList :::= rules
	    else
	      resultList ::= c
	    c.parent = Some(parent)
	  }
	  return resultList
	}
	
	

}