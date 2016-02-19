package tom.Policy2Filter.logic

import stapl.core._
import stapl.core.pdp._

//prune the policy before using it
object PolicyReduce {
  
  def toResource(policy: Policy, ctx: EvaluationCtx): Either[Policy,Decision] = {
    val targetVal = ExpressionReduce.toResource(policy.target,ctx)
    targetVal match {
      case Right(x) => x match {
        case false => return Right(NotApplicable)
        case true => 
      }
      case Left(x) => throw new UnsupportedOperationException("ReducePolicy does not currently support resource targets." + x)
    }
    
    val reducedSubpolicies = policy.subpolicies map {
      case x: Policy => PolicyReduce.toResource(x, ctx)
      case x: Rule => RuleReduce.toResource(x, ctx)
      case x => throw new UnsupportedOperationException("ReducePolicy does not support subpolicy " + x)
    }
    
    policy.pca match {
      case PermitOverrides => HandlePermitOverrides(policy,reducedSubpolicies,ctx)
      case DenyOverrides => HandleDenyOverrides(policy,reducedSubpolicies,ctx)
      case FirstApplicable => HandleFirstApplicable(policy,reducedSubpolicies,ctx)
    }
    
  }
  
  def getPermits(subpolicies: List[Either[AbstractPolicy,Decision]]): Int = {
    subpolicies  count {
      case Right(x) => x match {
        case Permit => true
        case _ => false
      }
      case _ => false
    }
  }
  def getDenies(subpolicies: List[Either[AbstractPolicy,Decision]]): Int = {
    subpolicies  count {
      case Right(x) => x match {
        case Deny => true
        case _ => false
      }
      case _ => false
    }
  }
  def getNotApplicables(subpolicies: List[Either[AbstractPolicy,Decision]]): Int = {
    subpolicies  count {
      case Right(x) => x match {
        case NotApplicable => true
        case _ => false
      }
      case _ => false
    }
  }
  def getNotDecided(subpolicies: List[Either[AbstractPolicy,Decision]]): List[AbstractPolicy] = {
    subpolicies filter {
      case Left(x) => true
      case _ => false
    } map {
      case Left(x) => x
      case _ => throw new IllegalStateException
    }
  }
  
  
  def HandlePermitOverrides(policy: Policy,reducedSubpolicies: List[Either[AbstractPolicy,Decision]],ctx: EvaluationCtx): Either[Policy,Decision] = {
    //Possibilities
    // There is a permit -> Permit
    // There is no permit
      // There are no undecided subpolicies
        //There is a deny -> Deny
        //There is no deny -> NotApplicable
      //There are undecided subpolicies
        //There is a deny -> Add defaultDeny to policy
        //There is no deny -> just the undecided subpolicies
    val permits = getPermits(reducedSubpolicies)
    if(permits > 0) {
      return Right(Permit)
    } else {
      val notDecided = getNotDecided(reducedSubpolicies)
      //If there are no remaining undecided policies we have a look at deny
      if(notDecided.size == 0) {
       val denies = getDenies(reducedSubpolicies)
       if(denies == 0) {
        //There are no denies, no Undecided and no Permits -> NotApplicable
         return Right(NotApplicable)
       } else {
         return Right(Deny)
       }
      } else {
        //There are undecided policies
        val denies = getDenies(reducedSubpolicies)
        //If there are denies, we add a defaultDeny
        if(denies == 0) {
          //there are no denies, no defaultDeny added
          val newPolicy = new Policy(policy.id)(policy.target,policy.pca,notDecided,policy.obligations)
          return Left(newPolicy)
        } else {
          val newSubpolicies = notDecided :+ new Rule("defaultDeny")(Deny,AlwaysTrue)
          val newPolicy = new Policy(policy.id)(policy.target,policy.pca,newSubpolicies,policy.obligations)
          return Left(newPolicy)
        }
      }

    }
  }
  def HandleDenyOverrides(policy: Policy,reducedSubpolicies: List[Either[AbstractPolicy,Decision]],ctx: EvaluationCtx): Either[Policy,Decision] = {
    //Possibilities
    // There is a Deny -> Deny
    // There is no Deny
      // There are no undecided subpolicies
        //There is a permit -> permit
        //There is no permit -> NotApplicable
      //There are undecided subpolicies
        //There is a permit -> Add defaultpermit to policy
        //There is no permit -> just the undecided subpolicies
    val denies = getDenies(reducedSubpolicies)
    if(denies > 0) {
      return Right(Deny)
    } else {
      val notDecided = getNotDecided(reducedSubpolicies)
      //If there are no remaining undecided policies we have a look at deny
      if(notDecided.size == 0) {
        val permits = getPermits(reducedSubpolicies)
         if(permits == 0) {
          //There are no denies, no Undecided and no Permits -> NotApplicable
           return Right(NotApplicable)
       } else {
         return Right(Permit)
       }
      } else {
        //There are undecided policies
        val permits = getPermits(reducedSubpolicies)
        //If there are denies, we add a defaultDeny
        if(permits == 0) {
          //there are no permits, no defaultPermit added
          val newPolicy = new Policy(policy.id)(policy.target,policy.pca,notDecided,policy.obligations)
          return Left(newPolicy)
        } else {
          val newSubpolicies = notDecided :+ new Rule("defaultPermit")(Permit,AlwaysTrue)
          val newPolicy = new Policy(policy.id)(policy.target,policy.pca,newSubpolicies,policy.obligations)
          return Left(newPolicy)
        }
      }

    }
  }
  def HandleFirstApplicable(policy: Policy,reducedSubpolicies: List[Either[AbstractPolicy,Decision]],ctx: EvaluationCtx): Either[Policy,Decision] = {
    //Two possibilities: the first subpolicy is a decision or not
    reducedSubpolicies foreach {
      x => x match {
        //First is undecided -> return policy unchanched / remove NotApplicables
        case Left(x) => {
          return HandleFirstApplicableNotDecided(policy,reducedSubpolicies,ctx) 
        }
        case Right(x) => x match {
          case Permit => return Right(Permit)
          case Deny => return Right(Deny)
          case NotApplicable => //Do nothing
        }
      }
    }
    //if we arrive here -> whole policy has only NotApplicables
    return Right(NotApplicable)
  }
  
  def HandleFirstApplicableNotDecided(policy: Policy,reducedSubpolicies: List[Either[AbstractPolicy,Decision]],ctx: EvaluationCtx): Either[Policy,Decision] = {
    //Everything that has been decided gets a defaultDeny,defaultPermit
    val newSubpolicies = reducedSubpolicies map {
      case Right(x) => x match {
        case Permit => new Rule("defaultPermit")(Permit,AlwaysTrue)
        case Deny => new Rule("defaultDeny")(Deny,AlwaysTrue)
        case NotApplicable => null
      }
      case Left(x) => x
    }
    val newFilteredSubpolicies = newSubpolicies.filter { x => x != null}
    val newPolicy = new Policy(policy.id)(policy.target,policy.pca,newFilteredSubpolicies,policy.obligations)
    return Left(newPolicy)
  }
  
}

object RuleReduce {
  
  def toResource(rule: Rule, ctx: EvaluationCtx): Either[Rule,Decision] = {
    
    //We evaluate the condition of this rule.
    //Iff the condition is true, the effect is returned.Permit/Deny
    //Iff the condition is false, the rule is NotApplicable
    //If the condition cannot be determined right now, the Rule is reduced.
    val x = ExpressionReduce.toResource(rule.condition, ctx)
    
    x match {
      //Boolean
      case Right(x) => x match {
        case true => Right(rule.effect)
        case false => Right(NotApplicable)
      }
      //rule
      case Left(x) => {
        //X: the new conditions
        return Left(new Rule(rule.id)(rule.effect,x,rule.obligationActions))
      }
    }
 
  }
}

object ExpressionReduce {
 
  def toResource(exp: Expression, ctx: EvaluationCtx): Either[Expression,Boolean] = {
    //println("matching: " ++ exp.toString())
		exp match {
  		case s: EqualsValue => simple2Resource(s, ctx)
  		case s: GreaterThanValue => simple2Resource(s,ctx)
  		case s: BoolExpression => simple2Resource(s,ctx)
  		case s: ValueIn => simple2Resource(s,ctx)
  		case s: Not => Not2Resource(s,ctx)
  		case s: And => And2Resource(s,ctx)
  		case s: Or => Or2Resource(s,ctx)
  		case AlwaysTrue => Right(true)
  		case AlwaysFalse => Right(false)
  		case _ => throw new IllegalStateException("Unsupported type for reduction" + exp.getClass);
		}
	}
  
  private def simple2Resource(s: Expression, ctx: EvaluationCtx): Either[Expression,Boolean] = {
      
  	  val x = try {    
  		  Right(s.evaluate(ctx))
  	  } catch {
  	    case e: AttributeNotFoundException => Left(s)
  	  }
  	  x
  }
  
  private def Not2Resource(s: Not, ctx: EvaluationCtx): Either[Expression,Boolean] = {
    val one: Either[Expression,Boolean] = toResource(s.expression,ctx)
    if(one.isRight) {
      one match {
        case Right(x) => return Right(! x)
      }
    } else {
      one match {
        case Left(x) => return Left(Not(x))
      }
    }
  }
  
  private def And2Resource(s: And, ctx: EvaluationCtx): Either[Expression,Boolean] = {
    val one: Either[Expression,Boolean] = toResource(s.expression1,ctx)
    val two: Either[Expression,Boolean] = toResource(s.expression2,ctx)
    //The rules for and:
    //one: 2xExpression
    if(one.isLeft && two.isLeft) {
      val left = one match {case Left(exp) => exp}
      val right = two match {case Left(exp) => exp}
      return Left(And(left,right))
    }
    //two: 1x Expression and 1 x resolved
    if(one.isRight) {
      val value = one match {case Right(x) => x}
      value match {
        case true => return two
        case false => return Right(false) //this is independent of two, and fails.
      }
    } else if(two.isRight) {
      val value = two match {case Right(x) => x}
      value match {
        case true => return one
        case false => return Right(false) //this is independent of one, and fails
      }
    } else {
      throw new IllegalStateException("Unknown case for AND" + s.toString())
    }
  }
  
  private def Or2Resource(s: Or, ctx: EvaluationCtx): Either[Expression,Boolean] = {
    val one: Either[Expression,Boolean] = toResource(s.expression1,ctx)
    val two: Either[Expression,Boolean] = toResource(s.expression2,ctx)
    //The rules for Or:
    //one: 2xExpression
    if(one.isLeft && two.isLeft) {
      val left = one match {case Left(exp) => exp}
      val right = two match {case Left(exp) => exp}
      return Left(Or(left,right))
    }
    if(one.isRight) {
      val value = one match {case Right(x) => x}
      value match {
        case true => return Right(true) //this is independent of two, OR succeeds
        case false => {
          if(two.isLeft) {
            return two
          } else {
            if(! (two match {case Right(x) => x})) { return Right(false) }
          }
        }//undetermined
      }
      
    } 
    if(two.isRight) {
      val value = two match {case Right(x) => x}
      value match {
        case true => return Right(true)
        case false =>  { //one case left: one is an expression:
          if(one.isLeft) {
            return one;
          } else {
            throw new IllegalStateException("unknown case for OR" + s.toString())
          }
        }
      }
    } else {
      throw new IllegalStateException("Unknown case for OR" + s.toString())
    }
    
  }
  

}
