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

import stapl.core._
import stapl.core.pdp._
import scala.collection.mutable.ListBuffer

/**
 * Object: PolicyReduce
 * Function: Partially evaluate the policy using the known attributes
 */
object PolicyReduce {
  
  //Translate to resource only policy
  def toResource(policy: Policy, ctx: EvaluationCtx): Either[Policy,Decision] = {
    val targetVal = ExpressionReduce.toResource(policy.target,ctx)
    var targetPolicy = policy
    targetVal match {
      case Right(x) => x match {
        case false => return Right(NotApplicable)
        case true => targetPolicy = new Policy(targetPolicy.id)(AlwaysTrue,targetPolicy.pca,targetPolicy.subpolicies.toList,targetPolicy.obligations)
      }
      case Left(reducedTarget) => targetPolicy = reduceResourceTarget(targetPolicy,reducedTarget)
    }
    
    val reducedSubpolicies = targetPolicy.subpolicies map {
      case x: Policy => PolicyReduce.toResource(x, ctx)
      case x: Rule => RuleReduce.toResource(x, ctx)
      case x => throw new UnsupportedOperationException("ReducePolicy does not support subpolicy " + x)
    }
    
    targetPolicy.pca match {
      case PermitOverrides => HandlePermitOverrides(targetPolicy,reducedSubpolicies,ctx)
      case DenyOverrides => HandleDenyOverrides(targetPolicy,reducedSubpolicies,ctx)
      case FirstApplicable => HandleFirstApplicable(targetPolicy,reducedSubpolicies,ctx)
    }
    
  }
  
  private def reduceResourceTarget(targetPolicy: Policy, targetExpression : Expression): Policy = {
    val newSubpolicies = new ListBuffer[AbstractPolicy]()
    for(subpolicy <- targetPolicy.subpolicies) {
      subpolicy match {
        case x: Rule => {
          val newCondition = And(x.condition,targetExpression)
          newSubpolicies += new Rule(x.id)(x.effect,newCondition,x.obligationActions)
        }
        case x: Policy => {
          val newTarget = And(x.target,targetExpression)
          newSubpolicies += new Policy(x.id)(newTarget,x.pca,x.subpolicies,x.obligations)
        }
      }
    }
    return new Policy(targetPolicy.id)(AlwaysTrue,targetPolicy.pca,newSubpolicies.toList,targetPolicy.obligations)
  }
  
  private def getPermits(subpolicies: List[Either[AbstractPolicy,Decision]]): Int = {
    subpolicies  count {
      case Right(x) => x match {
        case Permit => true
        case _ => false
      }
      case _ => false
    }
  }
  private def getDenies(subpolicies: List[Either[AbstractPolicy,Decision]]): Int = {
    subpolicies  count {
      case Right(x) => x match {
        case Deny => true
        case _ => false
      }
      case _ => false
    }
  }
  private def getNotApplicables(subpolicies: List[Either[AbstractPolicy,Decision]]): Int = {
    subpolicies  count {
      case Right(x) => x match {
        case NotApplicable => true
        case _ => false
      }
      case _ => false
    }
  }
  private def getNotDecided(subpolicies: List[Either[AbstractPolicy,Decision]]): List[AbstractPolicy] = {
    subpolicies filter {
      case Left(x) => true
      case _ => false
    } map {
      case Left(x) => x
      case _ => throw new IllegalStateException
    }
  }
  
  
  private def HandlePermitOverrides(policy: Policy,reducedSubpolicies: List[Either[AbstractPolicy,Decision]],ctx: EvaluationCtx): Either[Policy,Decision] = {
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
  private def HandleDenyOverrides(policy: Policy,reducedSubpolicies: List[Either[AbstractPolicy,Decision]],ctx: EvaluationCtx): Either[Policy,Decision] = {
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
  private def HandleFirstApplicable(policy: Policy,reducedSubpolicies: List[Either[AbstractPolicy,Decision]],ctx: EvaluationCtx): Either[Policy,Decision] = {
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
  
  private def HandleFirstApplicableNotDecided(policy: Policy,reducedSubpolicies: List[Either[AbstractPolicy,Decision]],ctx: EvaluationCtx): Either[Policy,Decision] = {
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

/**
 * Object: RuleReduce
 * Function: Partial evaluation of a STAPL Rule using the known attributes
 */
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

/**
 * Object: ExpressionReduce
 * Function: Partially evaluate the STAPL expression using known attributes
 */

object ExpressionReduce {
 
  def toResource(exp: Expression, ctx: EvaluationCtx): Either[Expression,Boolean] = {
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
