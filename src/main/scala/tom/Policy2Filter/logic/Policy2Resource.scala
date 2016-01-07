package tom.Policy2Filter.logic

import stapl.core._
import stapl.core.pdp._
import stapl.core.Result._

object Rule2Resource {
  def toResource(rule: Rule, ctx: EvaluationCtx): Either[Rule,Boolean] = {
    if(rule.effect != Permit) {
      throw new IllegalStateException("Can only decide over Permit operation" + rule.toString)
    }
    val x = Expression2Resource.toResource(rule.condition, ctx)
    x match {
      case Right(x) => return Right(x)
      case Left(x) => {
        return Left(new Rule(rule.id)(rule.effect,x,rule.obligationActions))
      }
    }
 
  }
}
object Expression2Resource {
 
  def toResource(exp: Expression, ctx: EvaluationCtx): Either[Expression,Boolean] = {
		exp match {
		case s: EqualsValue => simple2Resource(s, ctx)
		case s: GreaterThanValue => simple2Resource(s,ctx)
		case s: BoolExpression => simple2Resource(s,ctx)
		case s: ValueIn => simple2Resource(s,ctx)
		case s: Not => simple2Resource(s,ctx)
		case s: And => And2Resource(s,ctx)
		case s: Or => Or2Resource(s,ctx)
		case _ => throw new IllegalStateException("Unsupported type " + exp.getClass);
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
