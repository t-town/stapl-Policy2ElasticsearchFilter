package tom.Policy2Filter.logic

import stapl.core._
object toString {
  
  def AbstractPolicyToString(policy: AbstractPolicy): String = {
    policy match {
      case x: Rule => RuleToString(x)
      case x: Policy => PolicyToString(x)
    }
  }
  
  def RuleToString(rule: Rule): String = {
    "Rule(" +
    rule.effect +
    ") iff " +
    rule.condition +
    "\n"
  }
  
  def PolicyToString(policy: Policy): String = {
    val one = "Policy(" +
    policy.pca +
    " iff " +
    policy.target +
    "): \n"
    
    val two = policy.subpolicies.map { x => AbstractPolicyToString(x) }
    one + "\t" +  two.mkString("\t") + "\n"
  }
}