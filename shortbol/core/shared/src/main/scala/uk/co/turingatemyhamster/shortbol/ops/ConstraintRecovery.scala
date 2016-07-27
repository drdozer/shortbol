package uk.co.turingatemyhamster.shortbol.ops

import uk.co.turingatemyhamster.shortbol.ast.{Identifier, Literal, ValueExp}

/**
  *
  *
  * @author Matthew Pocock
  */
trait ConstraintRecovery[A] {
  def recover(cv: ConstraintViolation[A], a: A) : Option[A]
}

object ConstraintRecovery {
//
//  def recoverLiteral(conversion: LiteralConversion): ConstraintRecovery[Literal] = new ConstraintRecovery[Literal] {
//    override def recover(cv: ConstraintViolation[Literal],
//                         a: Literal) = cv match {
//      case ViolationIn(lit: Literal, 'type, cf : ConstraintFailure[Set[Identifier]]) =>
//        cf match {
//          case ConstraintFailure(MemberOf(reqTpe: Identifier), obsTpes: Set[Identifier]) =>
//            obsTpes.foldLeft (None: Option[Literal] ) ((o, reqTpe) => if (o.isDefined) o else conversion (a, reqTpe) )
//          case _ => None
//        }
//      case _ => None
//    }
//  }
//
//  def recoverValueExp(crLiteral: ConstraintRecovery[Literal]): ConstraintRecovery[ValueExp] = new ConstraintRecovery[ValueExp] {
//
//    override def recover(cv: ConstraintViolation[ValueExp],
//                         a: ValueExp) = cv match {
//      case va@ViolationIn(ve: ValueExp, 'literal, cf : ConstraintViolation[Literal]) =>
//        va.asInstanceOf[ViolationIn[ValueExp, Symbol, Literal]].lens.modifyF(crLiteral.recover(cf, _))(a)
//      case _ => None
//    }
//  }

}
