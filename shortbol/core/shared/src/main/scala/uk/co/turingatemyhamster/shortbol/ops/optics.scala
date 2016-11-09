package uk.co.turingatemyhamster.shortbol.ops

import monocle.macros.{GenLens, GenPrism}
import monocle.{Iso, Optional, POptional, _}
import monocle.Monocle.{none => _, _}
import uk.co.turingatemyhamster.shortbol.longhandAst

import scalaz.{-\/, Applicative, \/-}

/**
  *
  *
  * @author Matthew Pocock
  */
object optics {
//
//  def seqListIso[A] = Iso[Seq[A], List[A]](_.to[List])(_.to[Seq])
//
//  object instanceExp {
//    val id = GenLens[InstanceExp](_.id)
//    val cstrApp = GenLens[InstanceExp](_.cstrApp)
//  }
//
//
//  object constructorApp {
//    val cstr = GenLens[ConstructorApp](_.cstr)
//    val body = GenLens[ConstructorApp](_.body) composeIso seqListIso[BodyStmt]
//  }
//
////  object bodyStmt {
////    type PropValue = Either[ValueExp, ConstructorApp]
////
////    val assignment = GenPrism[BodyStmt, BodyStmt.Assignment] composeLens
////      GenLens[BodyStmt.Assignment](_.assignment)
////    val blankLine = GenPrism[BodyStmt, BodyStmt.BlankLine] composeLens
////      GenLens[BodyStmt.BlankLine](_.blankLine)
////    val comment = GenPrism[BodyStmt, BodyStmt.Comment] composeLens
////      GenLens[BodyStmt.Comment](_.comment)
////    val instanceExp = GenPrism[BodyStmt, BodyStmt.InstanceExp] composeLens
////      GenLens[BodyStmt.InstanceExp](_.instanceExp)
//////    val constructorApp = GenPrism[BodyStmt, BodyStmt.ConstructorApp] composeLens
//////      GenLens[BodyStmt.ConstructorApp](_.constructorApp)
////
////    val property: Optional[BodyStmt, (Identifier, PropValue)] = new POptional[BodyStmt, BodyStmt, (Identifier, PropValue), (Identifier, PropValue)] {
////      override def getOrModify(s: BodyStmt) = {
////        s match {
////          case BodyStmt.Assignment(Assignment(p, v)) =>
////            \/-(p -> (Left(v) : PropValue))
////          case BodyStmt.InstanceExp(InstanceExp(i, c)) =>
////            \/-(i -> (Right(c) : PropValue))
////          case bs =>
////            -\/(bs)
////        }
////      }
////
////      override def set(b: (Identifier, PropValue)) = s => b match {
////        case (p, Left(v)) =>
////          BodyStmt.Assignment(Assignment(p, v))
////        case (i, Right(c)) =>
////          BodyStmt.InstanceExp(InstanceExp(i, c))
////      }
////
////      override def getOption(s: BodyStmt) = getOrModify(s) fold (
////       _ => None,
////        Some(_)
////      )
////      def modifyF[F[_]: Applicative](f: ((Identifier, PropValue)) => F[(Identifier, PropValue)])(s: BodyStmt): F[BodyStmt] =
////        getOption(s).fold(
////          Applicative[F].point(s))(
////          a => Applicative[F].map(f(a))(set(_)(s))
////        )
////
////      def modify(f: ((Identifier, PropValue)) => (Identifier, PropValue)): BodyStmt => BodyStmt =
////        s => getOption(s).fold(s)(a => set(f(a))(s))
////    }
////
////    def propValue(propId: Identifier) = property composePrism
////      optics.unsafeSelect(_._1 == propId) composeLens
////      second
////  }
//
//  object assignment {
//    val property = GenLens[Assignment](_.property)
//    val value = GenLens[Assignment](_.value)
//  }
//
//  object valueExp {
//    val identifier = GenPrism[ValueExp, ValueExp.Identifier] composeLens
//      GenLens[ValueExp.Identifier](_.identifier)
//    val literal = GenPrism[ValueExp, ValueExp.Literal] composeLens
//      GenLens[ValueExp.Literal](_.literal)
//  }
//
//  object topLevel {
//    object instanceExp {
//      val instanceExp = GenLens[TopLevel.InstanceExp](_.instanceExp)
//    }
//  }
//
//  object sbFile {
//    val tops = GenLens[SBFile](_.tops) composeIso seqListIso[TopLevel]
//  }
//
//  object sbEvaluatedFile {
//    val tops = GenLens[SBFile](_.tops) composeIso seqListIso[TopLevel.InstanceExp]
//  }
//
//  object list {
//
//    implicit def listSublist[A] = Optional({(lse: (List[A], Int, Int)) =>
//      val (list, start, end) = lse
//      val sublist = list.slice(start, end)
//      if(sublist.length == end - start)
//        Some(sublist)
//      else
//        None
//    })({sublist => lse =>
//      val (list, start, end) = lse
//      val (pfx, tail) = list splitAt end
//      val (head, dead) = pfx splitAt start
//      (head ::: sublist ::: tail, start, (end - pfx.length + sublist.length))
//    })
//  }

  def unsafeSelect[A](predicate: A => Boolean): Prism[A, A] =
    Prism[A, A](a => if (predicate(a)) Some(a) else None)(a => a)
}
