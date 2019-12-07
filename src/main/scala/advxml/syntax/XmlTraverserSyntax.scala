package advxml.syntax

import advxml.core.validate.{EitherEx, MonadEx, ValidatedEx}
import advxml.core.XmlTraverser
import cats.{~>, Id, Monad}
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}

import scala.util.{Failure, Success, Try}
import scala.xml.NodeSeq

/**
  * Advxml
  * Created by geirolad on 14/06/2019.
  *
  * @author geirolad
  */
private[syntax] trait XmlTraverserAbstractSyntax {

  implicit def idConverter[A[_]]: A ~> A = λ[A ~> A](a => a)

  implicit private[syntax] val idToOptionConverter: Id ~> Option = λ[Id ~> Option](Some(_))

  implicit class XmlTraverseNodeSeqOps(target: NodeSeq) {

    def \![F[_]: MonadEx](q: String): F[Id[NodeSeq]] =
      XmlTraverser.mandatory.immediateChildren(target, q)

    def \\![F[_]: MonadEx](q: String): F[Id[NodeSeq]] =
      XmlTraverser.mandatory.children(target, q)

    def \@![F[_]: MonadEx](q: String): F[Id[String]] =
      XmlTraverser.mandatory.attr(target, q)

    def ![F[_]: MonadEx]: F[Id[String]] =
      XmlTraverser.mandatory.text(target)

    def \?[F[_]: MonadEx](q: String): F[Option[NodeSeq]] =
      XmlTraverser.optional.immediateChildren(target, q)

    def \\?[F[_]: MonadEx](q: String): F[Option[NodeSeq]] =
      XmlTraverser.optional.children(target, q)

    def \@?[F[_]: MonadEx](q: String): F[Option[String]] =
      XmlTraverser.optional.attr(target, q)

    def ?[F[_]: MonadEx]: F[Option[String]] =
      XmlTraverser.optional.text(target)
  }

  implicit class XmlTraverseMonadExOps[F[_]: MonadEx, G[_]: Monad](fg: F[G[NodeSeq]])(
    implicit C: G ~> F,
    O: G ~> Option
  ) {

    import cats.implicits._

    def \!(q: String): F[Id[NodeSeq]] = mandatory(_.\![F](q))

    def \\!(q: String): F[Id[NodeSeq]] = mandatory(_.\\![F](q))

    def \@!(q: String): F[Id[String]] = mandatory(_.\@![F](q))

    def ! : F[Id[String]] = mandatory(_.![F])

    def \?(q: String): F[Option[NodeSeq]] = optional(_.\?[Try](q))

    def \\?(q: String): F[Option[NodeSeq]] = optional(_.\\?[Try](q))

    def \@?(q: String): F[Option[String]] = optional(_.\@?[Try](q))

    def ? : F[Option[String]] = optional(_.?[Try])

    private def mandatory[T](op: NodeSeq => F[T]): F[T] =
      fg.flatMap(C.apply).flatMap(op)

    private def optional[T](op: NodeSeq => Try[Option[T]]): F[Option[T]] =
      fg.map(O.apply(_).map(op).flatMap {
        case Failure(_)     => None
        case Success(value) => value
      })
  }
}

private[syntax] trait XmlTraverserTrySyntax extends XmlTraverserAbstractSyntax {

  import cats.instances.option._
  import cats.instances.try_._

  implicit private[syntax] val optionToTryConverter: Option ~> Try = λ[Option ~> Try] {
    case Some(value) => Success(value)
    case None        => Failure(new RuntimeException("Missing XML element."))
  }

  implicit private[syntax] val idToTryConverter: Id ~> Try = λ[Id ~> Try](Success(_))

  implicit class XmlTraverseTryOps(target: NodeSeq) extends XmlTraverseTryIdOps(Success(target))

  implicit class XmlTraverseTryIdOps(target: Try[Id[NodeSeq]]) extends XmlTraverseMonadExOps[Try, Id](target)

  implicit class XmlTraverseTryOptionOps(target: Try[Option[NodeSeq]])
      extends XmlTraverseMonadExOps[Try, Option](target)
}

private[syntax] trait XmlTraverserEitherSyntax extends XmlTraverserAbstractSyntax {
  import cats.instances.either._
  import cats.instances.option._

  implicit private[syntax] val optionToEitherConverter: Option ~> EitherEx =
    λ[Option ~> EitherEx] {
      case Some(value) => Right(value)
      case None        => Left(new RuntimeException("Missing XML element."))
    }

  implicit private[syntax] val idToEitherConverter: Id ~> EitherEx =
    λ[Id ~> EitherEx](Right(_))

  implicit class XmlTraverseEitherOps(target: NodeSeq) extends XmlTraverseEitherIdOps(Right(target))

  implicit class XmlTraverseEitherIdOps(target: EitherEx[Id[NodeSeq]])
      extends XmlTraverseMonadExOps[EitherEx, Id](target)

  implicit class XmlTraverseEitherOptionOps(target: EitherEx[Option[NodeSeq]])
      extends XmlTraverseMonadExOps[EitherEx, Option](target)
}

private[syntax] trait XmlTraverserValidatedSyntax extends XmlTraverserAbstractSyntax {

  import cats.instances.option._
  import advxml.instances.validate._

  implicit private[syntax] val idToValidatedExConverter: Id ~> ValidatedEx =
    λ[Id ~> ValidatedEx](Valid(_))

  implicit private[syntax] val optionToValidatedExConverter: Option ~> ValidatedEx =
    λ[Option ~> ValidatedEx] {
      case Some(value) => Valid(value)
      case None        => Invalid(NonEmptyList.of(new RuntimeException("Missing XML element.")))
    }

  implicit class XmlTraverseValidatedExOps(target: NodeSeq) extends XmlTraverseValidatedExIdOps(Valid(target))

  implicit class XmlTraverseValidatedExIdOps(target: ValidatedEx[Id[NodeSeq]])
      extends XmlTraverseMonadExOps[ValidatedEx, Id](target)

  implicit class XmlTraverseValidatedExOptionOps(target: ValidatedEx[Option[NodeSeq]])
      extends XmlTraverseMonadExOps[ValidatedEx, Option](target)
}
