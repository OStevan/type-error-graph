package ch.epfl.ognjanovic.stevan.error_messages.inox

import scala.util.parsing.input.{Position, Positional}

trait SimpleTypes {

  object SimpleTypes {

    sealed abstract class Type extends Positional {

      def withPos(pos: Position): Type = this match {
        case UnitType() => UnitType().setPos(pos)
        case BooleanType() => BooleanType().setPos(pos)
        case BitVectorType(signed, size) => BitVectorType(signed, size).setPos(pos)
        case IntegerType() => IntegerType().setPos(pos)
        case StringType() => StringType().setPos(pos)
        case CharType() => CharType().setPos(pos)
        case RealType() => RealType().setPos(pos)
        case FunctionType(f, t) => FunctionType(f, t).setPos(pos)
        case SetType(t) => SetType(t).setPos(pos)
        case BagType(t) => BagType(t).setPos(pos)
        case MapType(f, t) => MapType(f, t).setPos(pos)
        case TupleType(ts) => TupleType(ts).setPos(pos)
        case ADTType(i, as) => ADTType(i, as).setPos(pos)
        case TypeParameter(id) => TypeParameter(id).setPos(pos)
        case u: Unknown => u.copy().setPos(pos)
        case _ => this
      }

      def contains(unknown: Unknown): Boolean = this match {
        case other: Unknown => unknown == other
        case FunctionType(froms, to) => froms.exists(_.contains(unknown)) || to.contains(unknown)
        case MapType(from, to) => from.contains(unknown) || to.contains(unknown)
        case SetType(elem) => elem.contains(unknown)
        case BagType(elem) => elem.contains(unknown)
        case TupleType(elems) => elems.exists(_.contains(unknown))
        case ADTType(_, args) => args.exists(_.contains(unknown))
        case _ => false
      }

      def unknowns: Set[Unknown] = this match {
        case unknown: Unknown => Set(unknown)
        case FunctionType(froms, to) => froms.map(_.unknowns).fold(to.unknowns)(_ union _)
        case MapType(from, to) => from.unknowns union to.unknowns
        case SetType(elem) => elem.unknowns
        case BagType(elem) => elem.unknowns
        case TupleType(elems) => elems.map(_.unknowns).fold(Set[Unknown]())(_ union _)
        case ADTType(_, args) => args.map(_.unknowns).fold(Set[Unknown]())(_ union _)
        case _ => Set()
      }

      def replaceTypeParams(mapping: Map[Identifier, Type]): Type = this match {
        case TypeParameter(id) => mapping.getOrElse(id, this)
        case FunctionType(froms, to) => FunctionType(froms.map(_.replaceTypeParams(mapping)), to.replaceTypeParams(mapping))
        case MapType(from, to) => MapType(from.replaceTypeParams(mapping), to.replaceTypeParams(mapping))
        case SetType(elem) => SetType(elem.replaceTypeParams(mapping))
        case BagType(elem) => BagType(elem.replaceTypeParams(mapping))
        case TupleType(elems) => TupleType(elems.map(_.replaceTypeParams(mapping)))
        case ADTType(id, elems) => ADTType(id, elems.map(_.replaceTypeParams(mapping)))
        case _ => this
      }
    }

    case class UnitType() extends Type

    case class BooleanType() extends Type

    case class BitVectorType(signed: Boolean, size: Int) extends Type

    case class IntegerType() extends Type

    case class StringType() extends Type

    case class CharType() extends Type

    case class RealType() extends Type

    case class FunctionType(froms: Seq[Type], to: Type) extends Type

    case class MapType(from: Type, to: Type) extends Type

    case class SetType(elem: Type) extends Type

    case class BagType(elem: Type) extends Type

    case class TupleType(elems: Seq[Type]) extends Type

    case class ADTType(identifier: Identifier, args: Seq[Type]) extends Type

    case class TypeParameter(identifier: Identifier) extends Type

    final class Unknown private(private val identifier: Int) extends Type {
      override def equals(that: Any): Boolean =
        that.isInstanceOf[Unknown] && that.asInstanceOf[Unknown].identifier == identifier

      override def hashCode(): Int = identifier

      override def toString: String = "Unknown(" + identifier + ")"

      def copy(): Unknown = new Unknown(identifier)
    }

    object Unknown {
      private var next: Int = 0

      def fresh: Unknown = synchronized {
        val ret = next
        next += 1
        new Unknown(ret)
      }
    }

  }

}
