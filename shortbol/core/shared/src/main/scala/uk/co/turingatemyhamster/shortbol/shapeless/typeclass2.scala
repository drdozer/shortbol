package uk.co.turingatemyhamster.shortbol.shapeless

import shapeless._
import labelled.{FieldType, field}
import ops.record.{Keys, Values}

import scala.language.higherKinds

/**
 * A type class abstracting over the `product` operation of type classes over
 * types of kind `* -> *`, as well as deriving instances using an isomorphism.
 */
trait ProductTypeClass2[C[_, _]] extends Serializable {
  /**
   * Given a type class instance for `H`, and a type class instance for a
   * product, produce a type class instance for the product prepended with `H`.
   */
  def product[H, HH, T <: HList, TT <: HList](ch: C[H, HH], ct: C[T, TT]): C[H :: T, HH :: TT]

  /**
   * The empty product.
   */
  def emptyProduct: C[HNil, HNil]

  /**
   * Given an isomorphism between `F` and `G` at `FF` and `GG`, and a type class instance for `G` at `GG`,
   * produce a type class instance for `F` at `FF`.
   */
  def project[F, G, FF, GG](instance: => C[G, GG], to: F => G, from: GG => FF): C[F, FF]
}

trait ProductTypeClassCompanion2[C[_, _]] extends Serializable {
  def apply[T, TT](implicit ct: Lazy[C[T, TT]]): C[T, TT] = ct.value

  val typeClass: ProductTypeClass2[C]

  implicit def deriveHNil: C[HNil, HNil] = typeClass.emptyProduct

  implicit def deriveHCons[H, T <: HList, HH, TT <: HList] (implicit ch: Lazy[C[H, HH]], ct: Lazy[C[T, TT]]): C[H :: T, HH :: TT] =
    typeClass.product(ch.value, ct.value)

  implicit def deriveInstance[F, G, FF, GG](implicit gen: Generic.Aux[F, G], gGen: Generic.Aux[FF, GG], cg: Lazy[C[G, GG]]): C[F, FF] =
    typeClass.project(cg.value, gen.to _, gGen.from _)
}


///**
// * A type class abstracting over the `product` operation of type classes over
// * types of kind `* -> *`, as well as deriving instances using an isomorphism.
// * Refines ProductTypeClass2 with the addition of runtime `String` labels
// * corresponding to the names of the product elements.
// */
//trait LabelledProductTypeClass2[C[_, _]] extends Serializable {
//  /**
//   * Given a type class instance for `H`, and a type class instance for a
//   * product, produce a type class instance for the product prepended with `H`.
//   */
//  def product[H, T <: HList, HH, TT <: HList](name: String, ch: C[H, HH], ct: C[T, TT]): C[H :: T, HH :: TT]
//
//  /**
//   * The empty product.
//   */
//  def emptyProduct: C[HNil, HNil]
//
//  /**
//   * Given an isomorphism between `F` and `G` at `FF` and `GG`, and a type class instance for `G` at `GG`,
//   * produce a type class instance for `F` at `GG`.
//   */
//  def project[F, G, FF, GG](instance: => C[G, GG], to: F => G, from: GG => FF): C[F, FF]
//}
//
//trait LabelledProductTypeClassCompanion2[C[_, _]] extends Serializable {
//  def apply[T, TT](implicit ct: Lazy[C[T, TT]]): C[T, TT] = ct.value
//
//  val typeClass: LabelledProductTypeClass2[C]
//
//  trait Wrap[KV] extends Serializable {
//    type V
//    type VV
//    val unwrap: C[V, VV]
//    def label(v: V): KV
//    def unlabel(rec: KV): VV
//  }
//
//  object Wrap {
//    type Aux[KV, V0, VV0] = Wrap[KV] { type V = V0; type VV = VV0 }
//  }
//
//  implicit def deriveHNil: Wrap.Aux[HNil, HNil, HNil] =
//    new Wrap[HNil] {
//      type V = HNil
//      type VV = HNil
//      val unwrap = typeClass.emptyProduct
//      def label(v: HNil): HNil = HNil
//      def unlabel(rec: HNil): HNil = HNil
//    }
//
//  implicit def deriveHCons[HK <: Symbol, HV, TKV <: HList, HVV]
//    (implicit
//      ch: Lazy[C[HV, HVV]],
//      key: Witness.Aux[HK],
//      ct: Lazy[Wrap[TKV] { type V <: HList; type VV <: HList }]
//    ): Wrap.Aux[FieldType[HK, HV] :: TKV, HV :: ct.value.V, HVV :: ct.value.VV] =
//      new Wrap[FieldType[HK, HV] :: TKV] {
//        type V = HV :: ct.value.V
//        type VV = HVV :: ct.value.VV
//        val unwrap = typeClass.product(key.value.name, ch.value, ct.value.unwrap)
//        def label(v: HV :: ct.value.V): FieldType[HK, HV] :: TKV = field[HK](v.head) :: ct.value.label(v.tail)
//        def unlabel(rec: FieldType[HK, HV] :: TKV): HV :: ct.value.V = rec.head :: ct.value.unlabel(rec.tail)
//      }
//
//  implicit def deriveInstance[T, LKV, TT]
//    (implicit
//      lgen: LabelledGeneric.Aux[T, LKV],
//      lwclkv: Lazy[Wrap[LKV]]
//    ): C[T, TT] = {
//      import lwclkv.value._
//      val to: T => V = (t: T) => lwclkv.value.unlabel(lgen.to(t))
//      val from: VV => TT = (v: VV) => lgen.from(label(v))
//      typeClass.project(unwrap, to, from)
//    }
//}

/**
 * A type class additionally abstracting over the `coproduct` operation of type
 * classes over types of kind `* -> *`.
 */
trait TypeClass2[C[_, _]] extends ProductTypeClass2[C] {
  /**
   * Given two type class instances for `L` and `R` at `LL` and `RR`, produce a type class
   * instance for the coproduct `L :+: R` at `LL :+: RR`.
   */
  def coproduct[L, R <: Coproduct, LL, RR <: Coproduct](cl: => C[L, LL], cr: => C[R, RR]): C[L :+: R, LL :+: RR]

  /**
   * The empty coproduct
   */
  def emptyCoproduct: C[CNil, CNil]
}

trait TypeClassCompanion2[C[_, _]] extends ProductTypeClassCompanion2[C] {
  val typeClass: TypeClass2[C]

  implicit def deriveCNil: C[CNil, CNil] = typeClass.emptyCoproduct

  implicit def deriveCCons[H, T <: Coproduct, HH, TT <: Coproduct] (implicit ch: Lazy[C[H, HH]], ct: Lazy[C[T, TT]]): C[H :+: T, HH :+: TT] =
    typeClass.coproduct(ch.value, ct.value)
}

///**
// * A type class additionally abstracting over the `coproduct` operation of type
// * classes over types of kind `* -> *`.
// *
// * Name hints can be safely ignored.
// */
//trait LabelledTypeClass2[C[_, _]] extends LabelledProductTypeClass2[C] {
//  /**
//   * Given two type class instances for `L` and `R` at `LL` and `RR`, produce a type class
//   * instance for the coproduct `L :+: R` at `LL :+: RR`.
//   */
//  def coproduct[L, R <: Coproduct, LL, RR <: Coproduct](name: String, cl: => C[L, LL], cr: => C[R, RR]): C[L :+: R, LL :+: RR]
//
//  /**
//   * The empty coproduct
//   */
//  def emptyCoproduct: C[CNil, CNil]
//}

//trait LabelledTypeClassCompanion2[C[_, _]] extends LabelledProductTypeClassCompanion2[C] {
//  val typeClass: LabelledTypeClass2[C]
//
//  implicit def deriveCNil: Wrap.Aux[CNil, CNil, CNil] =
//    new Wrap[CNil] {
//      type V = CNil
//      type VV = CNil
//      val unwrap = typeClass.emptyCoproduct
//      def label(v: CNil): CNil = ???
//      def unlabel(rec: CNil): CNil = ???
//    }
//
//  implicit def deriveCCons[HK <: Symbol, HV, TKV <: Coproduct, HVV]
//    (implicit
//      ch: Lazy[C[HV, HVV]],
//      key: Witness.Aux[HK],
//      ct: Lazy[Wrap[TKV] { type V <: Coproduct; type VV <: Coproduct }]
//    ): Wrap.Aux[FieldType[HK, HV] :+: TKV, HV :+: ct.value.V, HVV :+: ct.value.VV] =
//      new Wrap[FieldType[HK, HV] :+: TKV] {
//        type V = HV :+: ct.value.V
//        val unwrap = typeClass.coproduct(key.value.name, ch.value, ct.value.unwrap)
//        def label(v: HV :+: ct.value.V): FieldType[HK, HV] :+: TKV =
//          v match {
//            case Inl(hv) => Inl(field[HK](hv))
//            case Inr(tv) => Inr(ct.value.label(tv))
//          }
//        def unlabel(rec: FieldType[HK, HV] :+: TKV): HV :+: ct.value.V =
//          rec match {
//            case Inl(hkv) => Inl(hkv)
//            case Inr(tkv) => Inr(ct.value.unlabel(tkv))
//          }
//      }
//}
