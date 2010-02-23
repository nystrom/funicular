package opencl

import scala.collection.mutable.Stack

object OpenCL {

  import com.nativelibs4java.opencl.library.{OpenCLLibrary => cl}

  def generate(e: Exp[_]): String = generate(e, new StringBuffer).toString

  def generate(e: Exp[_], sb: StringBuffer): StringBuffer = {
    e match {
      /*
      case b @ Block => {
          sb.append("{\n");
          for (s <- b.body) {
            generate(s, sb)
            sb.append("\n")
          }
          sb.append("}\n")
      }
      */
      // case If => void_t
      case Assign(lhs, rhs) => {
          generate(lhs, sb)
          sb.append(" = ")
          generate(rhs, sb)
          sb.append(";\n")
      }
      case KInt(n) => sb.append(n)
      case KLong(n) => sb.append(n)
      case KFloat(n) => sb.append(n)
      case KDouble(n) => sb.append(n)
      case Local(name, t) => sb.append(name)
      case Global(name, t) => sb.append(name)
      case Eval(e) => {
          generate(e, sb)
          sb.append(";\n")
      }
      case Bin(op, e1, e2) => {
          sb.append("(")
          generate(e1, sb)
          sb.append(" ")
          sb.append(op)
          sb.append(" ")
          generate(e2, sb)
          sb.append(")")
      }
      case Un(op, e1) => {
          sb.append("(")
          sb.append(op)
          generate(e1, sb)
          sb.append(")")
      }
      case Fun(x, e) => ()
      case App(f, a) => ()
      case Async(e) => ()
      case Finish(e) => ()
      case Even(e) => {
          sb.append("(")
          generate(e, sb)
          sb.append(").even")
      }
      case Odd(e) => {
          sb.append("(")
          generate(e, sb)
          sb.append(").odd")
      }
      case Lo(e) => {
          sb.append("(")
          generate(e, sb)
          sb.append(").lo")
      }
      case Hi(e) => {
          sb.append("(")
          generate(e, sb)
          sb.append(").hi")
      }
      case False => sb.append("((bool) 0)")
      case True => sb.append("((bool) 1)")
      case Inline(_, _) => ()
      case Vec(a) => ()
      case Cast(e, t) => ()
      case Reinterpret(e, t) => ()
      case Select(e, k) => {
          sb.append("(")
          generate(e, sb)
          sb.append(").s")
          sb.append(k)
      }
    }
    sb
  }

  def typeof(e: Exp[_]): Type = e match {
    // case Block => void_t
    // case If => void_t
    case Assign(lhs, rhs) => void_t
    case KInt(n) => int_t
    case KLong(n) => long_t
    case KFloat(n) => float_t
    case KDouble(n) => double_t
    case Local(name, t) => t
    case Global(name, t) => t
    case Eval(e) => void_t
    case Async(e) => void_t
    case Finish(e) => void_t
    case Bin("+", e1, e2) => typeof(e1)
    case Bin("-", e1, e2) => typeof(e1)
    case Bin("*", e1, e2) => typeof(e1)
    case Bin("/", e1, e2) => typeof(e1)
    case Bin("<", e1, e2) => bool_t
    case Bin(">", e1, e2) => bool_t
    case Bin("<=", e1, e2) => bool_t
    case Bin(">=", e1, e2) => bool_t
    case Bin("!=", e1, e2) => bool_t
    case Bin("==", e1, e2) => bool_t
    case Un("!", e) => bool_t
    case Un("~", e) => typeof(e)
    case Un("-", e) => typeof(e)
    case Fun(x, e) => fun_t(typeof(x), typeof(e))
    case App(f, a) => typeof(f) match { case fun_t(d, r) => r }
    case _ => void_t
  }

  sealed abstract class Type
  object void_t extends Type
  object bool_t extends Type
  object int_t extends Type
  object long_t extends Type
  object float_t extends Type
  object double_t extends Type
  object int2_t extends Type
  object long2_t extends Type
  object float2_t extends Type
  object double2_t extends Type
  object int4_t extends Type
  object long4_t extends Type
  object float4_t extends Type
  object double4_t extends Type
  object int8_t extends Type
  object long8_t extends Type
  object float8_t extends Type
  object double8_t extends Type
  object int16_t extends Type
  object long16_t extends Type
  object float16_t extends Type
  object double16_t extends Type
  case class fun_t[S <: Type,T <: Type](domain: S, range: T) extends Type

  implicit def castOps[S<:Type](e: Exp[S]) = new CastOps[S](e)

  implicit def boolOps(e: Exp[bool_t.type]) = new BoolOps(e)

  implicit def floatOps(e: Exp[float_t.type]) = new FloatOps[float_t.type](e, float_t)
  implicit def float2Ops(e: Exp[float2_t.type]) = new FloatOps[float2_t.type](e, float2_t)
  implicit def float4Ops(e: Exp[float4_t.type]) = new FloatOps[float4_t.type](e, float4_t)
  implicit def float8Ops(e: Exp[float8_t.type]) = new FloatOps[float8_t.type](e, float8_t)
  implicit def float16Ops(e: Exp[float16_t.type]) = new FloatOps[float16_t.type](e, float16_t)

  implicit def doubleOps(e: Exp[double_t.type]) = new FloatOps[double_t.type](e, double_t)
  implicit def double2Ops(e: Exp[double2_t.type]) = new FloatOps[double2_t.type](e, double2_t)
  implicit def double4Ops(e: Exp[double4_t.type]) = new FloatOps[double4_t.type](e, double4_t)
  implicit def double8Ops(e: Exp[double8_t.type]) = new FloatOps[double8_t.type](e, double8_t)
  implicit def double16Ops(e: Exp[double16_t.type]) = new FloatOps[double16_t.type](e, double16_t)

  implicit def intOps(e: Exp[int_t.type]) = new IntOps[int_t.type](e, int_t)
  implicit def int2Ops(e: Exp[int2_t.type]) = new IntOps[int2_t.type](e, int2_t)
  implicit def int4Ops(e: Exp[int4_t.type]) = new IntOps[int4_t.type](e, int4_t)
  implicit def int8Ops(e: Exp[int8_t.type]) = new IntOps[int8_t.type](e, int8_t)
  implicit def int16Ops(e: Exp[int16_t.type]) = new IntOps[int16_t.type](e, int16_t)

  implicit def longOps(e: Exp[long_t.type]) = new IntOps[long_t.type](e, long_t)
  implicit def long2Ops(e: Exp[long2_t.type]) = new IntOps[long2_t.type](e, long2_t)
  implicit def long4Ops(e: Exp[long4_t.type]) = new IntOps[long4_t.type](e, long4_t)
  implicit def long8Ops(e: Exp[long8_t.type]) = new IntOps[long8_t.type](e, long8_t)
  implicit def long16Ops(e: Exp[long16_t.type]) = new IntOps[long16_t.type](e, long16_t)

  class CastOps[S<:Type](e: Exp[S]) {
    def convertTo[T<:Type](t: T) = Cast[S,T](e, t)
    def reinterpretAs[T<:Type](t: T) = Reinterpret[S,T](e, t)
  }

  class BoolOps(e1: Exp[bool_t.type]) {
    def &&(e2: Exp[bool_t.type]) = Bin[bool_t.type,bool_t.type]("&&", e1, e2)
    def ||(e2: Exp[bool_t.type]) = Bin[bool_t.type,bool_t.type]("||", e1, e2)
    def &(e2: Exp[bool_t.type]) = Bin[bool_t.type,bool_t.type]("&", e1, e2)
    def |(e2: Exp[bool_t.type]) = Bin[bool_t.type,bool_t.type]("|", e1, e2)
    def ^(e2: Exp[bool_t.type]) = Bin[bool_t.type,bool_t.type]("^", e1, e2)
    def ==(e2: Exp[bool_t.type]) = Bin[bool_t.type,bool_t.type]("==", e1, e2)
    def !=(e2: Exp[bool_t.type]) = Bin[bool_t.type,bool_t.type]("!=", e1, e2)
    def ! = Un[bool_t.type,bool_t.type]("!", e1)
  }

  class FloatOps[T <: Type](e1: Exp[T], t: T) {
    def +(e2: Exp[T]) = Bin[T,T]("+", e1, e2)
    def -(e2: Exp[T]) = Bin[T,T]("-", e1, e2)
    def *(e2: Exp[T]) = Bin[T,T]("*", e1, e2)
    def /(e2: Exp[T]) = Bin[T,T]("/", e1, e2)

    def <(e2: Exp[T]) = Bin[T,bool_t.type]("<", e1, e2)
    def >(e2: Exp[T]) = Bin[T,bool_t.type](">", e1, e2)
    def <=(e2: Exp[T]) = Bin[T,bool_t.type](">=", e1, e2)
    def >=(e2: Exp[T]) = Bin[T,bool_t.type]("<=", e1, e2)
    def ==(e2: Exp[T]) = Bin[T,bool_t.type]("==", e1, e2)
    def !=(e2: Exp[T]) = Bin[T,bool_t.type]("!=", e1, e2)

    def - = Un[T,T]("-", e1)
  }

  class IntOps[T <: Type](e1: Exp[T], t: T) {
    def +(e2: Exp[T]) = Bin[T,T]("+", e1, e2)
    def -(e2: Exp[T]) = Bin[T,T]("-", e1, e2)
    def *(e2: Exp[T]) = Bin[T,T]("*", e1, e2)
    def /(e2: Exp[T]) = Bin[T,T]("/", e1, e2)
    def %(e2: Exp[T]) = Bin[T,T]("%", e1, e2)

    def <<(e2: Exp[T]) = Bin[T,T]("<<", e1, e2)
    def >>(e2: Exp[T]) = Bin[T,T](">>", e1, e2)
    def >>>(e2: Exp[T]) = Bin[T,T](">>>", e1, e2)

    def &(e2: Exp[T]) = Bin[T,T]("&", e1, e2)
    def |(e2: Exp[T]) = Bin[T,T]("|", e1, e2)
    def ^(e2: Exp[T]) = Bin[T,T]("^", e1, e2)

    def <(e2: Exp[T]) = Bin[T,bool_t.type]("<", e1, e2)
    def >(e2: Exp[T]) = Bin[T,bool_t.type](">", e1, e2)
    def <=(e2: Exp[T]) = Bin[T,bool_t.type](">=", e1, e2)
    def >=(e2: Exp[T]) = Bin[T,bool_t.type]("<=", e1, e2)
    def ==(e2: Exp[T]) = Bin[T,bool_t.type]("==", e1, e2)
    def !=(e2: Exp[T]) = Bin[T,bool_t.type]("!=", e1, e2)

    // def <<(e2: Exp[int_t.type]) = Bin("<<", e1, e2)
    // def >>(e2: Exp[int_t.type]) = Bin(">>", e1, e2)
    // def >>>(e2: Exp[int_t.type]) = Bin(">>>", e1, e2)

    def ~ = Un[T,T]("~", e1)
    def - = Un[T,T]("-", e1)
  }

  sealed abstract class Exp[T <: Type]

  case class Bin[S<:Type,T<:Type](op: String, e1: Exp[S], e2: Exp[S]) extends Exp[T]
  case class Un[S<:Type,T<:Type](op: String, e1: Exp[S]) extends Exp[T]

  case class Inline[T <: Type](vars: List[String], code: String) extends Exp[T]

  abstract class Var[T <: Type](name: String, typ: T) extends Exp[T]
  case class App[S <: Type,T <: Type](e0: Exp[fun_t[S,T]], e1: Exp[S]) extends Exp[T]
  case class Fun[S <: Type,T <: Type](x: Var[S], e: Exp[T]) extends Exp[fun_t[S,T]]
  object True extends Exp[bool_t.type]
  object False extends Exp[bool_t.type]
  case class KInt(value: Int) extends Exp[int_t.type]
  case class KLong(value: Long) extends Exp[long_t.type]
  case class KFloat(value: Float) extends Exp[float_t.type]
  case class KDouble(value: Double) extends Exp[double_t.type]

  implicit def int2VecOps(e: Exp[int2_t.type]) = new VecOps[int2_t.type, int_t.type, int_t.type](e)
  implicit def int4VecOps(e: Exp[int4_t.type]) = new VecOps[int4_t.type, int_t.type, int2_t.type](e)
  implicit def int8VecOps(e: Exp[int8_t.type]) = new VecOps[int8_t.type, int_t.type, int4_t.type](e)
  implicit def int16VecOps(e: Exp[int16_t.type]) = new VecOps[int16_t.type, int_t.type, int8_t.type](e)

  implicit def long2VecOps(e: Exp[long2_t.type]) = new VecOps[long2_t.type, long_t.type, long_t.type](e)
  implicit def long4VecOps(e: Exp[long4_t.type]) = new VecOps[long4_t.type, long_t.type, long2_t.type](e)
  implicit def long8VecOps(e: Exp[long8_t.type]) = new VecOps[long8_t.type, long_t.type, long4_t.type](e)
  implicit def long16VecOps(e: Exp[long16_t.type]) = new VecOps[long16_t.type, long_t.type, long8_t.type](e)

  implicit def float2VecOps(e: Exp[float2_t.type]) = new VecOps[float2_t.type, float_t.type, float_t.type](e)
  implicit def float4VecOps(e: Exp[float4_t.type]) = new VecOps[float4_t.type, float_t.type, float2_t.type](e)
  implicit def float8VecOps(e: Exp[float8_t.type]) = new VecOps[float8_t.type, float_t.type, float4_t.type](e)
  implicit def float16VecOps(e: Exp[float16_t.type]) = new VecOps[float16_t.type, float_t.type, float8_t.type](e)

  implicit def double2VecOps(e: Exp[double2_t.type]) = new VecOps[double2_t.type, double_t.type, double_t.type](e)
  implicit def double4VecOps(e: Exp[double4_t.type]) = new VecOps[double4_t.type, double_t.type, double2_t.type](e)
  implicit def double8VecOps(e: Exp[double8_t.type]) = new VecOps[double8_t.type, double_t.type, double4_t.type](e)
  implicit def double16VecOps(e: Exp[double16_t.type]) = new VecOps[double16_t.type, double_t.type, double8_t.type](e)

  class VecOps[V <: Type, E <: Type, H <: Type](e: Exp[V]) {
      def s0: Exp[E] = Select[V,E](e, 0)
      def s1: Exp[E] = Select[V,E](e, 1)
      def s2: Exp[E] = Select[V,E](e, 2)
      def s3: Exp[E] = Select[V,E](e, 3)
      def s4: Exp[E] = Select[V,E](e, 4)
      def s5: Exp[E] = Select[V,E](e, 5)
      def s6: Exp[E] = Select[V,E](e, 6)
      def s7: Exp[E] = Select[V,E](e, 7)
      def s8: Exp[E] = Select[V,E](e, 8)
      def s9: Exp[E] = Select[V,E](e, 9)
      def sa: Exp[E] = Select[V,E](e, 10)
      def sb: Exp[E] = Select[V,E](e, 11)
      def sc: Exp[E] = Select[V,E](e, 12)
      def sd: Exp[E] = Select[V,E](e, 13)
      def se: Exp[E] = Select[V,E](e, 14)
      def sf: Exp[E] = Select[V,E](e, 15)

      def lo: Exp[H] = Lo[V,H](e)
      def hi: Exp[H] = Hi[V,H](e)

      def even: Exp[H] = Even[V,H](e)
      def odd: Exp[H] = Odd[V,H](e)
  }

  case class Select[V <: Type, E <: Type](vec: Exp[V], i: Int) extends Exp[E]
  case class Lo[V <: Type, H <: Type](vec: Exp[V]) extends Exp[H]
  case class Hi[V <: Type, H <: Type](vec: Exp[V]) extends Exp[H]
  case class Even[V <: Type, H <: Type](vec: Exp[V]) extends Exp[H]
  case class Odd[V <: Type, H <: Type](vec: Exp[V]) extends Exp[H]

  implicit def int2vec(e: Tuple2[Exp[int_t.type],Exp[int_t.type]]) = Vec[int_t.type, int2_t.type](Array[Exp[int_t.type]](e._1, e._2))
  implicit def int2vec(e: Tuple4[Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type]]) = Vec[int_t.type, int4_t.type](Array[Exp[int_t.type]](e._1, e._1, e._2, e._3))
  implicit def int2vec(e: Tuple8[Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type]]) = Vec[int_t.type, int8_t.type](Array[Exp[int_t.type]](e._1, e._2, e._3, e._4, e._5, e._6, e._7, e._8))
  implicit def int2vec(e: Tuple16[Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type],Exp[int_t.type]]) = Vec[int_t.type, int16_t.type](Array[Exp[int_t.type]](e._1, e._2, e._3, e._4, e._5, e._6, e._7, e._8, e._9, e._10, e._11, e._12, e._13, e._14, e._15, e._16))

  implicit def long2vec(e: Tuple2[Exp[long_t.type],Exp[long_t.type]]) = Vec[long_t.type, long2_t.type](Array[Exp[long_t.type]](e._1, e._2))
  implicit def long2vec(e: Tuple4[Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type]]) = Vec[long_t.type, long4_t.type](Array[Exp[long_t.type]](e._1, e._1, e._2, e._3))
  implicit def long2vec(e: Tuple8[Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type]]) = Vec[long_t.type, long8_t.type](Array[Exp[long_t.type]](e._1, e._2, e._3, e._4, e._5, e._6, e._7, e._8))
  implicit def long2vec(e: Tuple16[Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type],Exp[long_t.type]]) = Vec[long_t.type, long16_t.type](Array[Exp[long_t.type]](e._1, e._2, e._3, e._4, e._5, e._6, e._7, e._8, e._9, e._10, e._11, e._12, e._13, e._14, e._15, e._16))

  implicit def float2vec(e: Tuple2[Exp[float_t.type],Exp[float_t.type]]) = Vec[float_t.type, float2_t.type](Array[Exp[float_t.type]](e._1, e._2))
  implicit def float2vec(e: Tuple4[Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type]]) = Vec[float_t.type, float4_t.type](Array[Exp[float_t.type]](e._1, e._1, e._2, e._3))
  implicit def float2vec(e: Tuple8[Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type]]) = Vec[float_t.type, float8_t.type](Array[Exp[float_t.type]](e._1, e._2, e._3, e._4, e._5, e._6, e._7, e._8))
  implicit def float2vec(e: Tuple16[Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type],Exp[float_t.type]]) = Vec[float_t.type, float16_t.type](Array[Exp[float_t.type]](e._1, e._2, e._3, e._4, e._5, e._6, e._7, e._8, e._9, e._10, e._11, e._12, e._13, e._14, e._15, e._16))

  implicit def double2vec(e: Tuple2[Exp[double_t.type],Exp[double_t.type]]) = Vec[double_t.type, double2_t.type](Array[Exp[double_t.type]](e._1, e._2))
  implicit def double2vec(e: Tuple4[Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type]]) = Vec[double_t.type, double4_t.type](Array[Exp[double_t.type]](e._1, e._1, e._2, e._3))
  implicit def double2vec(e: Tuple8[Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type]]) = Vec[double_t.type, double8_t.type](Array[Exp[double_t.type]](e._1, e._2, e._3, e._4, e._5, e._6, e._7, e._8))
  implicit def double2vec(e: Tuple16[Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type],Exp[double_t.type]]) = Vec[double_t.type, double16_t.type](Array[Exp[double_t.type]](e._1, e._2, e._3, e._4, e._5, e._6, e._7, e._8, e._9, e._10, e._11, e._12, e._13, e._14, e._15, e._16))

  // vector literals
  case class Vec[E <: Type, V <: Type](array: Array[Exp[E]]) extends Exp[V]

  // representation-preserving cast
  case class Cast[S <: Type, T <: Type](exp: Exp[S], t: T) extends Exp[T]
  // value-preserving cast
  case class Reinterpret[S <: Type, T <: Type](exp: Exp[S], t: T) extends Exp[T]

  type Env = String => Type

  /*
  def translate[T](t: Exp[T], env: Env): String = {
      t match {
          case Block => 
          */

  // def when(cond: Exp[bool_t.type])(body: => Unit) = If(cond, body)

  def async[T<:Type](body: => Exp[T]) = {
      val b = new Block
      Async(withBlock(b)(body))
  }

  def finish[T<:Type](body: => Exp[T]) = {
      val b = new Block
      Finish(withBlock(b)(body))
  }

  case class Async[T<:Type](exp: Exp[T]) extends Exp[void_t.type]
  case class Finish[T<:Type](exp: Exp[T]) extends Exp[void_t.type]

  implicit def v2k(value: Int) = KInt(value)
  implicit def v2k(value: Long) = KLong(value)
  implicit def v2k(value: Float) = KFloat(value)
  implicit def v2k(value: Double) = KDouble(value)

  implicit def assignOps[T <: Type](lhs: Var[T]) = new AssignOps(lhs)

  class AssignOps[T <: Type](lhs: Exp[T]) {
    def :=(rhs: Exp[T]) = {
      val a = Assign[T](lhs, rhs)
      block.body.push(a)
      a
    }
  }

  def eval[T<:Type](e: Exp[T]) = {
    val a = Eval[T](e)
    block.body.push(a)
    a
  }

  case class Eval[T<:Type](e: Exp[T]) extends Exp[void_t.type]

  case class Local[T<:Type](name: String, t: T) extends Var[T](name, t)
  case class Global[T<:Type](name: String, t: T) extends Var[T](name, t)

  case class Assign[T<:Type](lhs: Exp[T], rhs: Exp[T]) extends Exp[void_t.type]

  class Block extends Exp[void_t.type] {
    val body = new Stack[Exp[void_t.type]]
  }

  def withBlock[T<:Type](block: Block)(body: => Exp[T]) = {
     try {
       blocks push block
       body
     }
     finally {
       blocks.pop
     }
  }

  val blocks = new Stack[Block]
  def block = blocks.top

  /*
  class FunBuilder(formals: Array[Var], body: Exp[void_t.type]) {
    def this(formals: Var*) = this(formals, new Block)
    def apply(args: Exp[_]*): Exp[T] = {
      val a = Call[T](this, args)
      block.body.push(a)
      a
    }
  }

  def function[T](formals: Var*)(body: => Exp[T]): FunBuilder[T] = {
    val fun = new Fun[T](formals) 
    withBlock(fun.body)(body)
    fun
  }
  */

  /*
  def fft = {
    val fft1D_1024 = function(in, out, sMemx, sMemy) {
        // as above
    }

    withGPU {
      fft1D_1024(new ROBuffer[ty_float2](num_entries),
                 new RWBuffer[ty_float2](num_entries),
                 null,
                 null)
    }
  }
  */
}
