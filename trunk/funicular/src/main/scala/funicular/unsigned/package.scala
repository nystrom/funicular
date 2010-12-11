package funicular

package object unsigned {
  type UInt = Unsigned.UInt

  object EnhancedIntegralOps {
    trait EnhancedIntegralOps[T] {
      def bitCount: Int
      def highestOneBit: T
      def lowestOneBit: T
      def numberOfLeadingZeros: Int
      def numberOfTrailingZeros: Int

      def reverse: T
      def reverseBytes: T
      def rotateLeft(dist: Int): T
      def rotateRight(dist: Int): T

      def <<@(dist: Int) = rotateLeft(dist)
      def >>@(dist: Int) = rotateRight(dist)
    }

    class EnhancedIntOps(value: Int) extends EnhancedIntegralOps[Int] {
      import java.lang.Integer

      def bitCount = Integer.bitCount(value)
      def highestOneBit = Integer.highestOneBit(value)
      def lowestOneBit = Integer.lowestOneBit(value)
      def numberOfLeadingZeros = Integer.numberOfLeadingZeros(value)
      def numberOfTrailingZeros = Integer.numberOfTrailingZeros(value)

      def reverse = Integer.reverse(value)
      def reverseBytes = Integer.reverseBytes(value)
      def rotateLeft(dist: Int) = Integer.rotateLeft(value, dist)
      def rotateRight(dist: Int) = Integer.rotateRight(value, dist)
    
      /*
      def to(that: Long) = Range.Long.inclusive(value, that, 1)
      def until(that: Long) = Range.Long(value, that, 1)
      */
    }

    implicit def moreIntOps(value: Int) = new EnhancedIntOps(value)

    class EnhancedLongOps(value: Long) extends EnhancedIntegralOps[Long] {
      import java.lang.Long

      def bitCount = Long.bitCount(value)
      def highestOneBit = Long.highestOneBit(value)
      def lowestOneBit = Long.lowestOneBit(value)
      def numberOfLeadingZeros = Long.numberOfLeadingZeros(value)
      def numberOfTrailingZeros = Long.numberOfTrailingZeros(value)

      def reverse = Long.reverse(value)
      def reverseBytes = Long.reverseBytes(value)
      def rotateLeft(dist: Int) = Long.rotateLeft(value, dist)
      def rotateRight(dist: Int) = Long.rotateRight(value, dist)
    }

    implicit def moreLongOps(value: Long) = new EnhancedLongOps(value)

    class EnhancedUIntOps(value: UInt) extends EnhancedIntegralOps[UInt] {
      import java.lang.Integer
      def bitCount = Integer.bitCount(value.rep)
      def highestOneBit = UInt(Integer.highestOneBit(value.rep))
      def lowestOneBit = UInt(Integer.lowestOneBit(value.rep))
      def numberOfLeadingZeros = Integer.numberOfLeadingZeros(value.rep)
      def numberOfTrailingZeros = Integer.numberOfTrailingZeros(value.rep)
      def reverse = UInt(Integer.reverse(value.rep))
      def reverseBytes = UInt(Integer.reverseBytes(value.rep))
      def rotateLeft(dist: Int) = UInt(Integer.rotateLeft(value.rep, dist))
      def rotateRight(dist: Int) = UInt(Integer.rotateRight(value.rep, dist))
    }
    implicit def moreUIntOps(value: UInt) = new EnhancedUIntOps(value)
  }

  object Ordering {
  /*
    trait UByteOrdering extends Ordering[UByte] {
      def compare(x: UByte, y: UByte) = 
        if (x < y) -1
        else if (x == y) 0
        else 1
    }
    implicit object UByte extends UByteOrdering

    trait UShortOrdering extends Ordering[UShort] {
      def compare(x: UShort, y: UShort) = 
        if (x < y) -1
        else if (x == y) 0
        else 1
    }
    implicit object UShort extends UShortOrdering
  */

    trait UIntOrdering extends Ordering[UInt] {
      def compare(x: UInt, y: UInt) = 
        if (x < y) -1
        else if (x == y) 0
        else 1
    }
    implicit object UInt extends UIntOrdering

  /*
    trait ULongOrdering extends Ordering[ULong] {
      def compare(x: ULong, y: ULong) = 
        if (x < y) -1
        else if (x == y) 0
        else 1
    }
    implicit object ULong extends ULongOrdering
  */
  }

  /*
  // TODO: does not work since minus and negate return Int, not UInt
  object Numeric {
    trait UIntIsIntegral extends Integral[UInt] {
      def plus(x: UInt, y: UInt): UInt = x + y
      def minus(x: UInt, y: UInt): Int = x - y
      def times(x: UInt, y: UInt): UInt = x * y
      def quot(x: UInt, y: UInt): UInt = x / y
      def rem(x: UInt, y: UInt): UInt = x % y
      def negate(x: UInt): Int = -x
      def fromInt(x: Int): UInt = UInt(x)
      def toInt(x: UInt) = x.toInt
      def toLong(x: UInt) = x.toLong
      def toFloat(x: UInt) = x.toFloat
      def toDouble(x: UInt) = x.toDouble
    }
    implicit object UIntIsIntegral extends UIntIsIntegral with Ordering.UIntOrdering
  }
  */

  object Range {
    object UInt {
      def apply(start: UInt, end: UInt, step: UInt) =
        new Iterator[UInt] {
          var cur = start

          def hasNext = cur < end
          def next = {
            if (cur < end) {
              val t = cur
              cur += step
              t
            }
            else throw new NoSuchElementException
          }
        }
      def inclusive(start: UInt, end: UInt, step: UInt) = 
        new Iterator[UInt] {
          var cur = start

          def hasNext = cur <= end
          def next = {
            if (cur <= end) {
              val t = cur
              cur += step
              t
            }
            else throw new NoSuchElementException
          }
        }
    }
  }

  trait NumericConversions {
    def toUInt: UInt

    def toByte: Byte = toInt.toByte
    def toShort: Short = toInt.toShort
    def toChar: Char = toInt.toChar
    def toInt: Int = toLong.toInt
    def toLong: Long
    def toFloat: Float = toDouble.toFloat
    def toDouble: Double
  }

  implicit def s2r(x: scala.runtime.RichInt) = new RichIntUnsigned(x.self.asInstanceOf[Int])

  class RichIntUnsigned(x: Int) {
    def to(y: UInt) = UInt(x) to y
    def until(y: UInt) = UInt(x) until y
  }

  implicit def s2r(x: Byte) = new IntUnsigned(x)
  implicit def s2r(x: Short) = new IntUnsigned(x)
  implicit def s2r(x: Char) = new IntUnsigned(x)
  implicit def s2r(x: Int) = new IntUnsigned(x)
  implicit def s2r(x: Float) = new FloatUnsigned(x)
  implicit def s2r(x: Long) = new LongUnsigned(x)
  implicit def s2r(x: Double) = new DoubleUnsigned(x)

  class IntUnsigned(x: Int) {
    def toUInt = UInt(x)
    def +(y: UInt): Int = x + y.toInt
    def -(y: UInt): Int = x - y.toInt
    def *(y: UInt): Int = x * y.toInt
    def /(y: UInt): Int = x / y.toInt
    def %(y: UInt): Int = x % y.toInt
  }

  class LongUnsigned(x: Long) {
    def toUInt = UInt((x & 0xffffffffL).toInt)
    def +(y: UInt): Long = x + y.toLong
    def -(y: UInt): Long = x - y.toLong
    def *(y: UInt): Long = x * y.toLong
    def /(y: UInt): Long = x / y.toLong
    def %(y: UInt): Long = x % y.toLong
  }

  class FloatUnsigned(x: Float) {
    def toUInt = UInt((x.toLong & 0xffffffffL).toInt)
    def +(y: UInt): Float = x + y.toFloat
    def -(y: UInt): Float = x - y.toFloat
    def *(y: UInt): Float = x * y.toFloat
    def /(y: UInt): Float = x / y.toFloat
    def %(y: UInt): Float = x % y.toFloat
  }

  class DoubleUnsigned(x: Double) {
    def toUInt = UInt((x.toLong & 0xffffffffL).toInt)
    def +(y: UInt): Double = x + y.toDouble
    def -(y: UInt): Double = x - y.toDouble
    def *(y: UInt): Double = x * y.toDouble
    def /(y: UInt): Double = x / y.toDouble
    def %(y: UInt): Double = x % y.toDouble
  }

  object UInt {
    val MinValue = 0.toUInt
    val MaxValue = (-1).toUInt

    val zero = 0.toUInt
    val one = 1.toUInt

    def apply(rep: Int): UInt = Unsigned.UInt(rep)
    def unapply(x: UInt) = Some((x.rep))
  }

  object Unsigned {
    case class UInt(rep: Int) extends NumericConversions {
      private def rot(x: Int) = (x + Int.MinValue)

      def to(y: UInt) = Range.UInt.inclusive(this, y, 1.toUInt)
      def until(y: UInt) = Range.UInt(this, y, 1.toUInt)

      override def toUInt = this
      override def toInt = rep
      override def toLong = (rep & 0xffffffffL)
      override def toFloat = toLong.toFloat
      override def toDouble = toLong.toDouble

      def signum = if (rep == 0) 0 else 1
      def abs = this

      def compare(that: Int): Int = {
          if (that < 0) 1 /* this > that */
          else if (rep < 0) 1 /* this > MAXINT > that */
          else if (rep < that) 1
          else if (rep > that) -1
          else 0
      }
      def compare(that: UInt): Int = {
          if (this < that) 1
          else if (this > that) -1
          else 0
      }
      def max(that: UInt) = if (this < that) that else this
      def min(that: UInt) = if (this > that) that else this
      
      // Result of operation with a signed int is signed
      // Even if that int is positive!
      def +(x: Int) = rep + x
      def *(x: Int) = rep * x
      def -(x: Int) = rep - x
      def /(x: Int) = rep / x
      def %(x: Int) = rep % x

      def +(x: UInt) = UInt(rep + x.rep)
      def *(x: UInt) = UInt(rep * x.rep)
      def -(x: UInt) = rep - x.rep // signed

      // Algorithm from Hacker's Delight
      def /(x: UInt) = {
        val n = rep
        val d = x.rep
        val t = d >> 31
        val n_ = n & ~t
        val q = ((n_ >>> 1) / d) << 1
        val r = n - q * d
        UInt(q + (if (rot(r) >= rot(d)) 1 else 0))
      }

      // Algorithm from Hacker's Delight
      def %(x: UInt) = {
        val n = rep
        val d = x.rep
        val t = d >> 31
        val n_ = n & ~t
        val q = ((n_ >>> 1) / d) << 1
        UInt(n - q * d)
      }

      // baked in
      // def ==(x: Any) = x match {
      //   case UInt(r) => rep == r
      //   case _ => false
      // }
      // def !=(x: Any) = ! (this == x)

      // def ==(x: UInt) = rep == x.rep
      // def !=(x: UInt) = rep != x.rep

      def ==(x: Byte) = toLong.toInt == x && x >= 0
      def !=(x: Byte) = ! (this == x)
      def ==(x: Short) = toLong.toInt == x && x >= 0
      def !=(x: Short) = ! (this == x)
      def ==(x: Char) = toLong.toInt == x
      def !=(x: Char) = ! (this == x)
      def ==(x: Int) = toLong.toInt == x && x >= 0
      def !=(x: Int) = ! (this == x)
      def ==(x: Long) = toLong == x && x >= 0
      def !=(x: Long) = ! (this == x)
      def ==(x: Float) = toLong == x && x >= 0
      def !=(x: Float) = ! (this == x)
      def ==(x: Double) = toLong == x && x >= 0
      def !=(x: Double) = ! (this == x)

      def <(x: UInt) = rot(rep) < rot(x.rep)
      def >(x: UInt) = rot(rep) > rot(x.rep)
      def <=(x: UInt) = rot(rep) <= rot(x.rep)
      def >=(x: UInt) = rot(rep) >= rot(x.rep)

      override def toString = toLong.toString

      def +(x: java.lang.String) = this.toString + x

      def &(x: UInt) = UInt(rep & x.rep)
      def |(x: UInt) = UInt(rep | x.rep)
      def ^(x: UInt) = UInt(rep ^ x.rep)

      def <<(x: Int) = UInt(rep << x)
      def >>(x: Int) = UInt(rep >>> x) // note >>>
      def >>>(x: Int) = UInt(rep >>> x)
      def <<(x: Long) = UInt(rep << x)
      def >>(x: Long) = UInt(rep >>> x) // note >>>
      def >>>(x: Long) = UInt(rep >>> x)
      def <<(x: UInt) = UInt(rep << (x.rep & 0x1f))
      def >>(x: UInt) = UInt(rep >>> (x.rep & 0x1f))
      def >>>(x: UInt) = UInt(rep >>> (x.rep & 0x1f))
      def unary_+ = this
      def unary_- = -toLong.toInt
      def unary_~ = UInt(~rep)
    }
  }
}
