package stp

import stp.jna.{STP => S}
import stp.jna.VCPtr
import stp.jna.ExprPtr;
import stp.jna.TypePtr;

/** Wrapper around the JNA vc pointer */
object STP {
    sealed class QueryResult
    object INVALID extends QueryResult
    object VALID extends QueryResult
    object ERROR extends QueryResult

    object VC {
        def setFlags(c: Char) = S.vc_setFlags(c)
    }

    case class VC private (val p: VCPtr) {
        def this() = this(S.vc_createValidityChecker)

        // destroys the S instance, and removes all the created expressions
        def destroy = S.vc_Destroy(p)

        def boolType = Type(this, S.vc_boolType(p))

        def arrayType(typeIndex: Type, typeData: Type) =
            Type(this, S.vc_arrayType(p, typeIndex.p, typeData.p))

        def varExpr(name: String, tpe: Type) =
                Expr(this, S.vc_varExpr(p, name, tpe.p))

        def varExpr1(name: String, indexWidth: Int, valueWidth: Int) =
                Expr(this, S.vc_varExpr1(p, name, indexWidth, valueWidth))

        // ! Create an equality expression. The two children must have the same
        // type.
        def eqExpr(child0: Expr, child1: Expr) =
                Expr(this, S.vc_eqExpr(p, child0.p, child1.p))

        // Boolean expressions

        // The following functions create Boolean expressions. The children
        // provided as arguments must be of type Boolean (except for the
        // function vc_iteExpr(). In the case of vc_iteExpr() the
        // conditional must always be Boolean, but the ifthenpart
        // (resp. elsepart) can be bit-vector or Boolean type. But, the
        // ifthenpart and elsepart must be both of the same type)
        def trueExpr = Expr(this, S.vc_trueExpr(p))

        def falseExpr = Expr(this, S.vc_falseExpr(p))

        def notExpr(child: Expr) = Expr(this, S.vc_notExpr(p, child.p))

        def andExpr(children: Expr*): Expr = {
            if (children.length == 0)
                    return trueExpr
            var e = children(0)
            for (i <- 1 until children.length)
                e = new Expr(this, S.vc_andExpr(p, e.p, children(i).p))
            e
        }

        def orExpr(children: Expr*): Expr = {
            if (children.length == 0)
                    return falseExpr
            var e = children(0)
            for (i <- 1 until children.length)
                e = new Expr(this, S.vc_orExpr(p, e.p, children(i).p))
            e
        }

        def impliesExpr(child0: Expr, child1: Expr) =
                 Expr(this, S.vc_impliesExpr(p, child0.p, child1.p))

        def iffExpr(child0: Expr, child1: Expr) =
                 Expr(this, S.vc_iffExpr(p, child0.p, child1.p))

        // The output type of vc_iteExpr can be Boolean (formula-level ite)
        // or bit-vector (word-level ite)
        def iteExpr(child0: Expr, child1: Expr, child2: Expr) =
                 Expr(this, S.vc_iteExpr(p, child0.p, child1.p, child2.p))

        // Boolean to single bit BV Expression
        def boolToBVExpr(from: Expr) =
                 Expr(this, S.vc_boolToBVExpr(p, from.p))

        // Arrays

        // ! Create an expression for the value of array at the given index
        def readExpr(array: Expr, index: Expr) =
                 Expr(this, S.vc_readExpr(p, array.p, index.p))

        // ! Array update; equivalent to "array WITH [index] := newValue"
        def writeExpr(array: Expr, index: Expr, newValue: Expr) =
                 Expr(this, S.vc_writeExpr(p, array.p, index.p, newValue.p))

        def printExpr(e: Expr) = S.vc_printExpr(p, e.p)

        // ! Prints counterexample to stdout.
        def printCounterExample() = S.vc_printCounterExample(p)

        // ! Prints variable declarations to stdout.
        def printVarDecls() = S.vc_printVarDecls(p)

        // ! Prints asserts to stdout. The flag simplify_print must be set to
        // "1" if you wish simplification to occur dring printing. It must be
        // set to "0" otherwise
        def printAsserts(simplify_print: Boolean) =
                S.vc_printAsserts(p, if (simplify_print) 1 else 0)

        /*
         * //! Prints the state of the query to malloc'd buffer '*buf' and //stores
         * ! the length of the buffer to '*len'. It is the //responsibility of the
         * caller to free the buffer. The flag //simplify_print must be set to "1"
         * if you wish simplification to //occur dring printing. It must be set to
         * "0" otherwise static native void vc_printQueryStateToBuffer(VC vc,
         * Expr e, char **buf, unsigned long *len, int simplify_print)
         * 
         * //! Similar to vc_printQueryStateToBuffer() static native void
         * vc_printCounterExampleToBuffer(VC vc, char **buf,unsigned long *len)
         */

        // ! Prints query to stdout.
        def printQuery = S.vc_printQuery(p)

        // ///////////////////////////////////////////////////////////////////////////
        // Context-related methods //
        // ///////////////////////////////////////////////////////////////////////////

        // ! Assert a new formula in the current context.
        /* ! The formula must have Boolean type. */
        def assertFormula(e: Expr) = S.vc_assertFormula(p, e.p)

        // ! Check validity of e in the current context. e must be a FORMULA
        //
        // if returned 0 then input is INVALID.
        //
        // if returned 1 then input is VALID
        //
        // if returned 2 then ERROR

        def query(e: Expr): QueryResult = {
            val r = S.vc_query(p, e.p)
            r match {
                case 0 => INVALID
                case 1 => VALID
                case _ => ERROR
            }
        }

        def simplify(e: Expr) = Expr(this, S.vc_simplify(p, e.p))

        // ! Return the counterexample after a failed query.
        def getCounterExample(e: Expr) =
                 Expr(this, S.vc_getCounterExample(p, e.p))

        // ! get size of counterexample, i.e. the number of variables/array
        // locations in the counterexample.
        def getCounterExampleSize() = S.vc_counterexample_size(p)

        // ! Checkpoint the current context and increase the scope level
        def push = S.vc_push(p)

        // ! Restore the current context to its state at the last checkpoint
        def pop = S.vc_pop(p)

        /**************************/
        /* BIT VECTOR OPERATIONS */
        /**************************/
        def bvType(no_bits: Int) = Type(this, S.vc_bvType(p, no_bits))

        def bv32Type = Type(this, S.vc_bv32Type(p))

        def bvConstExprFromStr(binary_repr: String) =
                 Expr(this, S.vc_bvConstExprFromStr(p, binary_repr))

        def bvConstExprFromInt(n_bits: Int, value: Int) =
                 Expr(this, S.vc_bvConstExprFromInt(p, n_bits, value))

        def bvConstExprFromLL(n_bits: Int, value: Long) =
                 Expr(this, S.vc_bvConstExprFromLL(p, n_bits, value))

        def bv32ConstExprFromInt(value: Int) =
                 Expr(this, S.vc_bv32ConstExprFromInt(p, value))

        def bvConcatExpr(left: Expr, right: Expr) =
                 Expr(this, S.vc_bvConcatExpr(p, left.p, right.p))

        def bvPlusExpr(n_bits: Int, left: Expr, right: Expr) =
                 Expr(this, S.vc_bvPlusExpr(p, n_bits, left.p, right.p))

        def bv32PlusExpr(left: Expr, right: Expr) =
                 Expr(this, S.vc_bv32PlusExpr(p, left.p, right.p))

        def bvMinusExpr(n_bits: Int, left: Expr, right: Expr) =
                 Expr(this, S.vc_bvMinusExpr(p, n_bits, left.p, right.p))

        def bv32MinusExpr(left: Expr, right: Expr) =
                 Expr(this, S.vc_bv32MinusExpr(p, left.p, right.p))

        def bvMultExpr(n_bits: Int, left: Expr, right: Expr) =
                 Expr(this, S.vc_bvMultExpr(p, n_bits, left.p, right.p))

        def bv32MultExpr(left: Expr, right: Expr) =
                 Expr(this, S.vc_bv32MultExpr(p, left.p, right.p))

        // left divided by right i.e. left/right
        def bvDivExpr(n_bits: Int, left: Expr, right: Expr) =
                 Expr(this, S.vc_bvDivExpr(p, n_bits, left.p, right.p))

        // left modulo right i.e. left%right
        def bvModExpr(n_bits: Int, left: Expr, right: Expr) =
                 Expr(this, S.vc_bvModExpr(p, n_bits, left.p, right.p))

        // signed left divided by right i.e. left/right
        def sbvDivExpr(n_bits: Int, left: Expr, right: Expr) =
                 Expr(this, S.vc_sbvDivExpr(p, n_bits, left.p, right.p))

        // signed left modulo right i.e. left%right
        def sbvModExpr(n_bits: Int, left: Expr, right: Expr) =
                 Expr(this, S.vc_sbvModExpr(p, n_bits, left.p, right.p))

        def bvLtExpr(left: Expr, right: Expr) =
                 Expr(this, S.vc_bvLtExpr(p, left.p, right.p))

        def bvLeExpr(left: Expr, right: Expr) =
                 Expr(this, S.vc_bvLeExpr(p, left.p, right.p))

        def bvGtExpr(left: Expr, right: Expr) =
                 Expr(this, S.vc_bvGtExpr(p, left.p, right.p))

        def bvGeExpr(left: Expr, right: Expr) =
                 Expr(this, S.vc_bvGeExpr(p, left.p, right.p))

        def sbvLtExpr(left: Expr, right: Expr) =
                 Expr(this, S.vc_sbvLtExpr(p, left.p, right.p))

        def sbvLeExpr(left: Expr, right: Expr) =
                 Expr(this, S.vc_sbvLeExpr(p, left.p, right.p))

        def sbvGtExpr(left: Expr, right: Expr) =
                 Expr(this, S.vc_sbvGtExpr(p, left.p, right.p))

        def sbvGeExpr(left: Expr, right: Expr) =
                 Expr(this, S.vc_sbvGeExpr(p, left.p, right.p))

        // bitwise operations: these are terms not formulas
        def bvAndExpr(left: Expr, right: Expr) =
                 Expr(this, S.vc_bvAndExpr(p, left.p, right.p))

        def bvOrExpr(left: Expr, right: Expr) =
                 Expr(this, S.vc_bvOrExpr(p, left.p, right.p))

        def bvXorExpr(left: Expr, right: Expr) =
                 Expr(this, S.vc_bvXorExpr(p, left.p, right.p))

        def bvNotExpr(child: Expr) =
                 Expr(this, S.vc_bvNotExpr(p, child.p))

        def bvLeftShiftExpr(sh_amt: Int, child: Expr) =
                 Expr(this, S.vc_bvLeftShiftExpr(p, sh_amt, child.p))

        def bvRightShiftExpr(sh_amt: Int, child: Expr) =
                 Expr(this, S.vc_bvRightShiftExpr(p, sh_amt, child.p))

        /* Same as vc_bvLeftShift only that the answer in 32 bits long */
        def bv32LeftShiftExpr(sh_amt: Int, child: Expr) =
                 Expr(this, S.vc_bv32LeftShiftExpr(p, sh_amt, child.p))

        /* Same as vc_bvRightShift only that the answer in 32 bits long */
        def bv32RightShiftExpr(sh_amt: Int, child: Expr) =
                 Expr(this, S.vc_bv32RightShiftExpr(p, sh_amt, child.p))

        def bvVar32LeftShiftExpr(sh_amt: Expr, child: Expr) =
                 Expr(this, S.vc_bvVar32LeftShiftExpr(p, sh_amt.p, child.p))

        def bvVar32RightShiftExpr(sh_amt: Expr, child: Expr) =
                 Expr(this, S.vc_bvVar32RightShiftExpr(p, sh_amt.p, child.p))

        def bvVar32DivByPowOfTwoExpr(child: Expr, rhs: Expr) =
                 Expr(this, S.vc_bvVar32DivByPowOfTwoExpr(p, child.p, rhs.p))

        def bvExtract(child: Expr, high_bit_no: Int, low_bit_no: Int) =
                 Expr(this, S.vc_bvExtract(p, child.p, high_bit_no, low_bit_no))

        // accepts a bitvector and position, and returns a boolean
        // corresponding to that position. More precisely, it return the
        // equation (x[bit_no:bit_no] = 0)
        // FIXME = 1 ?
        def bvBoolExtract(child: Expr, bit_no: Int) =
                 Expr(this, S.vc_bvBoolExtract(p, child.p, bit_no))

        def bvSignExtend(child: Expr, nbits: Int) =
                 Expr(this, S.vc_bvSignExtend(p, child.p, nbits))

        /* C pointer support: C interface to support C memory arrays in CVCL */
        def bvCreateMemoryArray(arrayName: String) =
                 Expr(this, S.vc_bvCreateMemoryArray(p, arrayName))

        def bvReadMemoryArray(array: Expr, byteIndex: Expr, numOfBytes: Int) =
                 Expr(this, S.vc_bvReadMemoryArray(p, array.p, byteIndex.p, numOfBytes))

        def bvWriteToMemoryArray(array: Expr, byteIndex: Expr, element: Expr, numOfBytes: Int) =
                 Expr(this, S.vc_bvWriteToMemoryArray(p, array.p,
                                byteIndex.p, element.p, numOfBytes))

        /* Register the given error handler to be called for each fatal error. */
        // static native void vc_registerErrorHandler(void
        // (*error_hdlr)(const char* err_msg))

        def getHashQueryStateToBuffer(query: Expr) =
                S.vc_getHashQueryStateToBuffer(p, query.p)

        // Get the whole counterexample from the current context
        // static native WholeCounterExample vc_getWholeCounterExample(VC
        // vc)

        // Get the value of a term expression from the CounterExample
        // static native Expr vc_getTermFromCounterExample(VC vc, Expr e,
        // WholeCounterExample c)
    }

    object ExprKind {
        val values = List(
            UNDEFINED,
            SYMBOL,
            BVCONST,
            BVNEG,
            BVCONCAT,
            BVOR,
            BVAND,
            BVXOR,
            BVNAND,
            BVNOR,
            BVXNOR,
            BVEXTRACT,
            BVLEFTSHIFT,
            BVRIGHTSHIFT,
            BVSRSHIFT,
            BVVARSHIFT,
            BVPLUS,
            BVSUB,
            BVUMINUS,
            BVMULTINVERSE,
            BVMULT,
            BVDIV,
            BVMOD,
            SBVDIV,
            SBVREM,
            BVSX,
            BOOLVEC,
            ITE,
            BVGETBIT,
            BVLT,
            BVLE,
            BVGT,
            BVGE,
            BVSLT,
            BVSLE,
            BVSGT,
            BVSGE,
            EQ,
            NEQ,
            FALSE,
            TRUE,
            NOT,
            AND,
            OR,
            NAND,
            NOR,
            XOR,
            IFF,
            IMPLIES,
            READ,
            WRITE,
            ARRAY,
            BITVECTOR,
            BOOLEAN
        )
    }

    sealed class ExprKind
    object UNDEFINED extends ExprKind
    object SYMBOL extends ExprKind
    object BVCONST extends ExprKind
    object BVNEG extends ExprKind
    object BVCONCAT extends ExprKind
    object BVOR extends ExprKind
    object BVAND extends ExprKind
    object BVXOR extends ExprKind
    object BVNAND extends ExprKind
    object BVNOR extends ExprKind
    object BVXNOR extends ExprKind
    object BVEXTRACT extends ExprKind
    object BVLEFTSHIFT extends ExprKind
    object BVRIGHTSHIFT extends ExprKind
    object BVSRSHIFT extends ExprKind
    object BVVARSHIFT extends ExprKind
    object BVPLUS extends ExprKind
    object BVSUB extends ExprKind
    object BVUMINUS extends ExprKind
    object BVMULTINVERSE extends ExprKind
    object BVMULT extends ExprKind
    object BVDIV extends ExprKind
    object BVMOD extends ExprKind
    object SBVDIV extends ExprKind
    object SBVREM extends ExprKind
    object BVSX extends ExprKind
    object BOOLVEC extends ExprKind
    object ITE extends ExprKind
    object BVGETBIT extends ExprKind
    object BVLT extends ExprKind
    object BVLE extends ExprKind
    object BVGT extends ExprKind
    object BVGE extends ExprKind
    object BVSLT extends ExprKind
    object BVSLE extends ExprKind
    object BVSGT extends ExprKind
    object BVSGE extends ExprKind
    object EQ extends ExprKind
    object NEQ extends ExprKind
    object FALSE extends ExprKind
    object TRUE extends ExprKind
    object NOT extends ExprKind
    object AND extends ExprKind
    object OR extends ExprKind
    object NAND extends ExprKind
    object NOR extends ExprKind
    object XOR extends ExprKind
    object IFF extends ExprKind
    object IMPLIES extends ExprKind
    object READ extends ExprKind
    object WRITE extends ExprKind
    object ARRAY extends ExprKind
    object BITVECTOR extends ExprKind
    object BOOLEAN extends ExprKind

    case class Expr(val vc: VC, val p: ExprPtr) {

        override def toString = S.exprString(p)

        // ! Get the type of the Expr.
        def getType: Type = Type(vc, S.vc_getType(vc.p, p))

        def vc_getBVLength: Int = S.vc_getBVLength(vc.p, p)

        // ! Return an int from a constant bitvector expression
        def getBVInt: Int = S.getBVInt(p)

        // ! Return an unsigned int from a constant bitvector expression
        def getBVUnsigned: Int = S.getBVUnsigned(p)

        // ! Return an unsigned long long int from a constant bitvector expressions
        def getBVUnsignedLong: Long = S.getBVUnsignedLongLong(p)

        // return a string representation of the Expr e. The caller is responsible
        // for deallocating the string with free()
        def exprString: String = S.exprString(p)

        def getChild(i: Int): Expr = Expr(vc, S.getChild(p, i))

        // 1.if input expr is TRUE then the function returns 1;
        //
        // 2.if input expr is FALSE then function returns 0;
        //
        // 3.otherwise the function returns -1
        def isBool: Boolean = {
            val n = S.vc_isBool(p)
            assert(n == 0 || n == 1)
            return n != 0;
        }

        // deletes the expression e
        def destroy = {
            S.vc_DeleteExpr(p)
        }

        // ! Simplify e with respect to the current context
        def simplify = Expr(vc, S.vc_simplify(vc.p, p))

        def print = S.vc_printExpr(vc.p, p)

        // get the kind of the expression
        def getExprKind: ExprKind = {
                val n = S.getExprKind(p);
                if (n < 0 || n >= ExprKind.values.length)
                    UNDEFINED
                else
                    ExprKind.values(n)
        }

        // get the number of children nodes
        def getDegree = S.getDegree(p)

        // get the bv length
        def getBVLength = S.getBVLength(p)

        // get expression type
        def getTypeKind: TypeKind = {
                val n = S.getType(p)
                if (n < 0 || n >= TypeKind.values.length)
                    UNKNOWN_TYPE
                else
                    TypeKind.values(n)
        }

        // get value bit width
        def getVWidth() = S.getVWidth(p)

        // get index bit width
        def getIWidth() = S.getIWidth(p)

        // Prints counterexample to an open file descriptor 'fd'
        // public static native void vc_printCounterExampleFile(VC vc, int fd);

        // get name of expression. must be a variable.
        def exprName: String = S.exprName(p)

        // get the node ID of an Expr.
        def getExprID() = S.getExprID(p)
    }

    object TypeKind {
        val values = List(BOOLEAN_TYPE, BITVECTOR_TYPE, ARRAY_TYPE, UNKNOWN_TYPE)
    }

    sealed class TypeKind
    object BOOLEAN_TYPE extends TypeKind
    object BITVECTOR_TYPE extends TypeKind
    object ARRAY_TYPE extends TypeKind
    object UNKNOWN_TYPE extends TypeKind

    case class Type(vc: VC, p: TypePtr) {
        // return a string representation of the Type t. The caller is responsible
        // for deallocating the string with free()
        def typeString = S.typeString(p)
    }
}
