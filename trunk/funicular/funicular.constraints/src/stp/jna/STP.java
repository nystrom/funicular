package stp.jna;

import com.sun.jna.*;

/** Native interface for STP library */
public class STP {
  static {
      Native.register("stp");
  }

  // o  : optimizations
  // c  : check counterexample
  // p  : print counterexample
  // h  : help
  // s  : stats
  // v  : print nodes
  public static native void vc_setFlags(char c);
  
  //! Flags can be NULL
  public static native VCPtr vc_createValidityChecker();
  
  // Basic types
  public static native TypePtr vc_boolType(VCPtr vc);
  
  //! Create an array type
  public static native TypePtr vc_arrayType(VCPtr vc, TypePtr typeIndex, TypePtr typeData);

  /////////////////////////////////////////////////////////////////////////////
  // Expr manipulation methods                                               //
  /////////////////////////////////////////////////////////////////////////////

  //! Create a variable with a given name and type 
  /*! The type cannot be a function type. The var name can contain
    only variables, numerals and underscore. If you use any other
    symbol, you will get a segfault. */  
  public static native ExprPtr vc_varExpr(VCPtr vc, String name, TypePtr type);

  //The var name can contain only variables, numerals and
  //underscore. If you use any other symbol, you will get a segfault.
  public static native ExprPtr vc_varExpr1(VCPtr vc, String name, 
		  int indexwidth, int valuewidth);

  //! Get the expression and type associated with a name.
  /*!  If there is no such Expr, a NULL Expr is returned. */
  //Expr vc_lookupVar(V vc, String name, Pointer* type);
  
  //! Get the type of the Expr.
  public static native TypePtr vc_getType(VCPtr vc, ExprPtr e);
  
  public static native int vc_getBVLength(VCPtr vc, ExprPtr e);

  //! Create an equality expression.  The two children must have the same type.
  public static native ExprPtr vc_eqExpr(VCPtr vc, ExprPtr child0, ExprPtr child1);
  
  // Boolean expressions
  
  // The following functions create Boolean expressions.  The children
  // provided as arguments must be of type Boolean (except for the
  // function vc_iteExpr(). In the case of vc_iteExpr() the
  // conditional must always be Boolean, but the ifthenpart
  // (resp. elsepart) can be bit-vector or Boolean type. But, the
  // ifthenpart and elsepart must be both of the same type)
  public static native ExprPtr vc_trueExpr(VCPtr vc);
  public static native ExprPtr vc_falseExpr(VCPtr vc);
  public static native ExprPtr vc_notExpr(VCPtr vc, ExprPtr child);
  public static native ExprPtr vc_andExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_orExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_impliesExpr(VCPtr vc, ExprPtr hyp, ExprPtr conc);
  public static native ExprPtr vc_iffExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  //The output type of vc_iteExpr can be Boolean (formula-level ite)
  //or bit-vector (word-level ite)
  public static native ExprPtr vc_iteExpr(VCPtr vc, ExprPtr conditional, ExprPtr ifthenpart, ExprPtr elsepart);
  
  //Boolean to single bit BV Expression
  public static native ExprPtr vc_boolToBVExpr(VCPtr vc, ExprPtr form);

  // Arrays
  
  //! Create an expression for the value of array at the given index
  public static native ExprPtr vc_readExpr(VCPtr vc, ExprPtr array, ExprPtr index);

  //! Array update; equivalent to "array WITH [index] := newValue"
  public static native ExprPtr vc_writeExpr(VCPtr vc, ExprPtr array, ExprPtr index, ExprPtr newValue);
  
  // Expr I/O
  //! Expr vc_parseExpr(V vc, String s);

  //! Prints 'e' to stdout.
  public static native void vc_printExpr(VCPtr vc, ExprPtr e);

  //! Prints 'e' into an open file descriptor 'fd'
  public static native void vc_printExprFile(VCPtr vc, ExprPtr e, int fd);

  //! Prints state of 'vc' into malloc'd buffer '*buf' and stores the 
  //  length into '*len'.  It is the responsibility of the caller to 
  //  free the buffer.
  //void vc_printStateToBuffer(V vc, char **buf, unsigned long *len);

  //! Prints 'e' to malloc'd buffer '*buf'.  Sets '*len' to the length of 
  //  the buffer. It is the responsibility of the caller to free the buffer.
  // public static native void vc_printExprToBuffer(V vc, E e, char **buf, unsigned long * len);

  //! Prints counterexample to stdout.
  public static native void vc_printCounterExample(VCPtr vc);

  //! Prints variable declarations to stdout.
  public static native void vc_printVarDecls(VCPtr vc);

  //! Prints asserts to stdout. The flag simplify_print must be set to
  //"1" if you wish simplification to occur dring printing. It must be
  //set to "0" otherwise
  public static native void vc_printAsserts(VCPtr vc, int simplify_print);

  //! Prints the state of the query to malloc'd buffer '*buf' and
  //stores ! the length of the buffer to '*len'.  It is the
  //responsibility of the caller to free the buffer. The flag
  //simplify_print must be set to "1" if you wish simplification to
  //occur dring printing. It must be set to "0" otherwise
  // public static native void vc_printQueryStateToBuffer(V vc, E e, 
				  // char **buf, unsigned long *len, int simplify_print);

  //! Similar to vc_printQueryStateToBuffer()
  // public static native void vc_printCounterExampleToBuffer(V vc, char **buf,unsigned long *len);

  //! Prints query to stdout.
  public static native void vc_printQuery(VCPtr vc);

  /////////////////////////////////////////////////////////////////////////////
  // Context-related methods                                                 //
  /////////////////////////////////////////////////////////////////////////////
  
  //! Assert a new formula in the current context.  
  /*! The formula must have Boolean type. */
  public static native void vc_assertFormula(VCPtr vc, ExprPtr e);
  
  //! Simplify e with respect to the current context
  public static native ExprPtr vc_simplify(VCPtr vc, ExprPtr e);

  //! Check validity of e in the current context. e must be a FORMULA
  //
  //if returned 0 then input is INVALID. 
  //
  //if returned 1 then input is VALID
  //
  //if returned 2 then ERROR
  public static native int vc_query(VCPtr vc, ExprPtr e);
  
  //! Return the counterexample after a failed query.
  public static native ExprPtr vc_getCounterExample(VCPtr vc, ExprPtr e);

  //! get size of counterexample, i.e. the number of variables/array
  //locations in the counterexample.
  public static native int vc_counterexample_size(VCPtr vc);
  
  //! Checkpoint the current context and increase the scope level
  public static native void vc_push(VCPtr vc);
  
  //! Restore the current context to its state at the last checkpoint
  public static native void vc_pop(VCPtr vc);

  //! Return an int from a constant bitvector expression
  public static native int getBVInt(ExprPtr e);

  //! Return an unsigned int from a constant bitvector expression
  public static native int getBVUnsigned(ExprPtr e);

  //! Return an unsigned long long int from a constant bitvector expressions
  public static native long getBVUnsignedLongLong(ExprPtr e);


  /**************************/
  /* BIT VECTOR OPERATIONS  */
  /**************************/
  public static native TypePtr vc_bvType(VCPtr vc, int no_bits);
  public static native TypePtr vc_bv32Type(VCPtr vc);
  
  public static native ExprPtr vc_bvConstExprFromStr(VCPtr vc, String binary_repr);
  public static native ExprPtr vc_bvConstExprFromInt(VCPtr vc, int n_bits, /*unsigned*/ int value);
  public static native ExprPtr vc_bvConstExprFromLL(VCPtr vc, int n_bits, /*unsigned*/ long value);
  public static native ExprPtr vc_bv32ConstExprFromInt(VCPtr vc, /*unsigned*/ int value);
  
  public static native ExprPtr vc_bvConcatExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_bvPlusExpr(VCPtr vc, int n_bits, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_bv32PlusExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_bvMinusExpr(VCPtr vc, int n_bits, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_bv32MinusExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_bvMultExpr(VCPtr vc, int n_bits, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_bv32MultExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  // left divided by right i.e. left/right
  public static native ExprPtr vc_bvDivExpr(VCPtr vc, int n_bits, ExprPtr left, ExprPtr right);
  // left modulo right i.e. left%right
  public static native ExprPtr vc_bvModExpr(VCPtr vc, int n_bits, ExprPtr left, ExprPtr right);
  // signed left divided by right i.e. left/right
  public static native ExprPtr vc_sbvDivExpr(VCPtr vc, int n_bits, ExprPtr left, ExprPtr right);
  // signed left modulo right i.e. left%right
  public static native ExprPtr vc_sbvModExpr(VCPtr vc, int n_bits, ExprPtr left, ExprPtr right);
  
  public static native ExprPtr vc_bvLtExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_bvLeExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_bvGtExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_bvGeExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  
  public static native ExprPtr vc_sbvLtExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_sbvLeExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_sbvGtExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_sbvGeExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  
  public static native ExprPtr vc_bvUMinusExpr(VCPtr vc, ExprPtr child);

  // bitwise operations: these are terms not formulas  
  public static native ExprPtr vc_bvAndExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_bvOrExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_bvXorExpr(VCPtr vc, ExprPtr left, ExprPtr right);
  public static native ExprPtr vc_bvNotExpr(VCPtr vc, ExprPtr child);
  
  public static native ExprPtr vc_bvLeftShiftExpr(VCPtr vc, int sh_amt, ExprPtr child);
  public static native ExprPtr vc_bvRightShiftExpr(VCPtr vc, int sh_amt, ExprPtr child);
  /* Same as vc_bvLeftShift only that the answer in 32 bits long */
  public static native ExprPtr vc_bv32LeftShiftExpr(VCPtr vc, int sh_amt, ExprPtr child);
  /* Same as vc_bvRightShift only that the answer in 32 bits long */
  public static native ExprPtr vc_bv32RightShiftExpr(VCPtr vc, int sh_amt, ExprPtr child);
  public static native ExprPtr vc_bvVar32LeftShiftExpr(VCPtr vc, ExprPtr sh_amt, ExprPtr child);
  public static native ExprPtr vc_bvVar32RightShiftExpr(VCPtr vc, ExprPtr sh_amt, ExprPtr child);
  public static native ExprPtr vc_bvVar32DivByPowOfTwoExpr(VCPtr vc, ExprPtr child, ExprPtr rhs);

  public static native ExprPtr vc_bvExtract(VCPtr vc, ExprPtr child, int high_bit_no, int low_bit_no);
  
  //accepts a bitvector and position, and returns a boolean
  //corresponding to that position. More precisely, it return the
  //equation (x[bit_no:bit_no] = 0)
  //FIXME  = 1 ?
  public static native ExprPtr vc_bvBoolExtract(VCPtr vc, ExprPtr x, int bit_no);  
  public static native ExprPtr vc_bvSignExtend(VCPtr vc, ExprPtr child, int nbits);
  
  /*C pointer support:  C interface to support C memory arrays in CVCL */
  public static native ExprPtr vc_bvCreateMemoryArray(VCPtr vc, String arrayName);
  public static native ExprPtr vc_bvReadMemoryArray(VCPtr vc, 
			  ExprPtr array, ExprPtr byteIndex, int numOfBytes);
  public static native ExprPtr vc_bvWriteToMemoryArray(VCPtr vc, 
			       ExprPtr array, ExprPtr  byteIndex, 
			       ExprPtr element, int numOfBytes);
  
  // return a string representation of the Expr e. The caller is responsible
  // for deallocating the string with free()
  public static native String exprString(ExprPtr e);
  
  // return a string representation of the Type t. The caller is responsible
  // for deallocating the string with free()
  public static native String typeString(TypePtr t);

  public static native ExprPtr getChild(ExprPtr e, int i);

  //1.if input expr is TRUE then the function returns 1;
  //
  //2.if input expr is FALSE then function returns 0;
  //
  //3.otherwise the function returns -1
  public static native int vc_isBool(ExprPtr e);

  /* Register the given error handler to be called for each fatal error.*/
  // public static native void vc_registerErrorHandler(void (*error_hdlr)(const String err_msg));

  public static native int vc_getHashQueryStateToBuffer(VCPtr vc, ExprPtr query);

  //destroys the STP instance, and removes all the created expressions
  public static native void vc_Destroy(VCPtr vc);

  //deletes the expression e
  public static native void vc_DeleteExpr(ExprPtr e);

  //Get the whole counterexample from the current context
  public static native WholeCounterExamplePtr vc_getWholeCounterExample(ExprPtr vc);

  //Get the value of a term expression from the CounterExample
//  public static native E vc_getTermFromCounterExample(V vc, E e, C c);

  // get the kind of the expression
  public static native int getExprKind (ExprPtr e);

  // get the number of children nodes
  public static native int getDegree (ExprPtr e);

  // get the bv length
  public static native int getBVLength(ExprPtr e);

  // get expression type
  public static native int getType (ExprPtr e);

  // get value bit width
  public static native int getVWidth (ExprPtr e);

  // get index bit width
  public static native int getIWidth (ExprPtr e);

  // Prints counterexample to an open file descriptor 'fd'
  public static native void vc_printCounterExampleFile(VCPtr vc, int fd);

  // get name of expression. must be a variable.
  public static native String exprName(ExprPtr e);
  
  // get the node ID of an Expr.
  public static native int getExprID (ExprPtr ex);
}
