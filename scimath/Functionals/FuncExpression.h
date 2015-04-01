//# FuncExpression.h: An expression executable as function
//# Copyright (C) 2001,2002
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
//# $Id$

#ifndef SCIMATH_FUNCEXPRESSION_H
#define SCIMATH_FUNCEXPRESSION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/scimath/Functionals/FuncExprData.h>
#include <casacore/casa/stdvector.h>

//# Forward Declarations
#include <casacore/casa/iosfwd.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MUString;
template <class T> class Vector;

// <summary> An expression executable as function
// </summary>

// <use visibility=export> 

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Function>Function</linkto> class
// </prerequisite>
//
// <synopsis>
// This class acts as an interface between a program given as a string (e.g.
// from a command line interface) and a
// <linkto class=Function>Function</linkto> class. The grammar of the language
// use to express the function is given below. The <src>FuncEXpression</src>
// can be used in all places where Functions can be used (like in the
// linear and non-linear <linkto module=Fitting>Fitting</linkto> classes.
//
// An expression is created by either supplying a <src>String</src> to a 
// constructor, or be setting a <src>String</src>. 
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// To tie the Glish language to non-linear fitting procedures
// </motivation>
//
// <thrown>
//    <li> AipsError if an illegal program passed in constructor
// </thrown>
//
// <todo asof="2001/11/21">
//   <li> nothing directly
// </todo>

class FuncExpression {
 public:
  //# Enumerations

  //# Constructors
  // Construct an empty executable expression
  FuncExpression();
  // Construct an executable expression from the given string
  explicit FuncExpression(const String &prog);
  // Make this object a (deep) copy of other.
  FuncExpression(const FuncExpression &other);
  // Make this object a (deep) copy of other.
  FuncExpression &operator=(const FuncExpression &other);

  // Destructor
  ~FuncExpression() {}

  //# Member functions
  // Create an executable program
  Bool create(const String &prog);
  // Get the current error message
  const String &errorMessage() { return error_p; }
  // Get the executable program
  const vector<FuncExprData::ExprOperator> &getCode() const;
  // Get the number of parameters in executable program
  uInt getNpar() const { return npar_p; }
  // Get the number of dimensions of executable program
  uInt getNdim() const {return ndim_p; }
  // Get reference to the compiled program
  const vector<FuncExprData::ExprOperator> &getCode() { return code_p; }
  // Get reference to compiled constants
  const vector<Double> &getConst() { return const_p; }
  // Execute the program
  Bool exec(Double &res) const;
  // Print the stack information (mainly for debugging)
  void print(ostream &os) const;

 private:
  //# Data
  // The expression data /// later into a singleton
  FuncExprData exd;
  // The latest error message
  mutable String error_p;
  // The executable code stack (a vector, since it is a re-usable stack)
  vector<FuncExprData::ExprOperator> code_p;
  // The reverse Polish work stack (a vector, since deque did not work on gcc)
  vector<FuncExprData::ExprOperator> rps_p;
  // The current state of the compilation
  FuncExprData::ExprCompState state_p;
  // The current constant stack
  vector<Double> const_p;
  // The number of parameters in code
  uInt npar_p;
  // The number of dimensions of expression
  uInt ndim_p;
  // Executing stack
  mutable vector<Double> exec_p;

  //# Member functions
  // Compile a statement (in prg, which will be adjusted)
  Bool compStmt(MUString &prg);
  // Compile an expression (in prg, which will be adjusted)
  Bool compExpr(MUString &prg);
  // Compile a term (in prg, which will be adjusted)
  Bool compTerm(MUString &prg);
  // Save an operation on compilation RP stack.
  Bool setOp(FuncExprData::ExprOperator &oper);
  // Save a value on constant stack.
  Bool setVal(const Double &val);
  // Save an executable code
  Bool setCode(const FuncExprData::ExprOperator &oper);
  // Initialise the state
  void initState();
};

//# Global Functions

// <summary> Output function </summary>
// <group name=output>
// Show the program
ostream &operator<<(ostream &os, const FuncExpression &ed);
// </group>

// <summary> Execute function </summary>
// <group name=execute>
// Execute the program
template <class T>
T FuncExecute(const Vector<T> &x, const Vector<T> &par);
// </group>


} //# NAMESPACE CASACORE - END

#endif
