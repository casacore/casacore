//# FuncExprData.h: Data and enumerations for functional expressions
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

#ifndef SCIMATH_FUNCEXPRDATA_H
#define SCIMATH_FUNCEXPRDATA_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/stdmap.h>

//# Forward Declarations
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> Data and enumerations for functional expressions
// </summary>

// <use visibility=local> 

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Function>Function</linkto> class
// </prerequisite>
//
// <synopsis>
// This class provides enumerations for expression analysis; and data element
// descriptions
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
//    <li>
// </thrown>
//
// <todo asof="2001/11/21">
//   <li> nothing directly
// </todo>

class FuncExprData {
public:
  //# Enumerations
  // Operations
  enum opTypes {
    NOP=0,
    UNAMIN,
    UNAPLUS,
    NON,
    POW,
    GTE,
    LTE,
    EQ,
    NEQ,
    OR,
    AND,
    CONDEX,
    CONDEX2,
    CONDEX3,
    ADD,
    SUB,
    MUL,
    DIV,
    LT,
    GT,
    CONST,
    PARAM,
    ARG,
    TOIMAG,
    LBRACE,
    RBRACE,
    LPAREN,
    RPAREN,
    LBR,
    RBR,
    COMMA,
    FINISH,
    GOTO,
    GOTOF,
    GOTOT,
    SIN,
    COS,
    ATAN,
    ATAN2,
    ASIN,
    ACOS,
    EXP,
    EXP10,
    EXP2,
    LOG,
    LOG10,
    LOG2,
    ERF,
    ERFC,
    PI,
    EE,
    ABS,
    FLOOR,
    CEIL,
    ROUND,
    INT,
    FRACT,
    SQRT,
    COMPLEX,
    REAL,
    IMAG,
    AMPL,
    PHASE,
    // Number
    NopTypes };

  // Operation category
  enum opCategories {
    // Unary, binary  1 or 2 character
    UNA2, UNA1, BIN2, BIN1, 
    // Special and functions
    SPEC, FUNC,
    // Number
    NopCategories };
  // Special categories
  enum specAction {
    NONE,
    // Save environment while compiling
    SAVENV,
    // Indicate a GOTO
    GOTOPC,
    // Final expression codes
    FINAL };
  // Special priority levels
  enum specPriority {
    // Lowest priority at which right-to-left rather than left-to-right
    // execution
    RTLPRI = 44,
    // Priority for specials - start
    SPCPRI = 60,
    // Priority for finals
    FINPRI = 00 };

  // The compilation state descriptor
  struct ExprCompState {
    // Old index of low RPS boundary
    uInt rpslow;
    // # of values available on value stack
    uInt nval;
    // Argument count
    uInt argcnt;
    // Previous saved program counter
    uInt pcptr;
  };
  // The operator description: code; priority; # of arguments; # of arguments
  // used up (or produced for functions)
  struct ExprOperator {
    // The operator code
    opTypes code;
    // The name (or characters)
    String name;
    // The category
    opCategories category;
    // Execution priority
    uInt priority;
    // # of arguments necessary (or minimum)
    uInt narg;
    // max # of arguments (for function)
    uInt nmaxarg;
    // # of results produced/used
    Int nresult;
    // code info (like par/x index; jump distance
    Int info;
    // special action
    specAction special;
    // state
    ExprCompState state;
  };

  //# Constructors
  // Construct the data for the expression analysis
  FuncExprData();

  // Destructor
  ~FuncExprData() {}

  //# Member functions
  // Accessors of the various maps
  // <group>
  map<String, ExprOperator> &unary2() { return una2_p; }
  const map<String, ExprOperator> &unary2() const { return una2_p; }
  map<String, ExprOperator> &unary1() { return una1_p; }
  const map<String, ExprOperator> &unary1() const { return una1_p; }
  map<String, ExprOperator> &binary2() { return bin2_p; }
  const map<String, ExprOperator> &binary2() const { return bin2_p; }
  map<String, ExprOperator> &binary1() { return bin1_p; }
  const map<String, ExprOperator> &binary1() const { return bin1_p; }
  map<String, ExprOperator> &special() { return spop_p; }
  const map<String, ExprOperator> &special() const { return spop_p; }
  map<String, ExprOperator> &function() { return func_p; }
  const map<String, ExprOperator> &function() const { return func_p; }
  // </group>
  // Print an operator map
  void print(ostream &os,
	     const map<String, FuncExprData::ExprOperator> &m) const;
  // Print an operation
  void print(ostream &os, const FuncExprData::ExprOperator &pos) const;

private:
  //# Data
  // Unary operators of 2 characters
  map<String, ExprOperator> una2_p;
  // Unary operators of 1 character
  map<String, ExprOperator> una1_p;
  // Binary operators of 2 characters
  map<String, ExprOperator> bin2_p;
  // Binary operators of 1 character
  map<String, ExprOperator> bin1_p;
  // Special operators
  map<String, ExprOperator> spop_p;
  // Function names
  map<String, ExprOperator> func_p;
  // All operators
  map<opTypes, ExprOperator> allop_p;
};

//# Global Functions

// <summary> Output function </summary>

// <group name=output>
// Show a list of all defined operators and functions
ostream &operator<<(ostream &os, const FuncExprData &ed);
// </group>


} //# NAMESPACE CASACORE - END

#endif



