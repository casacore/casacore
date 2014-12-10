//# FuncExprData.cc: Data and enumerations for functional expressions
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

//# Includes
#include <casacore/scimath/Functionals/FuncExprData.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
FuncExprData::FuncExprData() :
  una2_p(), una1_p(), bin2_p(), bin1_p(),
  spop_p(), func_p() {
  ExprCompState state = {0,0,0,0};
  static const ExprOperator olist[] = {
    {UNAMIN,  "-",       UNA1,  RTLPRI,    1, 0, 0, 0, NONE,    state},
    {UNAPLUS, "+",       UNA1,  RTLPRI,    1, 0, 0, 0, NONE,    state},
    {NON,     "!",       UNA1,  28,        1, 0, 0, 0, NONE,    state},
    {POW,     "**",      BIN2,  RTLPRI+4,  2, 0, 1, 0, NONE,    state},
    {GTE,     ">=",      BIN2,  32,        2, 0, 1, 0, NONE,    state},
    {LTE,     "<=",      BIN2,  32,        2, 0, 1, 0, NONE,    state},
    {EQ,      "==",      BIN2,  32,        2, 0, 1, 0, NONE,    state},
    {NEQ,     "!=",      BIN2,  32,        2, 0, 1, 0, NONE,    state},
    {OR,      "||",      BIN2,  20,        2, 0, 1, 0, NONE,    state},
    {AND,     "&&",      BIN2,  20,        2, 0, 1, 0, NONE,    state},
    {ADD,     "+",       BIN1,  36,        2, 0, 1, 0, NONE,    state},
    {SUB,     "-",       BIN1,  36,        2, 0, 1, 0, NONE,    state},
    {MUL,     "*",       BIN1,  40,        2, 0, 1, 0, NONE,    state},
    {DIV,     "/",       BIN1,  40,        2, 0, 1, 0, NONE,    state},
    {POW,     "^",       BIN1,  RTLPRI+4,  2, 0, 1, 0, NONE,    state},
    {GT,      ">",       BIN1,  32,        2, 0, 1, 0, NONE,    state},
    {LT,      "<",       BIN1,  32,        2, 0, 1, 0, NONE,    state},
    {CONDEX,  "?",       BIN1,  16,        2, 0, 1, 0, SAVENV,  state},
    {CONDEX3, "CONDEX3", BIN1,  16,        2, 0, 1, 0, NONE,    state},
    {CONDEX2, ":",       SPEC,  17,        2, 0, 1, 0, FINAL,   state},
    {CONST,   "CONST",   SPEC,  SPCPRI,    0, 0,-1, 0, NONE,    state},
    {PARAM,   "PARAM",   SPEC,  SPCPRI,    0, 0,-1, 0, NONE,    state},
    {ARG,     "ARG",     SPEC,  SPCPRI,    0, 0,-1, 0, NONE,    state},
    {TOIMAG,  "TOIMAG",  SPEC,  SPCPRI,    0, 0, 0, 0, NONE,    state},
    {LBRACE,  "{",       SPEC,  SPCPRI,    0, 0, 0, 0, NONE,    state},
    {RBRACE,  "}",       SPEC,  FINPRI,    0, 0, 0, 0, NONE,    state},
    {LPAREN,  "(",       SPEC,  SPCPRI,    0, 0, 0, 0, SAVENV,  state},
    {RPAREN,  ")",       SPEC,  FINPRI,    0, 0, 0, 0, FINAL,   state},
    {LBR,     "[",       SPEC,  SPCPRI,    0, 0, 0, 0, SAVENV,  state},
    {RBR,     "]",       SPEC,  FINPRI,    0, 0, 0, 0, FINAL,   state},
    {COMMA,   ",",       BIN1,  FINPRI,    0, 0, 0, 0, FINAL,   state},
    {FINISH,  "FINISH",  SPEC,  FINPRI,    0, 0, 0, 0, FINAL,   state},
    {GOTO,    "GOTO",    SPEC,  FINPRI,    0, 0, 0, 0, GOTOPC,  state},
    {GOTOF,   "GOTOF",   SPEC,  FINPRI,    0, 0, 0, 0, GOTOPC,  state},
    {GOTOT,   "GOTOT",   SPEC,  FINPRI,    0, 0, 0, 0, GOTOPC,  state},
    {SIN,     "sin",     FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {COS,     "cos",     FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {ATAN,    "atan",    FUNC,  SPCPRI,    1, 2, 1, 0, SAVENV,  state},
    {ATAN2,   "atan2",   FUNC,  SPCPRI,    2, 2, 1, 0, SAVENV,  state},
    {ASIN,    "asin",    FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {ACOS,    "acos",    FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {EXP,     "exp",     FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {EXP2,    "exp2",    FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {EXP10,   "exp10",   FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {LOG,     "ln",      FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {LOG2,    "log2",    FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {LOG10,   "log",     FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {ERF,     "erf",     FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {ERFC,    "erfc",    FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {PI,      "pi",      FUNC,  SPCPRI,    0, 1, 1, 0, SAVENV,  state},
    {EE,      "ee",      FUNC,  SPCPRI,    0, 1, 1, 0, SAVENV,  state},
    {ABS,     "abs",     FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {FLOOR,   "floor",   FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {CEIL,    "ceil",    FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {ROUND,   "round",   FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {INT,     "int",     FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {FRACT,   "fract",   FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {SQRT,    "sqrt",    FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {COMPLEX, "complex", FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {REAL,    "real",    FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {IMAG,    "imag",    FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {AMPL,    "ampl",    FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},
    {PHASE,   "phase",   FUNC,  SPCPRI,    1, 1, 1, 0, SAVENV,  state},

    // End of list
    {NOP,     "NOP",     SPEC,  FINPRI,    0, 0, 0, 0, NONE,    state}
  };
  uInt i = 0;
  for (i=0; olist[i].code != NOP; ++i) {
    switch (olist[i].category) {
    case UNA2:
      una2_p[olist[i].name] = olist[i];
      break;
    case UNA1:
      una1_p[olist[i].name] = olist[i];
      break;
    case BIN2:
      bin2_p[olist[i].name] = olist[i];
      break;
    case BIN1:
      bin1_p[olist[i].name] = olist[i];
      break;
    case SPEC:
      spop_p[olist[i].name] = olist[i];
      break;
    case FUNC:
      func_p[olist[i].name] = olist[i];
      break;
    default:
      break;
    }
    allop_p[olist[i].code] = olist[i];
  }
  spop_p[olist[i].name] = olist[i];
  allop_p[olist[i].code] = olist[i];
}

//# Operators

//# Member functions
void FuncExprData::print(ostream &os, const
			 map<String, FuncExprData::ExprOperator> &m) const {
  for (map<String, FuncExprData::ExprOperator>::const_iterator 
	 pos = m.begin(); pos != m.end(); pos++) print(os, pos->second);
}

void FuncExprData::print(ostream &os, const 
			 FuncExprData::ExprOperator &pos) const {
  os << setfill('0') << setw(2) << pos.code << ": " <<
    pos.name << setfill(' ') << setw(9-pos.name.length()) << ":" <<
    pos.category << ":" <<
    setfill('0') << setw(2) << pos.priority << ":" <<
    pos.narg << ":" <<
    setfill('0') << setw(2) << pos.nresult << ":" <<
    pos.info << ":" << endl;
}

//# Global functions
ostream &operator<<(ostream &os, const FuncExprData &ed) {
  os << "Unary operators with 2 characters:" << endl;
  ed.print(os, ed.unary2());
  os << "Unary operators with 1 character:" << endl;
  ed.print(os, ed.unary1());
  os << "Binary operators with 2 characters:" << endl;
  ed.print(os, ed.binary2());
  os << "Binary operators with 1 character:" << endl;
  ed.print(os, ed.binary1());
  os << "Special operations:" << endl;
  ed.print(os, ed.special());
  os << "Functions:" << endl;
  ed.print(os, ed.function());
  return os;
}

} //# NAMESPACE CASACORE - END

