//# FuncExprData.cc: Data and enumerations for functional expressions
//# Copyright (C) 2001
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
#include <trial/Functionals/FuncExprData.h>
#include <aips/iostream.h>
#include <aips/iomanip.h>

//# Constructors
FuncExprData::FuncExprData() :
  una2_p(), una1_p(), bin2_p(), bin1_p(),
  spop_p(), func_p() {
  static const ExprOperator olist[] = {
    {UNAMIN, 	"-",		UNA1,	48, 1, 0},
    {UNAPLUS, 	"+",	   	UNA1,	48, 1, 0},
    {NON, 	"!",		UNA1,	28, 1, 0},
    {POW, 	"**",		BIN2,	44, 2, 1},
    {GTE, 	">=",		BIN2,	32, 2, 1},
    {LTE, 	"<=",		BIN2,	32, 2, 1},
    {EQ, 	"==",		BIN2,	32, 2, 1},
    {NEQ, 	"!=",		BIN2,	32, 2, 1},
    {OR, 	"||",		BIN2,	20, 2, 1},
    {AND, 	"&&",		BIN2,	20, 2, 1},
    {ADD, 	"+",		BIN1,	36, 2, 1},
    {SUB, 	"-",		BIN1,	36, 2, 1},
    {MUL, 	"*",		BIN1,	40, 2, 1},
    {DIV, 	"/",		BIN1,	40, 2, 1},
    {POW, 	"^",		BIN1,	44, 2, 1},
    {GT, 	">",		BIN1,	32, 2, 1},
    {LT, 	"<",		BIN1,	32, 2, 1},
    {CONST, 	"const",	SPEC,	00, 0,-1},
    {LBRACE, 	"{",		SPEC,	60, 0, 0},
    {RBRACE, 	"}",		SPEC,	00, 0, 0},
    {LPAREN, 	"(",		SPEC,	60, 0, 0},
    {RPAREN, 	")",		SPEC,	00, 0, 0},
    {LBR, 	"[",		SPEC,	60, 0, 0},
    {RBR, 	"]",		SPEC,	00, 0, 0},
    {COMMA, 	",",		SPEC,	00, 0, 0},
    {FINISH, 	"finish",	SPEC,	00, 0, 0},
    {SIN, 	"sin",		FUNC,	60, 1, 1},
    {COS, 	"cos",		FUNC,	60, 1, 1},
    {ATAN, 	"atan",		FUNC,	60, 1, 1},
    {ASIN, 	"asin",		FUNC,	60, 1, 1},
    {ACOS, 	"acos",		FUNC,	60, 1, 1},
    {EXP, 	"exp",		FUNC,	60, 1, 1},
    {EXP2, 	"exp2",		FUNC,	60, 1, 1},
    {EXP10, 	"exp10",	FUNC,	60, 1, 1},
    {LOG, 	"ln",		FUNC,	60, 1, 1},
    {LOG2, 	"log2",		FUNC,	60, 1, 1},
    {LOG10, 	"log",		FUNC,	60, 1, 1},
    {PI, 	"pi",		FUNC,	60, 0, 1},
    {EE, 	"ee",		FUNC,	60, 0, 1},
    {ABS, 	"abs",		FUNC,	60, 1, 1},
    {FLOOR, 	"floor",	FUNC,	60, 1, 1},
    {CEIL, 	"ceil",		FUNC,	60, 1, 1},
    {ROUND, 	"round",	FUNC,	60, 1, 1},
    {INT, 	"int",		FUNC,	60, 1, 1},
    {FRACT, 	"fract",	FUNC,	60, 1, 1},
    {SQRT, 	"sqrt",		FUNC,	60, 1, 1},
    {COMPLEX, 	"complex",	FUNC,	60, 1, 1},
    {REAL, 	"real",		FUNC,	60, 1, 1},
    {IMAG, 	"imag",		FUNC,	60, 1, 1},
    {AMPL, 	"ampl",		FUNC,	60, 1, 1},
    {PHASE, 	"phase",	FUNC,	60, 1, 1},

    // End of list
    {NOP,	"NOP",	SPEC,	00, 0, 0}
  };
  for (uInt i=0; olist[i].code != NOP; ++i) {
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
      spop_p[olist[i].code] = olist[i];
      break;
    case FUNC:
      func_p[olist[i].name] = olist[i];
      break;
    default:
      break;
    }
  };
}

//# Global functions
ostream &operator<<(ostream &os, FuncExprData &ed) {
  os << "Unary operators with 2 characters:" << endl;
  for (map<String, FuncExprData::ExprOperator>::iterator 
	 pos=ed.unary2().begin(); pos != ed.unary2().end(); pos++) {
    os << setfill('0') << setw(2) << pos->second.code << ": " <<
      pos->second.name << setfill(' ') << setw(9-pos->second.name.length()) <<
      ":" <<
      pos->second.category << ":" <<
      setfill('0') << setw(2) << pos->second.priority << ":" <<
      pos->second.narg << ":" <<
      setfill('0') << setw(2) << pos->second.nresult << ":" << endl;
  };
  os << "Unary operators with 1 character:" << endl;
  for (map<String, FuncExprData::ExprOperator>::iterator 
	 pos=ed.unary1().begin(); pos != ed.unary1().end(); pos++) {
    os << setfill('0') << setw(2) << pos->second.code << ": " <<
      pos->second.name << setfill(' ') << setw(9-pos->second.name.length()) <<
      ":" <<
      pos->second.category << ":" <<
      setfill('0') << setw(2) << pos->second.priority << ":" <<
      pos->second.narg << ":" <<
      setfill('0') << setw(2) << pos->second.nresult << ":" << endl;
  };
  os << "Binary operators with 2 characters:" << endl;
  for (map<String, FuncExprData::ExprOperator>::iterator 
	 pos=ed.binary2().begin(); pos != ed.binary2().end(); pos++) {
    os << setfill('0') << setw(2) << pos->second.code << ": " <<
      pos->second.name << setfill(' ') << setw(9-pos->second.name.length()) <<
      ":" <<
      pos->second.category << ":" <<
      setfill('0') << setw(2) << pos->second.priority << ":" <<
      pos->second.narg << ":" <<
      setfill('0') << setw(2) << pos->second.nresult << ":" << endl;
  };
  os << "Binary operators with 1 character:" << endl;
  for (map<String, FuncExprData::ExprOperator>::iterator 
	 pos=ed.binary1().begin(); pos != ed.binary1().end(); pos++) {
    os << setfill('0') << setw(2) << pos->second.code << ": " <<
      pos->second.name << setfill(' ') << setw(9-pos->second.name.length()) <<
      ":" <<
      pos->second.category << ":" <<
      setfill('0') << setw(2) << pos->second.priority << ":" <<
      pos->second.narg << ":" <<
      setfill('0') << setw(2) << pos->second.nresult << ":" << endl;
  };
  os << "Special operations:" << endl;
  for (map<FuncExprData::opTypes, FuncExprData::ExprOperator>::iterator 
	 pos=ed.special().begin(); pos != ed.special().end(); pos++) {
    os << setfill('0') << setw(2) << pos->second.code << ": " <<
      pos->second.name << setfill(' ') << setw(9-pos->second.name.length()) <<
      ":" <<
      pos->second.category << ":" <<
      setfill('0') << setw(2) << pos->second.priority << ":" <<
      pos->second.narg << ":" <<
      setfill('0') << setw(2) << pos->second.nresult << ":" << endl;
  };
  os << "Functions:" << endl;
  for (map<String, FuncExprData::ExprOperator>::iterator 
	 pos=ed.function().begin(); pos != ed.function().end(); pos++) {
    os << setfill('0') << setw(2) << pos->second.code << ": " <<
      pos->second.name << setfill(' ') << setw(9-pos->second.name.length()) <<
      ":" <<
      pos->second.category << ":" <<
      setfill('0') << setw(2) << pos->second.priority << ":" <<
      pos->second.narg << ":" <<
      setfill('0') << setw(2) << pos->second.nresult << ":" << endl;
  };
  return os;
}

//# Templates (for test purposes)
///template void _M_erase(_Rb_tree_node<pair<String const,
///		       FuncExprData::ExprOperator> > *);
template class  map<String, FuncExprData::ExprOperator>;
template class map<FuncExprData::opTypes, FuncExprData::ExprOperator>;
template class
_Rb_tree<FuncExprData::opTypes,
			 pair<FuncExprData::opTypes const,
  FuncExprData::ExprOperator>,
  _Select1st<pair<FuncExprData::opTypes const,
  FuncExprData::ExprOperator> >,
  less<FuncExprData::opTypes>,
  allocator<FuncExprData::ExprOperator> >;
template class
_Rb_tree<String,
  pair<String const,
  FuncExprData::ExprOperator>,
  _Select1st<pair<String const,
  FuncExprData::ExprOperator> >, 
  less<String>,
  allocator<FuncExprData::ExprOperator> >;


