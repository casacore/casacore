//# CompiledFunction.cc:  Form a linear combination of Functions
//# Copyright (C) 2002,2004
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
//# $Id$

#ifndef SCIMATH_COMPILEDFUNCTION_TCC
#define SCIMATH_COMPILEDFUNCTION_TCC

//# Includes
#include <casacore/scimath/Functionals/CompiledFunction.h>
#include <casacore/scimath/Functionals/FuncExpression.h>
#include <casacore/scimath/Functionals/FunctionTraits.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/stdvector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors

//# Operators
template<class T>
T CompiledFunction<T>::eval(typename Function<T>::FunctionArg x) const {
  String error_p = "";
  T res(0);
  if (!this->functionPtr_p) {
    error_p = "No CompiledFunction specified";
    return res;
  }
  vector<T> exec_p;
  exec_p.resize(0);
  vector<Double>::const_iterator
    constp = this->functionPtr_p->getConst().begin();
  for (vector<FuncExprData::ExprOperator>::const_iterator
	 pos = this->functionPtr_p->getCode().begin();
       pos != this->functionPtr_p->getCode().end(); pos++) {
    T t(0);
    if (pos->narg == 2 ||
	(pos->code == FuncExprData::ATAN && pos->state.argcnt == 2)) {
      t = exec_p.back();
      exec_p.pop_back();
    }
    
    switch (pos->code) {
    case FuncExprData::UNAMIN:
      exec_p.back() = -exec_p.back();
    case FuncExprData::UNAPLUS:
      break;
  
    case FuncExprData::POW:
      exec_p.back() = pow(exec_p.back(), t);
      break;
    case FuncExprData::GTE:
      exec_p.back() = exec_p.back() >= t ? T(1) : T(0);
      break;
    case FuncExprData::LTE:
      exec_p.back() = exec_p.back() <= t ? T(1) : T(0);
      break;
    case FuncExprData::EQ:
      exec_p.back() = exec_p.back() == t ? T(1) : T(0);
      break;
    case FuncExprData::NEQ:
      exec_p.back() = exec_p.back() != t ? T(1) : T(0);
      break;
    case FuncExprData::OR:
      exec_p.back() = (exec_p.back() != T(0) || t != T(0)) ? T(1) : T(0);
      break;
    case FuncExprData::AND:
      exec_p.back() = (t*exec_p.back() != T(0)) ? T(1) : T(0);
      break;
    case FuncExprData::ADD:
      exec_p.back() += t;
      break;
    case FuncExprData::SUB:
      exec_p.back() -= t;
      break;
    case FuncExprData::MUL:
      exec_p.back() *= t;
      break;
    case FuncExprData::DIV:
      exec_p.back() /= t;
      break;
    case FuncExprData::CONDEX3:
      exec_p.back() = t;
      break;

    case FuncExprData::CONST:
      exec_p.push_back(T(constp[pos->info]));
      break;
    case FuncExprData::PARAM:
      exec_p.push_back(T(this->param_p[pos->info]));
      break;
    case FuncExprData::ARG:
      exec_p.push_back(T(x[pos->info]));
      break;
    case FuncExprData::TOIMAG:
      NumericTraits<T>::setValue(exec_p.back(),
				 NumericTraits<T>::getValue(exec_p.back(), 0),
				 1);
      NumericTraits<T>::setValue(exec_p.back(),
				 typename NumericTraits<T>::BaseType(0.0),
				 0);
      break;
   case FuncExprData::NOP:
      break;
    case FuncExprData::GOTO:
      pos += pos->info -
	(static_cast<uInt>(pos-this->functionPtr_p->getCode().begin())+1);
      break;
    case FuncExprData::GOTOF:
      if (exec_p.back() == T(0.0)) {
	pos += pos->info -
	  (static_cast<uInt>(pos-this->functionPtr_p->getCode().begin())+1);
      }
      break;
    case FuncExprData::GOTOT:
      if (exec_p.back() != T(0.0)) {
	pos += pos->info -
	  (static_cast<uInt>(pos-this->functionPtr_p->getCode().begin())+1);
      }
      break;

    case FuncExprData::SIN:
      exec_p.back() = sin(exec_p.back());
      break;
    case FuncExprData::COS:
      exec_p.back() = cos(exec_p.back());
      break;
    case FuncExprData::ATAN:
      if (pos->state.argcnt == 1) {
	exec_p.back() = atan(exec_p.back());
	break;
      }
    case FuncExprData::ATAN2:
      exec_p.back() = atan2(exec_p.back(), t);
      break;
    case FuncExprData::ASIN:
      exec_p.back() = asin(exec_p.back());
      break;
    case FuncExprData::ACOS:
      exec_p.back() = acos(exec_p.back());
      break;
    case FuncExprData::EXP:
      exec_p.back() = exp(exec_p.back());
      break;
    case FuncExprData::EXP2:
      exec_p.back() = exp(exec_p.back()*
			  static_cast<typename FunctionTraits<T>::BaseType>
			  (C::ln2));
      break;
    case FuncExprData::EXP10:
      exec_p.back() = exp(exec_p.back()*
			  static_cast<typename FunctionTraits<T>::BaseType>
			  (C::ln10));
      break;
    case FuncExprData::LOG:
      exec_p.back() = log(exec_p.back());
      break;
    case FuncExprData::LOG2:
      exec_p.back() = log(exec_p.back())/
	static_cast<typename FunctionTraits<T>::BaseType>(C::ln2);
      break;
    case FuncExprData::LOG10:
      exec_p.back() = log10(exec_p.back());
      break;
    case FuncExprData::ERF:
      exec_p.back() = erf(exec_p.back());
      break;
    case FuncExprData::ERFC:
      exec_p.back() = erfc(exec_p.back());
      break;
    case FuncExprData::PI: {
      if (pos->state.argcnt == 0) {
	exec_p.push_back(T(static_cast<typename FunctionTraits<T>::BaseType>
			   (C::pi)));
      } else {
	exec_p.back() *= static_cast<typename FunctionTraits<T>::BaseType>
	  (C::pi);
      }
      break; }
    case FuncExprData::EE: {
      if (pos->state.argcnt == 0) {
	exec_p.push_back(T(static_cast<typename FunctionTraits<T>::BaseType>
			   (C::e)));
      } else {
	exec_p.back() *= static_cast<typename FunctionTraits<T>::BaseType>
	  (C::e);
      }
      break; }
    case FuncExprData::ABS:
      exec_p.back() = abs(exec_p.back());
      break;
    case FuncExprData::FLOOR:
      exec_p.back() = floor(exec_p.back());
      break;
    case FuncExprData::CEIL:
      exec_p.back() = ceil(exec_p.back());
      break;
    case FuncExprData::ROUND:
      exec_p.back() = floor(exec_p.back()+T(0.5));
      break;
    case FuncExprData::INT:
      if (exec_p.back() < T(0)) exec_p.back() = floor(exec_p.back());
      else exec_p.back() = ceil(exec_p.back());
      break;
    case FuncExprData::FRACT:
      if (exec_p.back() < T(0)) exec_p.back() -= ceil(exec_p.back());
      else exec_p.back() -= floor(exec_p.back());
      break;
    case FuncExprData::SQRT:
      exec_p.back() = sqrt(exec_p.back());
      break;
    case FuncExprData::REAL:
      break;
    case FuncExprData::IMAG:
      exec_p.back() = T(0);
      break;
    case FuncExprData::AMPL:
      break;
    case FuncExprData::PHASE:
      exec_p.back() = T(0);
      break;
    default:
      error_p = String("Unknown execution code '") +
	pos->name + "': programming error";
      break;
    }
  }
  if (exec_p.size() != 1 && error_p.empty()) error_p = "No value returned";
  if (error_p.empty()) res = exec_p.back();

  return res;
}

} //# NAMESPACE CASACORE - END


#endif
