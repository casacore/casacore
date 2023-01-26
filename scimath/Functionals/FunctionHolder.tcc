//# FunctionHolder.cc: A holder for Functions to enable record conversions
//# Copyright (C) 2002,2003,2004
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

#ifndef SCIMATH_FUNCTIONHOLDER_TCC
#define SCIMATH_FUNCTIONHOLDER_TCC

//# Includes
#include <casacore/scimath/Functionals/FunctionHolder.h>
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/casa/Containers/RecordFieldId.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/scimath/Functionals/Gaussian1D.h>
#include <casacore/scimath/Functionals/Gaussian2D.h>
#include <casacore/scimath/Functionals/Gaussian3D.h>
#include <casacore/scimath/Functionals/GaussianND.h>
#include <casacore/scimath/Functionals/Polynomial.h>
#include <casacore/scimath/Functionals/EvenPolynomial.h>
#include <casacore/scimath/Functionals/OddPolynomial.h>
#include <casacore/scimath/Functionals/HyperPlane.h>
#include <casacore/scimath/Functionals/Sinusoid1D.h>
#include <casacore/scimath/Functionals/Chebyshev.h>
#include <casacore/scimath/Functionals/SimButterworthBandpass.h>
#include <casacore/scimath/Functionals/CombiFunction.h>
#include <casacore/scimath/Functionals/CompoundFunction.h>
#include <casacore/scimath/Functionals/CompiledFunction.h>
#include <casacore/casa/Utilities/MUString.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>
#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
template <class T>
FunctionHolder<T>::FunctionHolder() 
  : hold_p(), mode_p(), nam_p(N_Types), isFilled(false) {}

template <class T>
FunctionHolder<T>::FunctionHolder(const Function<T> &in) 
  : hold_p(in.clone()), mode_p(), nam_p(N_Types), isFilled(false) 
{
    if (in.hasMode()) {
	mode_p.set(new Record(RecordInterface::Variable));
	in.getMode( *(mode_p.ptr()) );
    }
}

template <class T>
FunctionHolder<T>::FunctionHolder(const FunctionHolder<T> &other)
  : hold_p(), mode_p(), nam_p(N_Types), isFilled(false) 
{
  if (other.hold_p.ptr()) hold_p.set(other.hold_p.ptr()->clone());
  if (other.mode_p.ptr()) mode_p.set(other.mode_p.ptr()->clone());
}

//# Destructor
template <class T>
FunctionHolder<T>::~FunctionHolder() {}

//# Operators
template <class T>
FunctionHolder<T> &FunctionHolder<T>::
operator=(const FunctionHolder<T> &other) {
  if (this != &other) {
    if (other.hold_p.ptr()) {
      hold_p.set(other.hold_p.ptr()->clone());
    } else {
      hold_p.clear();
    }

    if (other.mode_p.ptr()) {
      mode_p.set(other.mode_p.ptr()->clone());
    } else {
      mode_p.clear();
    }
  }
  return *this;
}

//# Member Functions
template <class T>
bool FunctionHolder<T>::isEmpty() const {
  return (!hold_p.ptr());
}

template <class T>
const Vector<String> &FunctionHolder<T>::names() const {
  init();
  return nam_p;
}

template <class T>
const Function<T> &FunctionHolder<T>::asFunction() const {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty FunctionHolder argument for asFunction"));
  }
  return *hold_p.ptr();
}

template <class T>
bool FunctionHolder<T>::addFunction(const Function<T> &fnc) {
  if (nf_p == COMBINE) {
    dynamic_cast<CombiFunction<T> &>(*hold_p.ptr()).addFunction(fnc);
  } else if (nf_p == COMPOUND) {
    dynamic_cast<CompoundFunction<T> &>(*hold_p.ptr()).addFunction(fnc);
  } else return false;
  return true;
} 

template <class T>
typename FunctionHolder<T>::Types FunctionHolder<T>::type() const {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty FunctionHolder argument for type()"));
  }
  return nf_p;
}

template <class T>
void FunctionHolder<T>::init() const {
  static FuncStat fnc[N_Types] = {
    { String("gaussian1d"), GAUSSIAN1D,
	false},
      { String("gaussian2d"), GAUSSIAN2D,
	  false},
	{ String("gaussian3d"), GAUSSIAN3D,
	    false},
	  { String("gaussianNd"), GAUSSIANND,
	      true},
	    { String("hyperplane"), HYPERPLANE,
		true},
	      { String("polynomial"), POLYNOMIAL,
		  true},
		{ String("evenpolynomial"), EVENPOLYNOMIAL,
		    true},
		  { String("oddpolynomial"), ODDPOLYNOMIAL,
		      true},
		    { String("sinusoid1d"), SINUSOID1D,
			false},
		      { String("chebyshev"), CHEBYSHEV,
			  true},
		      { String("butterworth"), BUTTERWORTH,
			  true},
			{ String("combine"), COMBINE,
			    false},
			  { String("compound"), COMPOUND,
			      false},
			    { String("compiled"), COMPILED,
				false}
  };
  if (!isFilled) {
    isFilled = true;
    for (uint32_t i=0; i<N_Types; ++i) {
      nam_p[i] = fnc[i].nam;
      if (i != static_cast<uint32_t>(fnc[i].tp)) {
	throw(AipsError("Lists in FunctionHolder incorrect order"));
      }
    }
  }
}

template <class T>
bool FunctionHolder<T>::fromRecord(String &error, const RecordInterface &in) {
  hold_p.clear();
  Function<T> *fn(0);
  if (!getRecord(error, fn, in)) {
    delete fn; fn = 0;
    return false;
  }
  hold_p.set(fn);
  return true;
}

template <class T>
template <class U>
bool FunctionHolder<T>::getRecord(String &error, Function<U> *&fn,
				  const RecordInterface &in) {
  try {
    if (!getType(error, fn, in)) return false;
    if ((nf_p == COMBINE || nf_p == COMPOUND) &&
	in.isDefined(String("nfunc")) &&
	in.isDefined(String("funcs")) &&
	in.type(in.idToNumber(RecordFieldId("funcs"))) == TpRecord) {
      int32_t nfunc;
      in.get(RecordFieldId("nfunc"), nfunc);
      Record fnsrec = in.asRecord(RecordFieldId("funcs"));
      for (int32_t i=0; i<nfunc; ++i) {
	Record fnr = fnsrec.asRecord(i);
	FunctionHolder<T> fnch;
	Function<U> *fnc(0);
	if (!fnch.getRecord(error, fnc, fnr)) {
	  delete fnc; fnc = 0;
	  return false;
	}
	if (nf_p == COMBINE) {
	  dynamic_cast<CombiFunction<U> *>(fn)->
	    addFunction(*fnc);
	} else {
	  dynamic_cast<CompoundFunction<U> *>(fn)->
	    addFunction(*fnc);
	}
	delete fnc; fnc = 0;
      }
    }
    if (in.isDefined(String("params"))) {
      Vector<T> params;
      in.get(RecordFieldId("params"), params);
      setParameters(fn, params);
    }
    if (in.isDefined(String("masks"))) {
      Vector<bool> masks;
      in.get(RecordFieldId("masks"), masks);
      for (uint32_t i=0; i<fn->nparameters(); ++i) fn->mask(i) = masks[i];
    }
    return true;
  } catch (const AipsError& x) {
    error = x.what();
  }
  error = String("Illegal Function record in "
                 "FunctionHolder<T>::fromRecord\n" + error);
  return false;
}

template <class T>
bool FunctionHolder<T>::fromString(String &error,
				   const String &in) {
  order_p = -1;
  text_p = "";
  int32_t nf;
  init();
  nf = MUString::minimaxNC(in, nam_p);
  nf_p = static_cast<Types>(nf);
  Function<T> *fn(0);
  if (getType(error, fn)) {
    hold_p.set(fn);
    return true;
  }
  delete fn; fn = 0;
  return false;
}

template <class T>
bool FunctionHolder<T>::toRecord(String &error, RecordInterface &out) const {
  if (hold_p.ptr() && putType(error, out)) {
    out.define(RecordFieldId("ndim"),
	       static_cast<int32_t>(hold_p.ptr()->ndim()));
    out.define(RecordFieldId("npar"),
	       static_cast<int32_t>(hold_p.ptr()->nparameters()));
    out.define(RecordFieldId("params"),
    	       hold_p.ptr()->parameters().getParameters());
    out.define(RecordFieldId("masks"),
    	       hold_p.ptr()->parameters().getParamMasks());

    Record mode;
    hold_p.ptr()->getMode(mode);
    if (mode.nfields() > 0) out.defineRecord(RecordFieldId("mode"), mode);

    if (nf_p == COMBINE || nf_p == COMPOUND) {
      int32_t x(0);
      if (nf_p == COMBINE) {
	x = dynamic_cast<const CombiFunction<T> *>
	  (hold_p.ptr())->nFunctions();
      } else {
	x = dynamic_cast<const CompoundFunction<T> *>
	  (hold_p.ptr())->nFunctions();
      }
      out.define("nfunc", x);
      Record func;
      for (int32_t i=0; i<x; ++i) {
	Record fnc;
	if (nf_p == COMBINE) {
	  FunctionHolder<T> fn(dynamic_cast<const CombiFunction<T> *>
			    (hold_p.ptr())->function(i));
	  if (!fn.toRecord(error, fnc)) return false;
	} else {
	  FunctionHolder<T> fn(dynamic_cast<const CompoundFunction<T> *>
			    (hold_p.ptr())->function(i));
	  if (!fn.toRecord(error, fnc)) return false;
	}
	ostringstream oss;
	oss << "__*" << i;
	func.defineRecord(String(oss), fnc);
      }
      out.defineRecord("funcs", func);
    }
    return true;
  }
  error += String("No Function specified in FunctionHolder::toRecord\n");
  return false;
}

template <class T>
const String &FunctionHolder<T>::ident() const {
  static String myid = "fnc";
  return myid;
}

template <class T>
bool FunctionHolder<T>::putType(String &error, RecordInterface &out) const {
  order_p = -1;
  text_p = "";
  if (dynamic_cast<const Gaussian1D<T> *>(hold_p.ptr())) {
    nf_p = GAUSSIAN1D;
  } else if (dynamic_cast<const Gaussian2D<T> *>(hold_p.ptr())) {
    nf_p = GAUSSIAN2D;
  } else if (dynamic_cast<const Gaussian3D<T> *>(hold_p.ptr())) {
    nf_p = GAUSSIAN3D;
  } else if (dynamic_cast<const GaussianND<T> *>(hold_p.ptr())) {
    nf_p = GAUSSIANND;
    order_p = int32_t(-3.0+sqrt(1.0+8.0*hold_p.ptr()->nparameters())+0.1)/2;
  } else if (dynamic_cast<const HyperPlane<T> *>(hold_p.ptr())) {
    nf_p = HYPERPLANE;
    order_p = hold_p.ptr()->nparameters();
  } else if (dynamic_cast<const Polynomial<T> *>(hold_p.ptr())) {
    nf_p = POLYNOMIAL;
    order_p = hold_p.ptr()->nparameters()-1;
  } else if (dynamic_cast<const EvenPolynomial<T> *>(hold_p.ptr())) {
    nf_p = EVENPOLYNOMIAL;
    order_p = 2*hold_p.ptr()->nparameters()-1;
  } else if (dynamic_cast<const OddPolynomial<T> *>(hold_p.ptr())) {
    nf_p = ODDPOLYNOMIAL;
    order_p = 2*hold_p.ptr()->nparameters()-1;
  } else if (dynamic_cast<const Sinusoid1D<T> *>(hold_p.ptr())) {
    nf_p = SINUSOID1D;
  } else if (dynamic_cast<const Chebyshev<T> *>(hold_p.ptr())) {
    nf_p = CHEBYSHEV;
    order_p = hold_p.ptr()->nparameters()-1;
  } else if (dynamic_cast<const SimButterworthBandpass<T> *>(hold_p.ptr())) {
    nf_p = BUTTERWORTH;
  } else if (dynamic_cast<const CombiFunction<T> *>(hold_p.ptr())) {
    nf_p = COMBINE;
  } else if (dynamic_cast<const CompoundFunction<T> *>(hold_p.ptr())) {
    nf_p = COMPOUND;
  } else if (dynamic_cast<const CompiledFunction<T> *>(hold_p.ptr())) {
    nf_p = COMPILED;
    text_p = dynamic_cast<const CompiledFunction<T> *>(hold_p.ptr())->
      getText();
  } else {
    error += String("Unknown functional in FunctionHolder::putType()\n");
    return false;
  }
  out.define(RecordFieldId("type"), nf_p);
  out.define(RecordFieldId("order"), order_p);
  if (nf_p == COMPILED) out.define(RecordFieldId("progtext"), text_p);
  return true;
}

template <class T>
template <class U>
bool FunctionHolder<T>::getType(String &error, Function<U> *&fn,
				const RecordInterface &in) {
  in.get(RecordFieldId("order"), order_p);
  if (in.isDefined(String("progtext")) &&
      in.type(in.idToNumber(RecordFieldId("progtext"))) == TpString) {
      in.get(RecordFieldId("progtext"), text_p);
  }

  // mode can hold function-specific configuration data
  if (in.isDefined(String("mode")) &&
      in.type(in.idToNumber(RecordFieldId("mode"))) == TpRecord) 
  {
      mode_p.set(new Record(in.asRecord(RecordFieldId("mode"))));
  }

  int32_t nf;
  if (in.type(in.idToNumber(RecordFieldId("type"))) == TpString) {
    String tp;
    in.get(RecordFieldId("type"), tp);
    init();
    nf = MUString::minimaxNC(tp, nam_p);
  } else {
    in.get(RecordFieldId("type"), nf);
  }
  nf_p = static_cast<Types>(nf);
  return getType(error, fn);
}

template <class T>
template <class U>
bool FunctionHolder<T>::getType(String &error, Function<U> *&fn) {
  if (nf_p<0 || nf_p >= N_Types) {
    error += "Unknown type in FunctionHolder::getType()\n";
    return false;
  }
  ///  hold_p.clear();
  switch (nf_p) {
    
  case GAUSSIAN1D:
    fn = (new Gaussian1D<U>);
    break;
    
  case GAUSSIAN2D:
    fn = (new Gaussian2D<U>);
    break;
        
  case GAUSSIAN3D:
    fn = (new Gaussian3D<U>);
    break;
    
  case GAUSSIANND:
    if (order_p >= 0) {
      fn = (new GaussianND<U>(order_p));
    } else fn = (new GaussianND<U>);
    break;
     
  case HYPERPLANE:
    if (order_p >= 0) {
      fn = (new HyperPlane<U>(order_p));
    } else fn = (new HyperPlane<U>);
    break;
   
  case POLYNOMIAL:
    if (order_p >= 0) {
      fn = (new Polynomial<U>(order_p));
    } else fn = (new Polynomial<U>);
    break;
    
  case EVENPOLYNOMIAL:
    if (order_p >= 0) {
      fn = (new EvenPolynomial<U>(order_p));
    } else fn = (new EvenPolynomial<U>);
    break;
   
  case ODDPOLYNOMIAL:
    if (order_p >= 0) {
      fn = (new OddPolynomial<U>(order_p));
    } else fn = (new OddPolynomial<U>);
    break;
    
  case SINUSOID1D:
    fn = (new Sinusoid1D<U>);
    break;
    
  case CHEBYSHEV:
    if (mode_p.ptr()) 
      fn = (new Chebyshev<U>(order_p, *(mode_p.ptr()) ));
    else 
      fn = (new Chebyshev<U>(order_p));
    break;
    
  case BUTTERWORTH:
    if (mode_p.ptr()) 
      fn = (new SimButterworthBandpass<U>(*(mode_p.ptr()) ));
    else 
      fn = (new SimButterworthBandpass<U>(0, 0));
    break;
    
  case COMBINE:
    fn = (new CombiFunction<U>);
    break;
    
  case COMPOUND:
    fn = (new CompoundFunction<U>);
    break;
    
  case COMPILED:
    fn = (new CompiledFunction<U>);
    if (!dynamic_cast<CompiledFunction<U> *>(fn)->setFunction(text_p)) {
      error += String("Illegal compiled expression:\n") +
	dynamic_cast<CompiledFunction<U> *>(fn)->errorMessage() + "\n";
      return false;
    }
    break;
    
  default:
    error += "Unknown type in FunctionHolder::getType()\n";
    return false;
    break;
  }
  return true;
}

template <class T>
void FunctionHolder<T>::setParameters(Function<T> *&fn,
				      const Vector<T> &params) {
  for (uint32_t i=0; i<fn->nparameters(); ++i) (*fn)[i] = params[i];
}

template <class T>
void FunctionHolder<T>::setParameters(Function<AutoDiff<T> > *&fn,
				      const Vector<T> &params) {
  for (uint32_t i=0; i<fn->nparameters(); ++i) {
    (*fn)[i] = AutoDiff<T>(params[i], fn->nparameters(), i);
  }
}

} //# NAMESPACE CASACORE - END


#endif
