//# LSQFit2.cc: Basic class for least squares fitting: templated methods
//# Copyright (C) 1999,2000,2002,2004-2008
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

#ifndef SCIMATH_LSQFIT2_TCC
#define SCIMATH_LSQFIT2_TCC
//#
// This separation of definitions necessary to get pre-compilation of
// templates done without having duplicate entries problems for
// non-templated member functions
//
//# Includes
#include <casacore/scimath/Fitting/LSQFit.h>
#include <algorithm>

using namespace std;

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Static values

  //# Constructors

  //# Member functions

  template <class U>
  void LSQFit::copy(const Double *beg, const Double *end,
		    U &sol, LSQReal) {
    std::copy(beg, end, sol);
  }

  template <class U>
  void LSQFit::copy(const Double *beg, const Double *end,
		    U *sol, LSQReal) {
    std::copy(beg, end, sol);
  }

  template <class U>
  void LSQFit::copy(const Double *beg, const Double *end,
		    U &sol, LSQComplex) {
    typename std::iterator_traits<U>::pointer tsol = sol.pos();
    for (const Double *i=beg; i!=end; i+=2) {
      *tsol++ = typename U::value_type(*i, *(i+1));
    }
  }

  template <class U>
  void LSQFit::copy(const Double *beg, const Double *end,
		    U *sol, LSQComplex) {
    for (const Double *i=beg; i!=end; i+=2) {
      *sol++ = U(*i, *(i+1));
    }
  }

  template <class U>
  void LSQFit::uncopy(Double *beg, const Double *end,
		      U &sol, LSQReal) {
    std::copy(sol, sol + (end-beg), beg);
  }

  template <class U>
  void LSQFit::uncopy(Double *beg, const Double *end,
		      U *sol, LSQReal) {
    std::copy(sol, sol + (end-beg), beg);
  }

  template <class U>
  void LSQFit::uncopy(Double *beg, const Double *end,
		      U &sol, LSQComplex) {
    typename U::difference_type n=(end-beg)/2;
    U solend = sol+n;
    Double *tbeg = beg;
    for (U i=sol; i<solend; ++i)  {
      *tbeg++ = (*i).real();
      *tbeg++ = (*i).imag();
    }
  }

  template <class U>
  void LSQFit::uncopy(Double *beg, const Double *end,
		      U *sol, LSQComplex) {
    Double *tbeg = beg;
    for (U *i=sol; i<sol+((end-beg)/2); ++i)  {
      *tbeg++ = (*i).real();
      *tbeg++ = (*i).imag();
    }
  }


  template <class U>
  void LSQFit::solve(U *sol) {
    solveIt();
    copy(wsol_p, wsol_p+nun_p, sol, LSQReal());
  }

  template <class U>
  void LSQFit::solve(std::complex<U> *sol) {
    solveIt();
    copy(wsol_p, wsol_p+nun_p, sol, LSQComplex());
  }

  template <class U>
  void LSQFit::solve(U &sol) {
    solveIt();
    copy(wsol_p, wsol_p+nun_p, sol,
	 typename LSQTraits
	 <typename std::iterator_traits<U>::value_type>::num_type());
  }

  template <class U>
  Bool LSQFit::solveLoop(uInt &nRank,
			 U *sol, Bool doSVD) {
    Double fit;
    return solveLoop(fit, nRank, sol, doSVD);
  }

  template <class U>
  Bool LSQFit::solveLoop(uInt &nRank,
			 std::complex<U> *sol, Bool doSVD) {
    Double fit;
    return solveLoop(fit, nRank, sol, doSVD);
  }

  template <class U>
  Bool LSQFit::solveLoop(uInt &nRank,
			 U &sol, Bool doSVD) {
    Double fit;
    return solveLoop(fit, nRank, sol, doSVD);
  }

  template <class U>
  Bool LSQFit::solveLoop(Double &fit, uInt &nRank,
			 U *sol, Bool doSVD) {
    getWorkSOL();
    uncopy(wsol_p, wsol_p+nun_p, sol, LSQReal());
    if (solveItLoop(fit, nRank, doSVD)) {
      copy(wsol_p, wsol_p+nun_p, sol, LSQReal());
      return True;
    }
    return False;
  }

  template <class U>
  Bool LSQFit::solveLoop(Double &fit, uInt &nRank,
			 std::complex<U> *sol, Bool doSVD) {
    getWorkSOL();
    uncopy(wsol_p, wsol_p+nun_p, sol, LSQComplex());
    if (solveItLoop(fit, nRank, doSVD)) {
      copy(wsol_p, wsol_p+nun_p, sol, LSQComplex());
      return True;
    }
    return false;
  }

  template <class U>
  Bool LSQFit::solveLoop(Double &fit, uInt &nRank,
			 U &sol, Bool doSVD) {
    getWorkSOL();
    uncopy(wsol_p, wsol_p+nun_p, sol,
	   typename LSQTraits
	   <typename std::iterator_traits<U>::value_type>::num_type());
    if (solveItLoop(fit, nRank, doSVD)) {
      copy(wsol_p, wsol_p+nun_p, sol,
	   typename LSQTraits
	   <typename std::iterator_traits<U>::value_type>::num_type());
      return True;
    }
    return false;
  }

  // Note that the explicit conversions to Double are necessary for Solaris
  // (Linux is ok). Solaris does not zero the low-order part of a Double from
  // a Float, giving non-repeatable results.
  template <class U, class V>
  void LSQFit::makeNorm(const V &cEq, const U &weight, const U &obs,
			Bool doNorm, Bool doKnown) {
    if (doNorm) {
      Double *i2 = norm_p->row(0);
      for (V cEqp=cEq; cEqp!=cEq+nun_p; ++cEqp) {
	if (*cEqp != 0) {
	  for (V i1=cEqp; i1!=cEq+nun_p; ++i1) {
	    *i2++ += Double(*cEqp)*Double(weight)*Double(*i1);
	  }
	} else i2 += cEq-cEqp+nun_p;
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      Double obswt = obs*weight;
      V cEqp = cEq; 
      for (Double *kp = known_p; kp!=known_p+nun_p; kp++)
	*kp += Double(*cEqp++)*obswt; 
      error_p[NC] += 1;				//cnt equations
      error_p[SUMWEIGHT] += weight; 		//sum weight
      error_p[SUMLL] += obs*obswt;		//sum rms
    }
  }

  template <class U, class V>
  void LSQFit::makeNorm(const V &cEq, const U &weight, const U &obs,
			LSQFit::Real,
			Bool doNorm, Bool doKnown) {
    makeNorm(cEq, weight, obs, doNorm, doKnown);
  }

  template <class U, class V>
  void LSQFit::makeNorm(const V &cEq, const U &weight,
			const std::complex<U> &obs,
			Bool doNorm, Bool doKnown) {
    uInt ln(nun_p/2);
    if (doNorm) {
      std::complex<U> dci;
      for (uInt i=0; i<ln; i++) {
	Double *i2 = norm_p->row(2*i); 		//row pointer
	for (uInt i1=i; i1<ln; i1++) {
	  dci = cEq[i]*conj(cEq[i1])*weight;
	  i2[2*i1] += dci.real();  		//real equations
	  i2[2*i1+1] += dci.imag();       	//imag. equations
	}
	Double *i4 = norm_p->row(2*i+1);		//next line duplicate
	for (uInt i1=2*i+1; i1<nun_p; i1+=2) i4[i1] = i2[i1-1];
	for (uInt i1=2*i+2; i1<nun_p; i1+=2) i4[i1] = -i2[i1+1];
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      std::complex<U> dci;
      for (uInt i1=0; i1<ln; i1++) {
	dci = obs*conj(cEq[i1])*weight;
	known_p[2*i1] += dci.real();		//real part
	known_p[2*i1+1] += dci.imag();		//imag part
      }
      // errors
      error_p[NC] += 2;				//cnt equations
      error_p[SUMWEIGHT] += 2*weight; 		//sum weight
      error_p[SUMLL] += weight*norm(obs);		//sum rms
    }
  }

  template <class U, class V>
  void LSQFit::makeNorm(const V &cEq, const U &weight,
			const std::complex<U> &obs,
			LSQFit::Complex,
			Bool doNorm, Bool doKnown) {
    makeNorm(cEq, weight, obs, doNorm, doKnown);
  }

  template <class U, class V>
  void LSQFit::makeNorm(const V &cEq, const U &weight,
			const std::complex<U> &obs,
			LSQFit::Separable,
			Bool doNorm, Bool doKnown) {
    if (doNorm) {
      for (uInt i=0; i<nun_p; i+=2) {
	Double *i2 = norm_p->row(i);		//row pointer
	for (uInt i1=i; i1<nun_p-1; i1+=2) {
	  i2[i1]   += realMC(cEq[i], cEq[i1])*weight;
	  i2[i1+1] += imagMC(cEq[i], cEq[i1+1])*weight;
	}
      }
      for (uInt i=1; i<nun_p; i+=2) {
	Double *i2 = norm_p->row(i);		//row pointer
	for (uInt i1=i; i1<nun_p-1; i1+=2) {
	  i2[i1]   += realMC(cEq[i], cEq[i1])*weight;
	  i2[i1]   -= imagMC(cEq[i], cEq[i1+1])*weight;
	}
	uInt i1=nun_p-1;
	i2[i1]   += realMC(cEq[i], cEq[i1])*weight;
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      for (uInt i1=0; i1<nun_p; i1+=2) {
	known_p[i1]   += realMC(obs, cEq[i1])*weight;
	known_p[i1+1] += imagMC(obs, cEq[i1+1])*weight;
      }
      // errors
      error_p[NC] += 2;				//cnt equations
      error_p[SUMWEIGHT] += 2*weight; 		//sum weight
      error_p[SUMLL] += weight*norm(obs);		//sum rms
    }
  }

  template <class U, class V>
  void LSQFit::makeNorm(const V &cEq, const U &weight,
			const std::complex<U> &obs,
			LSQFit::AsReal,
			Bool doNorm, Bool doKnown) {
    uInt ln(nun_p/2);
    if (doNorm) {
      for (uInt i=0; i<ln; i++) {
	Double *i2 = norm_p->row(2*i);		//row pointer real
	Double *i2i= norm_p->row(2*i+1);		//row pointer imag
	for (uInt i1=i; i1<ln; i1++) {		//real part
	  i2[2*i1]    += cEq[i].real()*cEq[i1].real()*weight; //real part
	  i2i[2*i1+1] += cEq[i].imag()*cEq[i1].imag()*weight; //imag part
	}
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      for (uInt i1=0; i1<ln; i1++) {
	known_p[2*i1]   += cEq[i1].real()*obs.real()*weight; //real part
	known_p[2*i1+1] += cEq[i1].imag()*obs.imag()*weight; //imag part
      }
      // errors
      error_p[NC] += 2;				//cnt equations
      error_p[SUMWEIGHT] += 2*weight; 		//sum weight
      error_p[SUMLL] += weight*norm(obs);		//sum rms
    }
  }

  template <class U, class V>
  void LSQFit::makeNorm(const V &cEq, const U &weight,
			const std::complex<U> &obs,
			LSQFit::Conjugate,
			Bool doNorm, Bool doKnown) {
    if (doNorm) {
      for (uInt i=0; i<nun_p; i+=2) {
	Double *i2 = norm_p->row(i);		//row pointer
	for (uInt i1=i; i1<nun_p; i1+=2) {
	  i2[i1]   += realMC(cEq[i]+cEq[i+1], cEq[i1]+cEq[i1+1])*weight;
	  i2[i1+1] += imagMC(cEq[i]+cEq[i+1], cEq[i1]-cEq[i1+1])*weight;
	}
      }
      for (uInt i=1; i<nun_p; i+=2) {
	Double *i2 = norm_p->row(i);		//row pointer
	for (uInt i1=i; i1<nun_p-1; i1+=2) {
	  i2[i1]   += realMC(cEq[i-1]-cEq[i], cEq[i1-1]-cEq[i1])*weight;
	  i2[i1+1] -= imagMC(cEq[i-1]-cEq[i], cEq[i1+1]+cEq[i1+2])*weight;
	}
	uInt i1=nun_p-1;
	i2[i1] += realMC(cEq[i-1]-cEq[i], cEq[i1-1]-cEq[i1])*weight;
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      for (uInt i1=0; i1<nun_p; i1+=2) {
	known_p[i1]   += realMC(obs, cEq[i1]+cEq[i1+1])*weight;
	known_p[i1+1] += imagMC(obs, cEq[i1]-cEq[i1+1])*weight;
      }
      // errors
      error_p[NC] += 2;				//cnt equations
      error_p[SUMWEIGHT] += 2*weight; 		//sum weight
      error_p[SUMLL] += weight*norm(obs);		//sum rms
    }
  }

  // Note that the explicit conversions to Double are necessary for Solaris
  // (Linux is ok). Solaris does not zero the low-order part of a Double from
  // a Float, giving non-repeatable results.
  template <class U, class V, class W>
  void LSQFit::makeNorm(uInt nIndex, const W &cEqIndex,
			const V &cEq, const U &weight,
			const U &obs,
			Bool doNorm, Bool doKnown) {
    if (doNorm) {
      for (uInt i=0; i<nIndex; ++i) {
	uInt cEqIndex_i = cEqIndex[i];
	Double *i2 = norm_p->row(cEqIndex_i);	//row pointer
	Double eq(cEq[i]);
	eq *= weight;
	for (uInt i1=i; i1<nIndex; ++i1) {
	  uInt cEqIndex_i1 = cEqIndex[i1];
	  if (cEqIndex_i <= cEqIndex_i1)
	    i2[cEqIndex_i1] += eq*Double(cEq[i1]); //equations
	  else norm_p->row(cEqIndex_i1)[cEqIndex_i] += eq*Double(cEq[i1]);
	}
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      Double obswt = obs*weight;
      for (uInt i1=0; i1<nIndex; ++i1)
	known_p[cEqIndex[i1]] += Double(cEq[i1])*obswt;	//data vector
      error_p[NC] += 1;				//cnt equations
      error_p[SUMWEIGHT] += weight;		//sum weight
      error_p[SUMLL] += obs*obswt;		//sum rms
    }
  }

  template <class U, class V, class W>
  void LSQFit::makeNorm(uInt nIndex, const W &cEqIndex,
			const V &cEq, const V &cEq2,
			const U &weight,
			const U &obs, const U &obs2,
			Bool doNorm, Bool doKnown) {
    if (doNorm) {
      for (uInt i=0; i<nIndex; ++i) {
	uInt cEqIndex_i = cEqIndex[i];
	Double *i2 = norm_p->row(cEqIndex_i);	//row pointer
	Double eq(cEq[i]);
	Double eq2(cEq2[i]);
	eq *= weight;
	eq2 *= weight;
	for (uInt i1=i; i1<nIndex; ++i1) {
	  uInt cEqIndex_i1 = cEqIndex[i1];
	  if (cEqIndex_i <= cEqIndex_i1)
	    i2[cEqIndex_i1] += eq*Double(cEq[i1]) + eq2*Double(cEq2[i1]);
	  else norm_p->row(cEqIndex_i1)[cEqIndex_i] +=
		 eq*Double(cEq[i1]) + eq2*Double(cEq2[i1]);
	}
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      Double obswt = obs*weight;
      Double obswt2 = obs2*weight;
      for (uInt i1=0; i1<nIndex; ++i1) 
	known_p[cEqIndex[i1]] += Double(cEq[i1])*obswt +
	  Double(cEq2[i1])*obswt2;		//data vector
      error_p[NC] += 2;				//cnt equations
      error_p[SUMWEIGHT] += 2*weight;		//sum weight
      error_p[SUMLL] += obs*obswt + obs2*obswt2;	//sum rms
    }
  }

  template <class U, class V, class W>
  void LSQFit::makeNorm(uInt nIndex, const W &cEqIndex,
			const V &cEq, const U &weight, const U &obs,
			LSQFit::Real,
			Bool doNorm, Bool doKnown) {
    makeNorm(nIndex, cEqIndex, cEq, weight, obs, doNorm, doKnown);
  }

  template <class U, class V, class W>
  void LSQFit::makeNorm(uInt nIndex, const W &cEqIndex,
			const V &cEq, const U &weight,
			const std::complex<U> &obs,
			Bool doNorm, Bool doKnown) {
    if (doNorm) {
      std::complex<U> dci;
      for (uInt i=0; i<nIndex; ++i) {
	Double *i2 = norm_p->row(2*cEqIndex[i]);	//row pointer
	for (uInt i1=0; i1<nIndex; ++i1) {
	  if (cEqIndex[i]<=cEqIndex[i1]) {
	    dci = cEq[i]*conj(cEq[i1]);
	    i2[2*cEqIndex[i1]]   += dci.real()*weight; //real equations
	    i2[2*cEqIndex[i1]+1] += dci.imag()*weight; //imag. equations
	  }
	}
	Double *i4 = norm_p->row(2*cEqIndex[i]+1);	//next line row pointer
	for (uInt i1=2*cEqIndex[i]+1; i1<nun_p; i1+=2) {	//duplicate
	  i4[i1] = i2[i1-1];
	}
	for (uInt i1=2*cEqIndex[i]+2; i1<nun_p; i1+=2) {
	  i4[i1] = -i2[i1+1];
	}
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      std::complex<U> dci;
      for (uInt i1=0; i1<nIndex; i1++) {
	dci = obs*conj(cEq[i1]);
	known_p[2*cEqIndex[i1]]   += dci.real()*weight;	//real part
	known_p[2*cEqIndex[i1]+1] += dci.imag()*weight;	//imag part
      }
      // errors
      error_p[NC] += 2;				//cnt equations
      error_p[SUMWEIGHT] += 2*weight; 		//sum weight
      error_p[SUMLL] += weight*norm(obs);		//sum rms
    }
  }

  template <class U, class V, class W>
  void LSQFit::makeNorm(uInt nIndex, const W &cEqIndex,
			const V &cEq, const U &weight,
			const std::complex<U> &obs,
			LSQFit::Complex,
			Bool doNorm, Bool doKnown) {
    makeNorm(nIndex, cEqIndex, cEq, weight, obs, doNorm, doKnown);
  }

  template <class U, class V, class W>
  void LSQFit::makeNorm(uInt nIndex, const W &cEqIndex,
			const V &cEq, const U &weight,
			const std::complex<U> &obs,
			LSQFit::Separable,
			Bool doNorm, Bool doKnown) {
    if (doNorm) {
      for (uInt i=0; i<nIndex; ++i) {
	Double *i2 = norm_p->row(cEqIndex[i]);	//row pointer
	if (cEqIndex[i]%2 == 0) {
	  for (uInt i1=0; i1<nIndex; ++i1) {
	    if (cEqIndex[i]<=cEqIndex[i1] && cEqIndex[i1]+1 < nun_p) {
	      if (cEqIndex[i1]%2 == 0) {
		i2[cEqIndex[i1]] += realMC(cEq[i], cEq[i1])*weight;
	      } else {
		i2[cEqIndex[i1]] += imagMC(cEq[i], cEq[i1])*weight;
	      }
	    }
	  }
	} else {
	  for (uInt i1=0; i1<nIndex; ++i1) {
	    if (cEqIndex[i]<=cEqIndex[i1]) {
	      if (cEqIndex[i1] != nun_p-1) {
		if (cEqIndex[i1]%2 != 0) {
		  i2[cEqIndex[i1]] += realMC(cEq[i], cEq[i1])*weight;
		} else {
		  i2[cEqIndex[i1]] -= imagMC(cEq[i], cEq[i1])*weight;
		}
	      } else {
		i2[cEqIndex[i1]] += realMC(cEq[i], cEq[i1])*weight;
	      }
	    }
	  }
	}
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      for (uInt i1=0; i1<nIndex; ++i1) {
	if (cEqIndex[i1]%2 == 0) {
	  known_p[cEqIndex[i1]] += realMC(obs, cEq[i1])*weight;
	} else {
	  known_p[cEqIndex[i1]] += imagMC(obs, cEq[i1])*weight;
	}
      }
      // errors
      error_p[NC] += 2;				//cnt equations
      error_p[SUMWEIGHT] += 2*weight; 		//sum weight
      error_p[SUMLL] += weight*norm(obs);		//sum rms
    }
  }

  template <class U, class V, class W>
  void LSQFit::makeNorm(uInt nIndex, const W &cEqIndex,
			const V &cEq, const U &weight,
			const std::complex<U> &obs,
			LSQFit::AsReal,
			Bool doNorm, Bool doKnown) {
    if (doNorm) {
      for (uInt i=0; i<nIndex; ++i) {
	Double *i2 = norm_p->row(2*cEqIndex[i]);	//row pointer real
	Double *i2i= norm_p->row(2*cEqIndex[i]+1); //row pointer imag
	for (uInt i1=0; i1<nIndex; ++i1) {
	  if (cEqIndex[i]<=cEqIndex[i1]) {
	    i2[2*cEqIndex[i1]]    += cEq[i].real()*cEq[i1].real()*weight; //real
	    i2i[2*cEqIndex[i1]+1] += cEq[i].imag()*cEq[i1].imag()*weight; //imag
	  }
	}
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      for (uInt i1=0; i1<nIndex; ++i1) {
	known_p[2*cEqIndex[i1]]   += cEq[i1].real()*obs.real()*weight ; //real
	known_p[2*cEqIndex[i1]+1] += cEq[i1].imag()*obs.imag()*weight ; //imag
      }
      // errors
      error_p[NC] += 2;				//cnt equations
      error_p[SUMWEIGHT] += 2*weight; 		//sum weight
      error_p[SUMLL] += weight*norm(obs);		//sum rms
    }
  }

  template <class U, class V, class W>
  void LSQFit::makeNorm(uInt nIndex, const W &cEqIndex,
			const V &cEq, const U &weight,
			const std::complex<U> &obs,
			LSQFit::Conjugate,
			Bool doNorm, Bool doKnown) {
    if (doNorm) {
      std::complex<U> tmp(0);
      for (uInt i=0; i<nIndex; ++i) {
	if (cEqIndex[i] < nun_p) {
	  Double *i2 = norm_p->row(cEqIndex[i]);		//row pointer
	  for (uInt i1=0; i1<nIndex; ++i1) {
	    if (cEqIndex[i1] < nun_p) {
	      tmp = (cEq[i]*conj(cEq[i1]))*weight;
	      if (cEqIndex[i]%2 == 0) {
		Double *i2 = norm_p->row(cEqIndex[i]);	//row pointer
		if (cEqIndex[i1]%2 == 0) {
		  if (cEqIndex[i] <= cEqIndex[i1]) {
		    i2[cEqIndex[i1]]   += tmp.real();
		    i2[cEqIndex[i1]+1] += tmp.imag(); }
		  i2 = norm_p->row(cEqIndex[i]+1);
		  if (cEqIndex[i] <= cEqIndex[i1] && cEqIndex[i1]+2 < nun_p) {
		    i2[cEqIndex[i1]+1] += tmp.real(); }
		  if (cEqIndex[i] < cEqIndex[i1]) {
		    i2[cEqIndex[i1]]   -= tmp.imag(); }
		} else {
		  if (cEqIndex[i] < cEqIndex[i1]) {
		    i2[cEqIndex[i1]-1] += tmp.real();
		    i2[cEqIndex[i1]]   -= tmp.imag(); }
		  i2 = norm_p->row(cEqIndex[i]+1);
		  if (cEqIndex[i] < cEqIndex[i1] && cEqIndex[i1]+1 < nun_p) {
		    i2[cEqIndex[i1]]   -= tmp.real(); }
		  if (cEqIndex[i]+2 < cEqIndex[i1] && cEqIndex[i1] < nun_p) {
		    i2[cEqIndex[i1]-1] -= tmp.imag(); }
		}
	      } else {
		i2 = norm_p->row(cEqIndex[i]-1);
		if (cEqIndex[i1]%2 == 0) {
		  if (cEqIndex[i] <= cEqIndex[i1]+1) {
		    i2[cEqIndex[i1]]   += tmp.real();
		    i2[cEqIndex[i1]+1] += tmp.imag(); }
		  i2 = norm_p->row(cEqIndex[i]);
		  if (cEqIndex[i] <= cEqIndex[i1]+1 && cEqIndex[i1]+2 < nun_p) {
		    i2[cEqIndex[i1]+1] -= tmp.real(); }
		  if (cEqIndex[i] < cEqIndex[i1]) {
		    i2[cEqIndex[i1]]   += tmp.imag(); }
		} else {
		  if (cEqIndex[i] <= cEqIndex[i1]) {
		    i2[cEqIndex[i1]-1] += tmp.real();
		    i2[cEqIndex[i1]]   -= tmp.imag(); }
		  i2 = norm_p->row(cEqIndex[i]);
		  if (cEqIndex[i] <= cEqIndex[i1] && cEqIndex[i1]+1 < nun_p) {
		    i2[cEqIndex[i1]]   += tmp.real(); }
		  if (cEqIndex[i] < cEqIndex[i1] && cEqIndex[i1] < nun_p) {
		    i2[cEqIndex[i1]-1] += tmp.imag(); }
		}
	      }
	    }
	    if (cEqIndex[i1] == nun_p-1) {
	      i2 = norm_p->row(nun_p-1);
	      if (cEqIndex[i] == cEqIndex[i1]) {
		i2[cEqIndex[i1]] += tmp.real(); }
	      if (cEqIndex[i] == nun_p-2) {
		i2[cEqIndex[i1]] -= tmp.real(); }
	    }
	    if (cEqIndex[i1] == nun_p-2) {
	      i2 = norm_p->row(nun_p-1);
	      if (cEqIndex[i] == cEqIndex[i1]) {
		i2[cEqIndex[i1]+1] += tmp.real(); }
	      if (cEqIndex[i] == nun_p-1) {
		i2[cEqIndex[i1]+1] -= tmp.real(); }
	    }
	  }
	}
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      std::complex<U> tmp(0);
      for (uInt i1=0; i1<nIndex; ++i1) {
	tmp = obs*conj(cEq[i1])*weight;
	if (cEqIndex[i1]%2 == 0) {
	  known_p[cEqIndex[i1]]   += tmp.real();
	  known_p[cEqIndex[i1]+1] += tmp.imag();
	} else {
	  known_p[cEqIndex[i1]-1] += tmp.real();
	  known_p[cEqIndex[i1]]   -= tmp.imag();
	}
      }
      // errors
      error_p[NC] += 2;				//cnt equations
      error_p[SUMWEIGHT] += 2*weight; 		//sum weight
      error_p[SUMLL] += weight*norm(obs);		//sum rms
    }
  }
  //
  template <class U, class V>
  void LSQFit::makeNorm(const std::vector<std::pair<uInt, V> > &cEq,
			const U &weight, const U &obs,
			Bool doNorm, Bool doKnown) {
    if (doNorm) {
      for (typename std::vector<std::pair<uInt, V> >::const_iterator
	     i=cEq.begin();
	   i !=  cEq.end(); ++i) {
	Double *i2 = norm_p->row(i->first);	//row pointer
	Double eq(i->second);
	eq *= weight;
	for (typename std::vector<std::pair<uInt, V> >::const_iterator i1=i;
	     i1 != cEq.end(); ++i1) {
	  i2[i1->first] += eq*Double(i1->second); //equations
	}
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      Double obswt = obs*weight;
      for (typename std::vector<std::pair<uInt, V> >::const_iterator
	     i1=cEq.begin();
	   i1 != cEq.end(); ++i1) {
	known_p[i1->first] += Double(i1->second)*obswt;	//data vector
      }
      error_p[NC] += 1;				//cnt equations
      error_p[SUMWEIGHT] += weight;		//sum weight
      error_p[SUMLL] += obs*obswt;		//sum rms
    }
  }

  template <class U, class V>
  void LSQFit::makeNorm(const std::vector<std::pair<uInt, V> > &cEq,
			const U &weight, const U &obs,
			LSQFit::Real,
			Bool doNorm, Bool doKnown) {
    makeNorm(cEq, weight, obs, doNorm, doKnown);
  }

  template <class U, class V>
  void LSQFit::makeNorm(const std::vector<std::pair<uInt, V> > &cEq,
			const U &weight,
			const std::complex<U> &obs,
			Bool doNorm, Bool doKnown) {
    if (doNorm) {
      std::complex<U> dci;
      for (typename std::vector<std::pair<uInt, V> >::const_iterator
	     i=cEq.begin();
	   i !=  cEq.end(); ++i) {
	Double *i2 = norm_p->row(2*i->first);	//row pointer
	for (typename std::vector<std::pair<uInt, V> >::const_iterator
	       i1=cEq.begin();
	     i1 !=  cEq.end(); ++i1) {
	  if (i->first<=i1->first) {
	    dci = i->second*conj(i1->second);
	    i2[2*i1->first]   += dci.real()*weight; //real equations
	    i2[2*i1->first+1] += dci.imag()*weight; //imag. equations
	  }
	}
	Double *i4 = norm_p->row(2*i->first+1);	//next line row pointer
	for (uInt i1=2*i->first+1; i1<nun_p; i1+=2) {	//duplicate
	  i4[i1] = i2[i1-1];
	}
	for (uInt i1=2*i->first+2; i1<nun_p; i1+=2) {
	  i4[i1] = -i2[i1+1];
	}
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      std::complex<U> dci;
      for (typename std::vector<std::pair<uInt, V> >::const_iterator
	     i1=cEq.begin();
	   i1 !=  cEq.end(); ++i1) {
	dci = obs*conj(i1->second);
	known_p[2*i1->first]   += dci.real()*weight;	//real part
	known_p[2*i1->first+1] += dci.imag()*weight;	//imag part
      }
      // errors
      error_p[NC] += 2;				//cnt equations
      error_p[SUMWEIGHT] += 2*weight; 		//sum weight
      error_p[SUMLL] += weight*norm(obs);		//sum rms
    }
  }

  template <class U, class V>
  void LSQFit::makeNorm(const std::vector<std::pair<uInt, V> > &cEq,
			const U &weight,
			const std::complex<U> &obs,
			LSQFit::Complex,
			Bool doNorm, Bool doKnown) {
    makeNorm(cEq, weight, obs, doNorm, doKnown);
  }

  template <class U, class V>
  void LSQFit::makeNorm(const std::vector<std::pair<uInt, V> > &cEq,
			const U &weight,
			const std::complex<U> &obs,
			LSQFit::Separable,
			Bool doNorm, Bool doKnown) {
    if (doNorm) {
      for (typename std::vector<std::pair<uInt, V> >::const_iterator
	     i=cEq.begin();
	   i !=  cEq.end(); ++i) {
	Double *i2 = norm_p->row(i->first);	//row pointer
	if (i->first%2 == 0) {
	  for (typename std::vector<std::pair<uInt, V> >::const_iterator
		 i1=cEq.begin();
	       i1 !=  cEq.end(); ++i1) {
	    if (i->first<=i1->first && i1->first+1 < nun_p) {
	      if (i1->first%2 == 0) {
		i2[i1->first] += realMC(i->second, i1->second)*weight;
	      } else {
		i2[i1->first] += imagMC(i->second, i1->second)*weight;
	      }
	    }
	  }
	} else {
	  for (typename std::vector<std::pair<uInt, V> >::const_iterator
		 i1=cEq.begin();
	       i1 !=  cEq.end(); ++i1) {
	    if (i->first<=i1->first) {
	      if (i1->first != nun_p-1) {
		if (i1->first%2 != 0) {
		  i2[i1->first] += realMC(i->second, i1->second)*weight;
		} else {
		  i2[i1->first] -= imagMC(i->second, i1->second)*weight;
		}
	      } else {
		i2[i1->first] += realMC(i->second, i1->second)*weight;
	      }
	    }
	  }
	}
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      for (typename std::vector<std::pair<uInt, V> >::const_iterator
	     i1=cEq.begin();
	   i1 !=  cEq.end(); ++i1) {
	if (i1->first%2 == 0) {
	  known_p[i1->first] += realMC(obs, i1->second)*weight;
	} else {
	  known_p[i1->first] += imagMC(obs, i1->second)*weight;
	}
      }
      // errors
      error_p[NC] += 2;				//cnt equations
      error_p[SUMWEIGHT] += 2*weight; 		//sum weight
      error_p[SUMLL] += weight*norm(obs);		//sum rms
    }
  }

  template <class U, class V>
  void LSQFit::makeNorm(const std::vector<std::pair<uInt, V> > &cEq,
			const U &weight,
			const std::complex<U> &obs,
			LSQFit::AsReal,
			Bool doNorm, Bool doKnown) {
    if (doNorm) {
      for (typename std::vector<std::pair<uInt, V> >::const_iterator
	     i=cEq.begin();
	   i !=  cEq.end(); ++i) {
	Double *i2 = norm_p->row(2*i->first);	//row pointer real
	Double *i2i= norm_p->row(2*i->first+1); //row pointer imag
	for (typename std::vector<std::pair<uInt, V> >::const_iterator
	       i1=cEq.begin();
	     i1 !=  cEq.end(); ++i1) {
	  if (i->first<=i1->first) {
	    i2[2*i1->first]    +=
	      i->second.real()*i1->second.real()*weight; //real
	    i2i[2*i1->first+1] +=
	      i->second.imag()*i1->second.imag()*weight; //imag
	  }
	}
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      for (typename std::vector<std::pair<uInt, V> >::const_iterator
	     i1=cEq.begin();
	   i1 !=  cEq.end(); ++i1) {
	known_p[2*i1->first]   += i1->second.real()*obs.real()*weight ; //real
	known_p[2*i1->first+1] += i1->second.imag()*obs.imag()*weight ; //imag
      }
      // errors
      error_p[NC] += 2;				//cnt equations
      error_p[SUMWEIGHT] += 2*weight; 		//sum weight
      error_p[SUMLL] += weight*norm(obs);		//sum rms
    }
  }

  template <class U, class V>
  void LSQFit::makeNorm(const std::vector<std::pair<uInt, V> > &cEq,
			const U &weight,
			const std::complex<U> &obs,
			LSQFit::Conjugate,
			Bool doNorm, Bool doKnown) {
    if (doNorm) {
      std::complex<U> tmp(0);
      for (typename std::vector<std::pair<uInt, V> >::const_iterator
	     i=cEq.begin();
	   i !=  cEq.end(); ++i) {
	if (i->first < nun_p) {
	  Double *i2 = norm_p->row(i->first);		//row pointer
	  for (typename std::vector<std::pair<uInt, V> >::const_iterator
		 i1=cEq.begin();
	       i1 !=  cEq.end(); ++i1) {
	    if (i1->first < nun_p) {
	      tmp = (i->second*conj(i1->second))*weight;
	      if (i->first%2 == 0) {
		Double *i2 = norm_p->row(i->first);	//row pointer
		if (i1->first%2 == 0) {
		  if (i->first <= i1->first) {
		    i2[i1->first]   += tmp.real();
		    i2[i1->first+1] += tmp.imag(); }
		  i2 = norm_p->row(i->first+1);
		  if (i->first <= i1->first && i1->first+2 < nun_p) {
		    i2[i1->first+1] += tmp.real(); }
		  if (i->first < i1->first) {
		    i2[i1->first]   -= tmp.imag(); }
		} else {
		  if (i->first < i1->first) {
		    i2[i1->first-1] += tmp.real();
		    i2[i1->first]   -= tmp.imag(); }
		  i2 = norm_p->row(i->first+1);
		  if (i->first < i1->first && i1->first+1 < nun_p) {
		    i2[i1->first]   -= tmp.real(); }
		  if (i->first+2 < i1->first && i1->first < nun_p) {
		    i2[i1->first-1] -= tmp.imag(); }
		}
	      } else {
		i2 = norm_p->row(i->first-1);
		if (i1->first%2 == 0) {
		  if (i->first <= i1->first+1) {
		    i2[i1->first]   += tmp.real();
		    i2[i1->first+1] += tmp.imag(); }
		  i2 = norm_p->row(i->first);
		  if (i->first <= i1->first+1 && i1->first+2 < nun_p) {
		    i2[i1->first+1] -= tmp.real(); }
		  if (i->first < i1->first) {
		    i2[i1->first]   += tmp.imag(); }
		} else {
		  if (i->first <= i1->first) {
		    i2[i1->first-1] += tmp.real();
		    i2[i1->first]   -= tmp.imag(); }
		  i2 = norm_p->row(i->first);
		  if (i->first <= i1->first && i1->first+1 < nun_p) {
		    i2[i1->first]   += tmp.real(); }
		  if (i->first < i1->first && i1->first < nun_p) {
		    i2[i1->first-1] += tmp.imag(); }
		}
	      }
	    }
	    if (i1->first == nun_p-1) {
	      i2 = norm_p->row(nun_p-1);
	      if (i->first == i1->first) {
		i2[i1->first] += tmp.real(); }
	      if (i->first == nun_p-2) {
		i2[i1->first] -= tmp.real(); }
	    }
	    if (i1->first == nun_p-2) {
	      i2 = norm_p->row(nun_p-1);
	      if (i->first == i1->first) {
		i2[i1->first+1] += tmp.real(); }
	      if (i->first == nun_p-1) {
		i2[i1->first+1] -= tmp.real(); }
	    }
	  }
	}
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      std::complex<U> tmp(0);
      for (typename std::vector<std::pair<uInt, V> >::const_iterator
	     i1=cEq.begin();
	   i1 !=  cEq.end(); ++i1) {
	tmp = obs*conj(i1->second)*weight;
	if (i1->first%2 == 0) {
	  known_p[i1->first]   += tmp.real();
	  known_p[i1->first+1] += tmp.imag();
	} else {
	  known_p[i1->first-1] += tmp.real();
	  known_p[i1->first]   -= tmp.imag();
	}
      }
      // errors
      error_p[NC] += 2;				//cnt equations
      error_p[SUMWEIGHT] += 2*weight; 		//sum weight
      error_p[SUMLL] += weight*norm(obs);		//sum rms
    }
  }
  //
  template <class U, class V, class W>
  void LSQFit::makeNormSorted(uInt nIndex, const W &cEqIndex,
			      const V &cEq,
			      const U &weight, const U &obs,
			      Bool doNorm, Bool doKnown) {
    if (doNorm) {
      for (uInt i=0; i<nIndex; ++i) {
	Double *i2 = norm_p->row(cEqIndex[i]);	// row pointer
	Double eq(cEq[i]);
	eq *= weight;
	for (uInt i1=i; i1<nIndex; ++i1) {
	  i2[cEqIndex[i1]] += eq*Double(cEq[i1]); // equations
	}
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      Double obswt = obs*weight;
      for (uInt i1=0; i1<nIndex; ++i1) {
	known_p[cEqIndex[i1]] += Double(cEq[i1])*obswt; //data vector
      }
      error_p[NC] += 1;				 // cnt equations
      error_p[SUMWEIGHT] += weight;		 // sum weight
      error_p[SUMLL] += obs*obswt;		 // sum rms
    }
  }
  //
  template <class U, class V, class W>
  void LSQFit::makeNormSorted(uInt nIndex, const W &cEqIndex,
			      const V &cEq, const V &cEq2,
			      const U &weight,
			      const U &obs, const U &obs2,
			      Bool doNorm, Bool doKnown) {
    if (doNorm) {
      for (uInt i=0; i<nIndex; ++i) {
	Double *i2 = norm_p->row(cEqIndex[i]);	// row pointer
	Double eq(cEq[i]);
	Double eq2(cEq2[i]);
	eq *= weight;
	eq2 *= weight;
	for (uInt i1=i; i1<nIndex; ++i1) {
	  uInt cEqIndex_i1 = cEqIndex[i1];
	  i2[cEqIndex_i1] += eq*Double(cEq[i1]); // equations
	  i2[cEqIndex_i1] += eq2*Double(cEq2[i1]); // equations
	}
      }
      state_p &= ~TRIANGLE;
    }
    if (doKnown) {
      Double obswt = obs*weight;
      Double obswt2 = obs2*weight;
      for (uInt i1=0; i1<nIndex; ++i1) {
	known_p[cEqIndex[i1]] += Double(cEq[i1])*obswt +
	  Double(cEq2[i1])*obswt2;		 //data vector
      }
      error_p[NC] += 2;				 // cnt equations
      error_p[SUMWEIGHT] += weight * 2;		 // sum weight
      error_p[SUMLL] += obs*obswt + obs2*obswt2; // sum rms
    }
  }
  //
  template <class U>
  Bool LSQFit::getConstraint(uInt n, U *cEq) const {
    n += r_p;
    if (n<nun_p) {
      Double r0 = 1;	       		        //normalisation
      for (uInt i1=0; i1<r_p; ++i1) {
	cEq[piv_p[i1]] = nceq_p->row(i1)[n]; 	//copy constraint
	Double r1 = std::abs(cEq[piv_p[i1]]);
	if (r1 > 1e-8) r0 = std::min(r0, r1); 	//normalisation
      }
      for (uInt i1=r_p; i1<nun_p; ++i1) {		//final values
	cEq[piv_p[i1]] = 0;
      }
      cEq[piv_p[n]] = 1;			        //unit extend
      for (uInt i1=0; i1<=n; ++i1) cEq[piv_p[i1]] /= U(r0); //normalise
      return True;
    }
    return False;
  }

  template <class U>
  Bool LSQFit::getConstraint(uInt n, std::complex<U> *cEq) const {
    if (2*n+1 < nun_p) {
      U *eqp = new U[2*nun_p];
      if (getConstraint(2*n, eqp) && getConstraint(2*n+1, eqp+nun_p)) {
	for (uInt j=0; 2*j+1<nun_p; ++j) {
	  cEq[j] = std::complex<U>(eqp[2*j], -eqp[2*j+1]);
	}
	delete [] eqp;
	return True;
      }
      delete [] eqp;
    }
    return False;
  }

  template <class U>
  Bool LSQFit::getConstraint(uInt n, U &cEq) const {
    if (n<nun_p) {
      typename std::iterator_traits<U>::pointer eqp =
	new typename std::iterator_traits<U>::value_type[nun_p];
      if (getConstraint(n, eqp)) {
	typename U::difference_type l =
	  nun_p/LSQTraits<typename std::iterator_traits<U>::
	  value_type >::size;
	std::copy(eqp, eqp+l, cEq);
	delete [] eqp;
	return True;
      }
      delete [] eqp;
    }
    return False;
  }

  template <class U, class V>
  Bool LSQFit::setConstraint(uInt n, const V &cEq, const U &obs) {
    if (n>=ncon_p || nun_p==0) return False;
    std::copy(cEq, cEq+nun_p, constr_p+n*nun_p);
    known_p[nun_p+n] = obs;
    state_p &= ~TRIANGLE;
    return True;
  }

  template <class U, class V>
  Bool LSQFit::setConstraint(uInt n, const V &cEq,
			     const std::complex<U> &obs) {
    if (2*n+1>=ncon_p || nun_p==0) return False;
    for (uInt i=0; i<nun_p; i+=2) {
      constr_p[2*n*nun_p+i]       =  cEq[i/2].real();
      constr_p[2*n*nun_p+i+1]     = -cEq[i/2].imag();
      constr_p[(2*n+1)*nun_p+i]   = -cEq[i/2].imag();
      constr_p[(2*n+1)*nun_p+i+1] = -cEq[i/2].real();
    }
    known_p[nun_p+2*n]   = obs.real();
    known_p[nun_p+2*n+1] = obs.imag();
    state_p &= ~TRIANGLE;
    return True;
  }

  template <class U, class V, class W>
  Bool LSQFit::setConstraint(uInt n, uInt nIndex, const W &cEqIndex,
			     const V &cEq, const U &obs) {
    if (n>=ncon_p || nun_p==0) return False;
    for (uInt i=0; i<nIndex; ++i) constr_p[n*nun_p+cEqIndex[i]] = cEq[i];
    known_p[nun_p+n] = obs;
    state_p &= ~TRIANGLE;
    return True;
  }

  template <class U, class V, class W>
  Bool LSQFit::setConstraint(uInt n, uInt nIndex, const W &cEqIndex,
			     const V &cEq,
			     const std::complex<U> &obs) {
    if (2*n+1>=ncon_p || nun_p == 0) return False;
    for (uInt i=0; i<nIndex; ++i) {
      constr_p[2*n*nun_p+cEqIndex[2*i]]       =  cEq[i].real();
      constr_p[2*n*nun_p+cEqIndex[2*i+1]]     = -cEq[i].imag();
      constr_p[(2*n+1)*nun_p+cEqIndex[2*i]]   = -cEq[i].imag();
      constr_p[(2*n+1)*nun_p+cEqIndex[2*i+1]] = -cEq[i].real();
    }
    known_p[nun_p+2*n]   = obs.real();
    known_p[nun_p+2*n+1] = obs.imag();
    state_p &= ~TRIANGLE;
    return True;
  }

  template <class U, class V>
  Bool LSQFit::addConstraint(const V &cEq, const U &obs) {
    extendConstraints(ncon_p+1);
    return setConstraint(ncon_p-1, cEq, obs);
  }

  template <class U, class V>
  Bool LSQFit::addConstraint(const V &cEq,
			     const std::complex<U> &obs) {
    extendConstraints(ncon_p+2);
    return setConstraint((ncon_p-2)/2, cEq, obs);
  }

  template <class U, class V, class W>
  Bool LSQFit::addConstraint(uInt nIndex, const W &cEqIndex,
			     const V &cEq, const U &obs) {
    extendConstraints(ncon_p+1);
    return setConstraint(ncon_p-1, nIndex, cEqIndex, cEq, obs);
  }

  template <class U, class V, class W>
  Bool LSQFit::addConstraint(uInt nIndex, const W &cEqIndex,
			     const V &cEq,
			     const std::complex<U> &obs) {
    extendConstraints(ncon_p+2);
    return setConstraint((ncon_p-2)/2, nIndex, cEqIndex, cEq, obs);}

  template <class U>
  Bool LSQFit::getCovariance(U *covar) {
    if (!invertRect()) return False;
    for (uInt i=0; i<nun_p; i++) {		// all columns
      Double *j0 = nceq_p->row(i);
      U *j2 = covar + i*nun_p;
      for (uInt i1=0; i1<i; i1++) {		// return solution
	j2[i1] = static_cast<U>(nceq_p->row(i1)[i]);
      }
      for (uInt i1=i; i1<nun_p; i1++) {
	j2[i1] = static_cast<U>(j0[i1]);
      }
    }
    return True;
  }

  template <class U>
  Bool LSQFit::getCovariance(std::complex<U> *covar) {
    getWorkCOV();
    if (!LSQFit::getCovariance(wcov_p)) return False;
    for (uInt i=0; i<n_p; i += 2) {
      for (uInt j=0; j<n_p; j += 2) {
	covar[i*n_p/4 + j/2] =
	  std::complex<U>(wcov_p[i*n_p + j], wcov_p[i*n_p + j+1]);
      }
    }
    return True;
  }

  template <class U>
  Bool LSQFit::getErrors(U *errors) {
    if (!invertRect()) return False;
    for (uInt i=0; i<nun_p; ++i) {		// all columns
      *errors++ = sqrt(std::abs(nceq_p->row(i)[i]))*error_p[CHI2];
    }
    return True;
  }

  template <class U>
  Bool LSQFit::getErrors(std::complex<U> *errors) {
    if (!invertRect()) return False;
    for (uInt i=0; i+1<nun_p; i+=2) {		// all columns
      *errors++ = std::complex<U>(sqrt(std::abs(nceq_p->row(i)[i])),
				  sqrt(std::abs(nceq_p->row(i+1)[i+1])))*
	static_cast<U>(error_p[CHI2]);
    }
    return True;
  }

  template <class U>
  Bool LSQFit::getErrors(U &errors) {
    if (!invertRect()) return False;
    copyDiagonal(errors, 
		 typename LSQTraits
		 <typename std::iterator_traits<U>::value_type>::num_type());
    return True;
  }

  template <class U>
  void LSQFit::copyDiagonal(U &errors, LSQReal) {
    for (uInt i=0; i<nun_p; ++i) {		// all columns
      *errors++ = sqrt(std::abs(*nceq_p->diag(i)))*error_p[CHI2];
    }
  }

  template <class U>
  void LSQFit::copyDiagonal(U &errors, LSQComplex) {
    for (uInt i=0; i+1<nun_p; i+=2) {		// all columns
      *errors++ =
	typename U::value_type(sqrt(std::abs(nceq_p->row(i)[i])),
			       sqrt(std::abs(nceq_p->row(i+1)[i+1])))*
	(error_p[CHI2]);
    }
  }

} //# NAMESPACE CASACORE - END


#endif
