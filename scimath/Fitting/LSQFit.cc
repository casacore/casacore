//# LSQFit.cc: Basic class for least squares fitting
//# Copyright (C) 1999,2000,2002,2004-2006,2008
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

//# Includes
#include <casacore/scimath/Fitting/LSQFit.h>
#include <algorithm>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Static values
LSQFit::Real      LSQFit::REAL      = LSQFit::Real();
LSQFit::Complex   LSQFit::COMPLEX   = LSQFit::Complex();
LSQFit::Separable LSQFit::SEPARABLE = LSQFit::Separable();
LSQFit::AsReal    LSQFit::ASREAL    = LSQFit::AsReal();
LSQFit::Conjugate LSQFit::CONJUGATE = LSQFit::Conjugate();

//# Constructors
LSQFit::LSQFit(uInt nUnknowns, uInt nConstraints)
  : state_p(0),
    nun_p(nUnknowns),  ncon_p(nConstraints), n_p(0), r_p(0),
    prec_p(1e-12), startnon_p(1e-3), nonlin_p(1),
    stepfactor_p(10), epsval_p(1e-6), epsder_p(1e-6),
    balanced_p(False), maxiter_p(0), niter_p(0), ready_p(NONREADY),
    piv_p(0), norm_p(0), nnc_p(0), nceq_p(0),
    known_p(0), error_p(0), constr_p(0),
    sol_p(0),
    nar_p(0), lar_p(0),
    wsol_p(0), wcov_p(0) {
  init();
  clear();
}

LSQFit::LSQFit(uInt nUnknowns,  const LSQReal &,
	       uInt nConstraints)
  : state_p(0),
    nun_p(nUnknowns), ncon_p(nConstraints), n_p(0), r_p(0),
    prec_p(1e-12), startnon_p(1e-3), nonlin_p(1),
    stepfactor_p(10), epsval_p(1e-6), epsder_p(1e-6),
    balanced_p(False), maxiter_p(0), niter_p(0), ready_p(NONREADY),
    piv_p(0), norm_p(0), nnc_p(0), nceq_p(0),
    known_p(0), error_p(0), constr_p(0),
    sol_p(0),
    nar_p(0), lar_p(0),
    wsol_p(0), wcov_p(0) {
  init();
  clear();
}

LSQFit::LSQFit(uInt nUnknowns, const LSQComplex &,
	       uInt nConstraints)
  : state_p(0), nun_p(2*nUnknowns), ncon_p(2*nConstraints),
    n_p(0), r_p(0),
    prec_p(1e-12), startnon_p(1e-3), nonlin_p(1), epsval_p(1e-8), epsder_p(1e-8),
    balanced_p(False), maxiter_p(0), niter_p(0), ready_p(NONREADY),
    piv_p(0), norm_p(0), nnc_p(0), nceq_p(0),
    known_p(0), error_p(0), constr_p(0),
    sol_p(0),
    nar_p(0), lar_p(0),
    wsol_p(0), wcov_p(0) {
  init();
  clear();
}

LSQFit::LSQFit()
  : state_p(0), nun_p(0), ncon_p(0),
    n_p(0), r_p(0),
    prec_p(1e-12), startnon_p(1e-3), nonlin_p(1), 
    stepfactor_p(10), epsval_p(1e-8), epsder_p(1e-8),
    balanced_p(False), maxiter_p(0), niter_p(0), ready_p(NONREADY),
    piv_p(0), norm_p(0), nnc_p(0), nceq_p(0),
    known_p(0), error_p(0), constr_p(0),
    sol_p(0),
    nar_p(0), lar_p(0),
    wsol_p(0), wcov_p(0) {}

LSQFit::LSQFit(const LSQFit &other) 
  : state_p(other.state_p), nun_p(other.nun_p), ncon_p(other.ncon_p),
    n_p(other.n_p), r_p(other.r_p), 
    prec_p(other.prec_p), startnon_p(other.startnon_p),
    nonlin_p(other.nonlin_p), stepfactor_p(other.stepfactor_p),
    epsval_p(other.epsval_p), epsder_p(other.epsder_p),
    balanced_p(other.balanced_p), maxiter_p(other.maxiter_p), 
    niter_p(other.niter_p), ready_p(other.ready_p),
    piv_p(0), norm_p(0), nnc_p(other.nnc_p), nceq_p(0),
    known_p(0), error_p(0), constr_p(0),
    sol_p(0),
    nar_p(0), lar_p(0),
    wsol_p(0), wcov_p(0) {
  init();
  copy(other);
}

LSQFit &LSQFit::operator=(const LSQFit &other) {
  if (this != &other) {
    deinit();
    state_p = other.state_p;
    nun_p = other.nun_p; 
    ncon_p = other.ncon_p;
    n_p = other.n_p; 
    r_p = other.r_p;
    prec_p = other.prec_p; 
    startnon_p = other.startnon_p;
    nonlin_p = other.nonlin_p;
    stepfactor_p = other.stepfactor_p;
    epsval_p = other.epsval_p;
    epsder_p = other.epsder_p;
    balanced_p = other.balanced_p;
    maxiter_p = other.maxiter_p; 
    niter_p = other.niter_p; 
    ready_p = other.ready_p;
    nnc_p = other.nnc_p;
    init();
    copy(other);
  }
  return *this;
}

//# Destructor
LSQFit::~LSQFit() {
  deinit();
}

void LSQFit::init() {
  n_p	= nun_p + ncon_p;
  r_p	= n_p;
  if (nun_p) {
    norm_p  = new LSQMatrix(nun_p);
    if (ncon_p) constr_p = new Double[nun_p*ncon_p];
  }
  if (n_p) known_p = new Double[n_p];
  error_p = new Double[N_ErrorField];
}

void LSQFit::clear() {
  if (piv_p) for (uInt *i=piv_p; i!=piv_p+n_p; ++i) *i = i-piv_p;
  if (norm_p) norm_p->clear();
  if (known_p) std::fill_n(known_p, n_p, 0.0);
  if (error_p) std::fill_n(error_p, uInt(N_ErrorField), 0.0);
  if (ncon_p) std::fill_n(constr_p, ncon_p*nun_p, 0.0);
  state_p = 0;
}

void LSQFit::deinit() {
  delete [] piv_p;	piv_p=0;
  delete    norm_p;	norm_p=0;
  delete [] known_p;	known_p=0;
  delete [] error_p;	error_p=0;
  delete [] sol_p;	sol_p=0;
  delete [] constr_p;	constr_p=0;
  delete    nceq_p;	nceq_p=0;
  delete    nar_p;	nar_p=0;
  delete [] lar_p;	lar_p=0;
  delete [] wsol_p; 	wsol_p=0;
  delete [] wcov_p; 	wcov_p=0;
}

void LSQFit::copy(const LSQFit &other, Bool all) {
  if (!nun_p) return;
  if (other.known_p  && !known_p)  known_p = new Double[n_p];
  if (other.error_p  && !error_p)  error_p = new Double[N_ErrorField];
  if (other.constr_p && !constr_p) constr_p= new Double[ncon_p*nun_p];
  if (other.nceq_p   && !nceq_p)   nceq_p  = new LSQMatrix(nnc_p);
  if (other.norm_p   && !norm_p)   norm_p  = new LSQMatrix(nun_p);
  if (all) {
    if (other.piv_p && !piv_p)	piv_p   = new uInt[nnc_p];
    if (other.sol_p && !sol_p)	sol_p   = new Double[nnc_p];
    if (other.lar_p && !lar_p) 	lar_p   = new Double[n_p*n_p];
  }
  if (other.norm_p)   norm_p->copy(*(other.norm_p));
  if (other.known_p)  std::copy(other.known_p, other.known_p+n_p, known_p);
  if (other.error_p)  std::copy(other.error_p, other.error_p+N_ErrorField,
				error_p);
  if (other.constr_p) std::copy(other.constr_p, other.constr_p+ncon_p*nun_p,
				constr_p);
  if (other.nceq_p)   nceq_p->copy(*(other.nceq_p));
  if (all) {
    if (other.piv_p) std::copy(other.piv_p, other.piv_p+nnc_p, piv_p);
    if (other.sol_p) std::copy(other.sol_p, other.sol_p+nnc_p, sol_p);
    if (other.lar_p) std::copy(other.lar_p, other.lar_p+n_p*n_p, lar_p);
  }
}

//# Member functions

Bool LSQFit::invert(uInt &nRank, Bool doSVD) {
  // Already done
  if ((n_p != nun_p) && (state_p & INVERTED)) return True;
  // Copy the data for solution equations
  createNCEQ();
  Double d0(0);						//collinearity test
  // Assume non-linear
  state_p &= ~NONLIN;
  // Make diagonal != 0
  nceq_p->doDiagonal(nun_p);
  // Special if constraints
  if (nnc_p != nun_p) {
    if (!invertRect()) return False;
  } else {
    // decompose
    for (uInt i=0; i<nnc_p; i++) {
      if (i<r_p) {					//still rank left
	Double *i3 = nceq_p->row(i);			//row pointer
	while (True) {
	  d0 = i3[i];					//get collinearity
	  for (uInt i2=0; i2<i; i2++) {
	    Double *i4 = nceq_p->row(i2);		//row pointer
	    d0 -= i4[i]*i4[i]/i4[i2];
	  }
	  if (d0*d0/i3[i] <= prec_p) {		 	//dependancy
	    if (!doSVD) return False;			//should be ok
	    if (i<r_p-1) {				//rank left
	      uInt j0 = r_p-1;				//rank pointer
	      for (uInt i2=0; i2<i; i2++) {		//shift pivot
		Double *i4 = nceq_p->row(i2);		//row pointer
		std::swap(i4[i], i4[j0]); 
	      }
	      std::swap(i3[i], nceq_p->row(j0)[j0]);
	      for (uInt i2=i+1; i2<j0; i2++) {
		std::swap(i3[i2], nceq_p->row(i2)[j0]);
	      }
	      Double *i4 = nceq_p->row(j0);	     	//row pointer
	      for (uInt i2=j0+1; i2<nnc_p; i2++) {	//shift pivot
		std::swap(i3[i2], i4[i2]);
	      }
	      r_p--;					//decrease rank
	      std::swap(piv_p[i], piv_p[j0]);
	      continue;
	    } else {
	      r_p = i;					//set rank
	    }
	  }
	  break;
	}
	i3[i] = d0;					//diagonal
	for (uInt i1=i+1; i1<nnc_p; i1++) {		//lu decomposition
	  for (uInt i2=0; i2<i; i2++) {
	    Double *i4 = nceq_p->row(i2);		//row pointer
	    i3[i1] -= i4[i]*i4[i1]/i4[i2];
	  }
	}
      }
    }
    // constraints
    for (uInt i1=r_p; i1<nnc_p; i1++) {
      for (uInt i=r_p-1; (Int)i>=0; i--) {
	Double *i3 = nceq_p->row(i);			//row pointer
	for (uInt i2=i+1; i2<r_p; i2++) {
	  i3[i1] += i3[i2]*nceq_p->row(i2)[i1];
	}
	i3[i1] /= -i3[i];
      }
    }
    // rank basis (a=i+g1'*.g1')
    for (uInt i=r_p; i<nnc_p; i++) {
      Double *i3 = nceq_p->row(i);			//row pointer
      for (uInt i1=i; i1<nnc_p; i1++) {
	i3[i1] = 0;
	for (uInt i2=0; i2<r_p; i2++) {
	  Double *i4 = nceq_p->row(i2);		   	//row pointer
	  i3[i1] += i4[i]*i4[i1];
	}
      }
      i3[i] += 1.0;
    }
    // triangular a
    for (uInt i=r_p; i<nnc_p; i++) {
      Double *i3 = nceq_p->row(i);			//row pointer
      for (uInt i1=i; i1<nnc_p; i1++) {
	for (uInt i2=r_p; i2<i; i2++) {
	  Double *i4 = nceq_p->row(i2);		   	//row pointer
	  i3[i1] -= i4[i]*i4[i1]/i4[i2];
	}
      }
    }
  }
  //
  nRank = r_p;						//rank
  // ready
  return True;
}

void LSQFit::solveIt() {
  getWorkSOL();
  if (state_p & INVERTED) {		                //constraints inverted
    for (uInt i1=0; i1<r_p; i1++) {		        //all unknowns
      Double *j0 = nceq_p->row(i1);
      sol_p[i1] = 0;
      for (uInt i2=0; i2<i1; i2++) {
	sol_p[i1] += nceq_p->row(i2)[i1]*known_p[i2];
      }
      for (uInt i2=i1; i2<r_p; i2++) {
	sol_p[i1] += j0[i2]*known_p[i2];
      }
    }
    Double dmu = 0;
    for (uInt i1=0; i1<r_p; i1++) {
      dmu += sol_p[i1]*known_p[i1];	 		//make rms
    }
    dmu = error_p[SUMLL] - dmu; 			//chi**2
    // error per observation
    dmu = sqrt(std::max(0.0, dmu/std::max(1.0, error_p[NC] - nun_p)));
    error_p[CHI2] = dmu;				//save
    std::copy(sol_p, sol_p+nnc_p, wsol_p);              //return solution
    // solve
  } else {						// normal
    for (uInt i1=0; i1<r_p; i1++) {			//all unknowns
      sol_p[i1] = known_p[piv_p[i1]];
      for (uInt i2=0; i2<i1; i2++) {
	Double *i3 = nceq_p->row(i2); 			//row pointer
	sol_p[i1] -= i3[i1]*sol_p[i2]/i3[i2];		//step 1
      }
    }
    for (uInt i1=r_p-1; (Int)i1>=0; i1--) {
      Double *i3 = nceq_p->row(i1); 			//row pointer
      for (uInt i2=i1+1; i2<r_p; i2++) {
	sol_p[i1] -= i3[i2]*sol_p[i2]; 			//solution
      }
      sol_p[i1] /= i3[i1];
    }
    Double dmu=0;
    for (uInt i1=0; i1<r_p; i1++) {
      dmu += sol_p[i1]*known_p[piv_p[i1]];		//make rms
    }
    dmu = error_p[SUMLL] - dmu; 			//chi**2
    dmu = sqrt(std::max(0.0, dmu/std::max(1.0, error_p[NC] - nun_p)));
    error_p[CHI2] = dmu; 				//save
    // Missing rank
    solveMR(nnc_p);
    // solution
    for (uInt i1=0; i1<nnc_p; ++i1) wsol_p[piv_p[i1]] = sol_p[i1];
  }
}

Bool LSQFit::solveItLoop(Double &fit, uInt &nRank, Bool doSVD) {
  if (!(state_p & NONLIN)) {       		// first time through loop
    nonlin_p = startnon_p;
    if (balanced_p) startnon_p *= norm_p->maxDiagonal(nun_p); // start factor
    stepfactor_p = 2;
    niter_p = maxiter_p;
    fit = 1.0;					// loop more
    ready_p = LSQFit::NONREADY;
    if (normInfKnown(known_p) <= epsder_p) ready_p = DERIVLEVEL; // known small
    createNCEQ();;;
    save(False);				// save current information
    state_p |= NONLIN;				// non-first loop
  } else {
    Double d0((error_p[SUMLL] + nar_p->error_p[SUMLL])/2.0);
    // Get fitting goodness (interim)
    if (d0>0) fit = (error_p[SUMLL] - nar_p->error_p[SUMLL])/d0;
    else fit = -1e-10;    			// dummy
    // Get expected improvement
    d0 = 0;
    if (balanced_p)
      for (uInt i=0; i<nun_p; ++i)
	d0 += nar_p->sol_p[i]*(nonlin_p*nar_p->sol_p[i]+known_p[i]);
    else
      for (uInt i=0; i<nun_p; ++i)
	d0 += nar_p->sol_p[i]*(nonlin_p*nar_p->sol_p[i]*(*norm_p->diag(i))+known_p[i]);  
    d0 *= 0.5;
    Double f = 0.5*(nar_p->error_p[SUMLL] - error_p[SUMLL]); 
    if (d0>0 && f>0) {
      if (balanced_p) {
	Double t0(2.0*f/d0-1.0), t1(1.0/3.0);
	t0 *= -t0*t0;
	t0 += 1.0;
	nonlin_p *= (t0>t1 ? t0 : t1);           // new factor
      } else nonlin_p *= 0.3; 
      stepfactor_p = 2;
      save(False);
      if (normInfKnown(known_p) <= epsder_p) ready_p = DERIVLEVEL; // known
    } else {
      nonlin_p *= stepfactor_p;
      stepfactor_p *= 2;
      if (stepfactor_p > 1e10) ready_p = NOREDUCTION; /// make it a constant
      for (Double *i=wsol_p, *i1=nar_p->sol_p; i!=wsol_p+nun_p; ++i,++i1)
	*i-=*i1; // new solution
      restore(False);				// restore info
    }
  }
  if (!ready_p && (maxiter_p==0 || niter_p>0)) {
    if (maxiter_p>0) --niter_p;
    if (balanced_p)  norm_p->addDiagonal(nun_p, nonlin_p);  // apply factor
    else  norm_p->mulDiagonal(nun_p, nonlin_p);
    if (!invert(nRank, doSVD)) {
      ready_p = SINGULAR;
      return False;	                        // decompose
    }
    std::copy(wsol_p, wsol_p+nun_p, nar_p->sol_p);// save current solution
    solveIt();			                // solve
    if (normSolution(wsol_p) <= epsval_p*(normSolution(nar_p->sol_p)+epsval_p))
      ready_p = SOLINCREMENT;
    std::swap_ranges(wsol_p, wsol_p+nun_p, nar_p->sol_p); // restore sol
    for (Double *i=wsol_p, *i1=nar_p->sol_p; i!=wsol_p+nun_p; ++i,++i1)
      *i+=*i1;
    nar_p->error_p[CHI2] = error_p[CHI2];;;
    nar_p->error_p[NC] = error_p[NC];;;
    nar_p->error_p[SUMWEIGHT] = error_p[SUMWEIGHT];;;
    clear();				      // clear for next part
    state_p |= NONLIN;			      // set in non-linear loop 
  } else if (!ready_p) ready_p = MAXITER;
  if (ready_p) fit = -1e-10;                  // force fit (old system)
  else fit = 1.0;
  return True;
}

void LSQFit::solveMR(uInt nin) {
  // missing rank
  for (uInt i1=r_p; i1<nin; i1++) {		//make b2=-g1'*.x1'
    sol_p[i1] = 0;
    for (uInt i2=0; i2<r_p; i2++) {
      sol_p[i1] -= sol_p[i2]*nceq_p->row(i2)[i1];
    }
  }
  // sol_pe x2
  for (uInt i1=r_p; i1<nin; i1++) {			//all unknowns
    for (uInt i2=r_p; i2<i1; i2++) {
      Double *i3 = nceq_p->row(i2); 			//row pointer
      sol_p[i1] -= i3[i1]*sol_p[i2]/i3[i2];		//step 1
    }
  }
  for (uInt i1=nin-1; (Int)i1>=(Int)r_p; i1--) {
    Double *i3 = nceq_p->row(i1);			//row pointer
    for (uInt i2=i1+1; i2<nin; i2++) {
      sol_p[i1] -= i3[i2]*sol_p[i2]; 			//solution
    }
    sol_p[i1] /= i3[i1];
  }
  // final x1
  if (r_p<nnc_p) {
    for (uInt i1=0; i1<r_p; i1++) {
      Double *i3 = nceq_p->row(i1);			//row pointer
      for (uInt i2=r_p; i2<nin; i2++) {
	sol_p[i1] += sol_p[i2]*i3[i2];
      }
    }
  }
}

Bool LSQFit::invertRect() {
  // Already done?
  if (state_p & INVERTED) return True;
  if (!lar_p) lar_p = new Double[nnc_p*nnc_p];	//get workspace
  if (nnc_p != nun_p) {				//lu necessary
    // lu decomposition
    // get matrix
    for (uInt i=0; i<nnc_p; i++) {		//fill matrix
      Double *j0 = nceq_p->row(i);		//input row
      Double *j1 = rowrt(i);			//output row
      j1[i] = j0[i];		 		//diagonal
      for (uInt i1=i+1; i1<nnc_p; i1++) {	//rest
	j1[i1] = j0[i1];
	rowrt(i1)[i] = j0[i1];
      }
    }
    // get scaling
    for (uInt i=0; i<nnc_p; i++) { 		//column loop
      Double d0 = 0;
      for (uInt i1=0; i1<nnc_p; i1++) {
	Double *j1 = rowrt(i1);
	if (std::abs(j1[i])>d0) d0 = std::abs(j1[i]);
      }
      if (d0 == 0) return False;		//cannot solve
      sol_p[i] = 1./d0;				//save scaling
    }
    // do crout
    for (uInt i1=0; i1<nnc_p; i1++) {		//all columns
      Double *j0 = rowrt(i1);
      for (uInt i=0; i<i1; i++) {
	for (uInt i2=0; i2<i; i2++) {
	  Double *j1 = rowrt(i2);
	  j0[i] -= j1[i]*j0[i2];
	}
      }
	
      Double d0 = 0;
      uInt i4 = 0;
      for (uInt i=i1; i<nnc_p; i++) {		//check pivot
	for (uInt i2=0; i2<i1; i2++) {
	  Double *j1 = rowrt(i2);
	  j0[i] -= j1[i]*j0[i2];
	}
	if (sol_p[i]*std::abs(j0[i]) >= d0) {	 //find best pivot
	  i4 = i;
	  d0 = sol_p[i]*std::abs(j0[i]);
	}
      }
      if (i1 != i4) {	 			//interchange rows
	for (uInt i2=0; i2<nnc_p; i2++) {
	  Double *j1 = rowrt(i2);
	  std::swap(j1[i4], j1[i1]);
	}
	sol_p[i4] = sol_p[i1];	 		//change scale factor
      }
      piv_p[i1] = i4;				//save pivot
      if (i1 != nnc_p-1) {	 		//correct for pivot
	for (uInt i=i1+1; i<nnc_p; i++) j0[i] /= j0[i1];
      }
    }
    // do invert
    for (uInt i3=0; i3<nnc_p; i3++) {		//all columns
      std::fill_n(sol_p, nnc_p, 0.0);           //inversion test
      sol_p[i3] = 1.0;
      for (uInt i=0; i<nnc_p; i++) {		//forward
	std::swap(sol_p[piv_p[i]], sol_p[i]);	//pivots
	for (uInt i1=0; i1<i; i1++) {
	  Double *j0 = rowrt(i1);
	  sol_p[i] -= j0[i]*sol_p[i1];
	}
      }
      for (uInt i=nnc_p-1; (Int)i>=0; i--) {	//backward
	Double *j0 = rowrt(i);
	for (uInt i1=i+1; i1<nnc_p; i1++) {
	  sol_p[i] -=  rowrt(i1)[i]*sol_p[i1];
	}
	sol_p[i] /= j0[i];
      }
      Double *j0 = nceq_p->row(i3);		//row result
      for (uInt i=i3; i<nnc_p; i++) {		//save inverted
	j0[i] = sol_p[i];
      }
    }
    state_p |= INVERTED;
    return True;
  }
  // invert cholesky
  for (uInt i=0; i<r_p; i++) {
    for (uInt i1=0; i1<r_p; i1++) {		//all unknowns
      if (i == piv_p[i1]) {
	sol_p[i1] = 1;
      } else {
	sol_p[i1] = 0;
      }
      for (uInt i2=0; i2<i1; i2++) {
	Double *i3 = nceq_p->row(i2); 		//row pointer
	sol_p[i1] -= i3[i1]*sol_p[i2]/i3[i2]; 	//step 1
      }
    }
    for (uInt i1=r_p-1; (Int)i1>=0; i1--) {
      Double *i3 = nceq_p->row(i1);		//row pointer
      for (uInt i2=i1+1; i2<r_p; i2++) {
	sol_p[i1] -= i3[i2]*sol_p[i2]; 		//solution
      }
      sol_p[i1] /= i3[i1];
    }
    // missing rank
    solveMR(nun_p);
    // solution
    for (uInt i1=0; i1<nun_p; i1++) {		//save solution
      rowrt(i)[piv_p[i1]] = sol_p[i1];
    }
  }
  // and again
  for (uInt i=0; i<nun_p; i++) {
    for (uInt i1=0; i1<r_p; i1++) {		//get cv
      sol_p[i1] = rowrt(i1)[i];
    }
    // missing rank
    solveMR(nun_p);
    // solution
    for (uInt i1=0; i1<nun_p; i1++) {		//save solution
      rowrt(piv_p[i1])[i] = sol_p[i1];
    }
  }
  // Save solution
  for (uInt i=0; i<nun_p; i++) {
    Double *j0 = nceq_p->row(i);		//output row
    Double *j1 = rowru(i);			//input row
    for (uInt i1=i; i1<nun_p; i1++) j0[i1] = j1[i1];
  }
  
  state_p |= INVERTED;
  return True;
}

Bool LSQFit::merge(const LSQFit &other) {
  if (other.nun_p != nun_p || 
      (state_p & ~NONLIN) != (other.state_p & ~NONLIN)) return False;
  // Copy normal equations
  Double *i2 = norm_p->row(0);
  Double *i3 = other.norm_p->row(0);
  for (uInt i=0; i<nun_p; ++i)
    for (uInt j=i; j<nun_p; ++j, ++i2, ++i3) *i2 += *i3;
  // Copy known terms
  i2 = known_p;
  i3 = other.known_p;
  for (uInt i=0; i<nun_p; ++i, ++i2, ++i3) *i2 += *i3;
  // Copy statistics information
  error_p[NC]        += other.error_p[NC];
  error_p[SUMWEIGHT] += other.error_p[SUMWEIGHT];
  error_p[SUMLL]     += other.error_p[SUMLL];
  // Copy constraint equations
  for (uInt i=0; i<other.ncon_p; ++i) {
    addConstraint(other.constr_p + i*other.nun_p, other.known_p[nun_p+i]);
  }  
  return True;
}

Bool LSQFit::mergeIt(const LSQFit &other, uInt nIndex, const uInt *nEqIndex) {
  ///  if (other.nun_p != nIndex || state_p || other.state_p) return False;
  if (other.nun_p != nIndex) return False;
  // Copy normal equations
  for (uInt i=0; i<nIndex; ++i) {
    if (nEqIndex[i]<nun_p) {
      Double *i3 = other.norm_p->row(i);
      for (uInt i1=i; i1<nIndex; ++i1) {
	if (nEqIndex[i1]<nun_p) {
	  if (nEqIndex[i] <= nEqIndex[i1]) {
	    norm_p->row(nEqIndex[i])[nEqIndex[i1]] += i3[i1];
	  } else norm_p->row(nEqIndex[i1])[nEqIndex[i]] += i3[i1];
	}
      }
    }
  }
  // Copy known terms
  Double *i2 = known_p;
  Double *i3 = other.known_p;
  for (uInt i=0; i<nIndex; ++i) {
    if (nEqIndex[i]<nun_p) i2[nEqIndex[i]] += i3[i];
  }
  // Copy statistics information
  error_p[NC]        += other.error_p[NC];
  error_p[SUMWEIGHT] += other.error_p[SUMWEIGHT];
  error_p[SUMLL]     += other.error_p[SUMLL];
  // Copy constraint equations
  for (uInt i=0; i<other.ncon_p; ++i) {
    addConstraint(nIndex, const_cast<uInt *>(nEqIndex),
		  other.constr_p + i*other.nun_p, other.known_p[nun_p+i]);
  }  
  return True;
}

void LSQFit::reset() {
  clear();
}

void LSQFit::extendConstraints(uInt n) {
  if ((constr_p && ncon_p == n) || nun_p==0) return; // Already right size
  if (n==0) {
    delete [] constr_p; constr_p = 0;
  } else {
    Double *newcon = new Double[n*nun_p];	// Newly sized area
    Double *newknw = new Double[n+nun_p];
    Double *cptr = newcon;			// Prepare copying
    Double *vptr = newknw;
    Double *inc = constr_p;
    Double *inv = known_p;
    for (uInt j=0; j<nun_p; ++j) *vptr++ =  *inv++;
    for (uInt i=0; i<ncon_p && i<n; ++i) {
      for (uInt j=0; j<nun_p; ++j) *cptr++ =  *inc++;
      *vptr++ = *inv++;
    }
    for (uInt i=ncon_p; i<n; ++i) {
      for (uInt j=0; j<nun_p; ++j) *cptr++ = 0;
      *vptr++ = 0;
    }
    delete [] constr_p; constr_p = newcon;
    delete [] known_p;  known_p  = newknw;
  }
  ncon_p = n;
  n_p    = nun_p+ncon_p;
  r_p    = n_p;
}

void LSQFit::createNCEQ(){
  if (nun_p == 0) return;
  if (!(state_p & TRIANGLE) || !nceq_p || nnc_p != n_p) {
    delete nceq_p;
    nnc_p = n_p;
    nceq_p = new LSQMatrix(nnc_p);
    delete [] piv_p;
    piv_p = new uInt[nnc_p];
    delete [] sol_p;
    sol_p = new Double[nnc_p];
  }
  // Copy the normal and constraint equations
  Double *ne = norm_p->trian_p;
  Double *se = nceq_p->trian_p;
  for (uInt i=0; i<nun_p; ++i) {
    for (uInt j=i; j<nun_p; ++j) *se++ = *ne++;
    for (uInt j=0; j<ncon_p; ++j) *se++ = constr_p[j*nun_p+i];
  }
  for (uInt i=nun_p; i<n_p; ++i) {
    for (uInt j=i; j<n_p; ++j) *se++ = 0;
  }
  // Initialise pivot table ///
  for (uInt i=0; i<nnc_p; ++i) piv_p[i] = i;
  state_p |= TRIANGLE;
}

void  LSQFit::set(uInt nUnknowns, uInt nConstraints) {
  deinit();
  nun_p = nUnknowns;
  ncon_p = nConstraints;
  init();
  clear();
}

void  LSQFit::set(uInt nUnknowns, const LSQComplex &, uInt nConstraints) {
  deinit();
  nun_p = 2*nUnknowns;
  ncon_p = 2*nConstraints;
  init();
  clear();
}

void  LSQFit::set(Double factor, Double LMFactor) {
  prec_p = factor*factor;
  startnon_p = LMFactor;
}

const std::string &LSQFit::readyText() const {
  static std::string txt[LSQFit::N_ReadyCode] =
    { "Not ready",
      "Incremental solution too small",
      "Residual vector too small",
      "Maximum number iterations reached",
      "No minimum CHI2 can be found",
      "Normal equations are singular" };
  return txt[ready_p];
}

void LSQFit::save(Bool all) {
  if (!nar_p) {
    nar_p = new LSQFit(*this);
  } else {
    nar_p->copy(*this, all);
  }
}

void LSQFit::restore(Bool all) {
  if (nar_p) copy(*nar_p, all);
}

Double LSQFit::getChi() const {
  Double *erv(error_p);
  if ((state_p & NONLIN) && nar_p) erv = nar_p->error_p;
  Double x = erv[CHI2];
  return x*x*(erv[NC] - nun_p);
}

Double LSQFit::getSD() const {
  Double *erv(error_p);
  if ((state_p & NONLIN) && nar_p) erv = nar_p->error_p;
  return erv[CHI2];
}

Double LSQFit::getWeightedSD() const {
  Double *erv(error_p);
  if ((state_p & NONLIN) && nar_p) erv = nar_p->error_p;
  Double x = erv[NC];
  if (erv[SUMWEIGHT] > 0.0) x /= erv[SUMWEIGHT];
  return erv[CHI2] * sqrt(std::max(0.0, x));
}

  Double LSQFit::normSolution(const Double *sol) const {
    Double ret(0);
    for (const Double *i=sol; i!=sol+nun_p; ++i) ret += *i * *i;
    return sqrt(ret);
  }

  Double LSQFit::normInfKnown(const Double *known) const {
    Double tmp(0), ret(0);
    for (const Double *i=known; i!=known+nun_p; ++i)
      if (ret < (tmp=std::abs(*i))) ret=tmp;
    return ret;
   }
 
void LSQFit::debugIt(uInt &nun, uInt &np, uInt &ncon, uInt &ner, uInt &rank,
		     Double *&nEq, Double *&known,
		     Double *&constr, Double *&er,
		     uInt *&piv, Double *&sEq, Double *&sol,
		     Double &prec, Double &nonlin) const {
  nun    = nun_p;
  np     = n_p;
  ncon   = ncon_p;
  ner    = N_ErrorField;
  rank   = r_p;
  nEq    = (norm_p ? norm_p->trian_p : 0);
  known  = known_p;
  constr = constr_p;
  er     = error_p;
  piv    = piv_p;
  sEq    = (nceq_p ? nceq_p->trian_p : 0);
  sol    = wsol_p;
  prec   = sqrt(prec_p);
  nonlin = nonlin_p;
}

void LSQFit::getWorkSOL() {
  if (!wsol_p) wsol_p = new Double[n_p];
}

void LSQFit::getWorkCOV() {
  if (!wcov_p) wcov_p = new Double[n_p*n_p];
}

} //# NAMESPACE CASACORE - END

