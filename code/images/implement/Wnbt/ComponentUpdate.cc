//# ComponentUpdate.cc: This class updates components in UV plane
//# Copyright (C) 2000
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

#include <trial/Wnbt/ComponentUpdate.h>
#include <aips/Exceptions/Error.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/VectorIter.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Utilities/Assert.h>

// Statics
const Int ComponentUpdate::N_unknown[N_Solve] = {
  3 
};
const LSQ::normType ComponentUpdate::solveType[N_Solve] = {
  LSQ::REAL
};

// Constructors
ComponentUpdate::ComponentUpdate() :
  soltp_p(SEPARATE), solve_p(ILM),
  nmodel_p(0),
  dt_p(0),
  complist_p(), fit_p() {
  init();
}

ComponentUpdate::ComponentUpdate(ComponentList &model) :
  soltp_p(SEPARATE), solve_p(ILM),
  nmodel_p(0),
  dt_p(0),
  complist_p(model), fit_p() {
  init();
}

ComponentUpdate::ComponentUpdate(ComponentList &model,
				 const ComponentUpdate::Solve solve) :
  soltp_p(SEPARATE), solve_p(solve),
  nmodel_p(0),
  dt_p(0),
  complist_p(model), fit_p() { 
  init();
}

ComponentUpdate::ComponentUpdate(ComponentList &model,
				 const ComponentUpdate::Solve solve,
				 const ComponentUpdate::Type tp) :
  soltp_p(tp), solve_p(solve),
  nmodel_p(0),
  dt_p(0),
  complist_p(model), fit_p() {
  init();
}

// Destructor
ComponentUpdate::~ComponentUpdate() {
  clean();
}

// Methods
void ComponentUpdate::makeEquations(const Array<DComplex> &deriv,
				    const Vector<DComplex> &data) {
  IPosition sz(deriv.shape());
  DebugAssert(data.shape()(0) == sz(2) &&
	      sz(1) == nmodel_p && sz(0) == N_unknown[solve_p],
	      AipsError);
  ReadOnlyVectorIterator<DComplex> ival(deriv); 
  // Make normal equations for uv points and for all sources
  for (Int j=0; j<sz(2); j++) {
    for (Int i=0; i<sz(1); i++) {
      // Real part
      for (Int k=0; k<N_unknown[solve_p]; k++) {
	dt_p[k] = real(ival.vector()(k));
      };
      fit_p[i]->makeNorm(dt_p, 1.0, real(data(j)));
      // Complex part
      for (Int k=0; k<N_unknown[solve_p]; k++) {
	dt_p[k] = imag(ival.vector()(k));
      };
      fit_p[i]->makeNorm(dt_p, 1.0, imag(data(j)));
      ival.next();
    };
  };
}

Bool ComponentUpdate::solve(Matrix<Double> &sol,
			    Matrix<Double> &err) {
  Double mu, sd;
  uInt rank;
  IPosition rs(2, N_unknown[solve_p], nmodel_p);
  if (sol.shape() != rs) {
    sol.resize();
    sol.resize(rs);
  };
  if (err.shape() != rs) {
    err.resize();
    err.resize(rs);
  };
  VectorIterator<Double> isol(sol);  
  VectorIterator<Double> ierr(err);  
  for (Int i=0; i<nmodel_p; i++) {
    fit_p[i]->invert(rank, True);
    fit_p[i]->solve(isol.vector(), sd, mu);
    fit_p[i]->getErrors(ierr.vector());
    isol.next();
    ierr.next();
  };
  return True;
}

void ComponentUpdate::init() {
  // Get number of solution areas
  if (soltp_p != SEPARATE) {
    throw(AipsError("In ComponentUpdate only SEPARATE implemented"));
  };
  // Free current fitters
  clean();
  nmodel_p = complist_p.nelements();
  // Fill new fitters
  fit_p.resize(nmodel_p);
  for (Int i=0; i<nmodel_p; i++) {
    fit_p[i] = new FitLSQ(N_unknown[solve_p], solveType[solve_p]);
  }; 
  dt_p = new Double[N_unknown[solve_p]];
}

void ComponentUpdate::clean() {
  for (uInt i=0; i<fit_p.nelements(); i++) {
    delete fit_p[i]; fit_p[i] = 0;
  };
  delete [] dt_p; dt_p = 0;
}










