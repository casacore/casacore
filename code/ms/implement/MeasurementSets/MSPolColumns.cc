//# NewMSPolarizationColumns.cc:  provides easy access to NewMeasurementSet columns
//# Copyright (C) 1999,2000
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

#include <aips/MeasurementSets/NewMSPolColumns.h>
#include <aips/MeasurementSets/NewMSPolarization.h>
#include <aips/Utilities/String.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>

RONewMSPolarizationColumns::
RONewMSPolarizationColumns(const NewMSPolarization& msPolarization):
  corrProduct_p(msPolarization, NewMSPolarization::
		columnName(NewMSPolarization::CORR_PRODUCT)),
  corrType_p(msPolarization, NewMSPolarization::
	     columnName(NewMSPolarization::CORR_TYPE)),
  flagRow_p(msPolarization, NewMSPolarization::
	    columnName(NewMSPolarization::FLAG_ROW)),
  numCorr_p(msPolarization, NewMSPolarization::
	    columnName(NewMSPolarization::NUM_CORR))
{}

RONewMSPolarizationColumns::~RONewMSPolarizationColumns() {}

RONewMSPolarizationColumns::RONewMSPolarizationColumns():
  corrProduct_p(),
  corrType_p(),
  flagRow_p(),
  numCorr_p()
{
}

void RONewMSPolarizationColumns::
attach(const NewMSPolarization& msPolarization)
{
  corrProduct_p.attach(msPolarization, NewMSPolarization::
		       columnName(NewMSPolarization::CORR_PRODUCT));
  corrType_p.attach(msPolarization, NewMSPolarization::
		    columnName(NewMSPolarization::CORR_TYPE));
  flagRow_p.attach(msPolarization, NewMSPolarization::
		   columnName(NewMSPolarization::FLAG_ROW));
  numCorr_p.attach(msPolarization, NewMSPolarization::
		   columnName(NewMSPolarization::NUM_CORR));
}

Int RONewMSPolarizationColumns::
match(const Vector<Stokes::StokesTypes>& polType, Int tryRow=-1) {
  uInt r = nrow();
  if (r == 0) return -1;
  // Convert the corrType to Integers.
  const Int nCorr = polType.nelements();
  Vector<Int> polInt(nCorr);
  for (Int p = 0; p < nCorr; p++) {
    polInt(p) = polType(p);
  }
  // Main matching loop
  if (tryRow >= 0) {
    const uInt tr = tryRow;
    if (tr >= r) {
      throw(AipsError("RONewMSPolarszationColumns::match(...) - "
                      "the row you suggest is too big"));
    }
    if (!flagRow()(tr) &&
	numCorr()(tr) == nCorr && 
	matchCorrType(tr, polInt)) {
      return tr;
    }
    if (tr == r-1) r--;
  }
  while (r > 0) {
    r--;
    if (!flagRow()(r) &&
	numCorr()(r) == nCorr && 
	matchCorrType(r, polInt)) {
      return r;
    }
  }
  return -1;
}

Bool RONewMSPolarizationColumns::
matchCorrType(uInt row, const Vector<Int>& polType) const {
  DebugAssert(row < nrow(), AipsError);
  return allEQ(corrType()(row), polType);
}

Bool RONewMSPolarizationColumns::
matchCorrProduct(uInt row, const Matrix<Int>& polProduct) const {
  DebugAssert(row < nrow(), AipsError);
  return allEQ(corrProduct()(row), polProduct);
}

NewMSPolarizationColumns::
NewMSPolarizationColumns(NewMSPolarization& msPolarization):
  RONewMSPolarizationColumns(msPolarization),
  corrProduct_p(msPolarization, NewMSPolarization::
		columnName(NewMSPolarization::CORR_PRODUCT)),
  corrType_p(msPolarization, NewMSPolarization::
	   columnName(NewMSPolarization::CORR_TYPE)),
  flagRow_p(msPolarization, NewMSPolarization::
	  columnName(NewMSPolarization::FLAG_ROW)),
  numCorr_p(msPolarization, NewMSPolarization::
	    columnName(NewMSPolarization::NUM_CORR))
{}

NewMSPolarizationColumns::~NewMSPolarizationColumns() {}

NewMSPolarizationColumns::NewMSPolarizationColumns():
  RONewMSPolarizationColumns(),
  corrProduct_p(),
  corrType_p(),
  flagRow_p(),
  numCorr_p()
{
}

void NewMSPolarizationColumns::
attach(NewMSPolarization& msPolarization)
{
  RONewMSPolarizationColumns::attach(msPolarization);
  corrProduct_p.attach(msPolarization, NewMSPolarization::
		       columnName(NewMSPolarization::CORR_PRODUCT));
  corrType_p.attach(msPolarization, NewMSPolarization::
		    columnName(NewMSPolarization::CORR_TYPE));
  flagRow_p.attach(msPolarization, NewMSPolarization::
		   columnName(NewMSPolarization::FLAG_ROW));
  numCorr_p.attach(msPolarization, NewMSPolarization::
		   columnName(NewMSPolarization::NUM_CORR));
}
// Local Variables: 
// compile-command: "gmake NewMSPolColumns"
// End: 
