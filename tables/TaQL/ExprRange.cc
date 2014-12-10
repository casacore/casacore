//# ExprRange.cc: Select range of a column in an select expression
//# Copyright (C) 1994,1995,1999,2000
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

#include <casacore/tables/TaQL/ExprRange.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableExprRange::TableExprRange()
: tabColPtr_p(0)
{}

TableExprRange::TableExprRange(const TableColumn& col, double stval,
			       double endval)
: sval_p     (1),
  eval_p     (1),
  tabColPtr_p(0)
{
    tabColPtr_p = new TableColumn(col);
    sval_p(0) = stval;
    eval_p(0) = endval;
}

TableExprRange::TableExprRange (const TableExprRange& that)
: sval_p     (that.sval_p),
  eval_p     (that.eval_p),
  tabColPtr_p(0)
{
    if (that.tabColPtr_p != 0) {
	tabColPtr_p = new TableColumn (*(that.tabColPtr_p));
    }
}

TableExprRange::~TableExprRange()
    { delete tabColPtr_p; }

TableExprRange& TableExprRange::operator= (const TableExprRange& that)
{
    if (this != &that) {
	sval_p       = that.sval_p;
	eval_p       = that.eval_p;
	delete tabColPtr_p;
	if (that.tabColPtr_p != 0) {
	    tabColPtr_p = new TableColumn (*(that.tabColPtr_p));
	}
    }
    return *this;
}

const TableColumn& TableExprRange::getColumn() const
    { return *tabColPtr_p; }


//# And two vectors of ranges.
//# This means taking the intersection of all ranges in both vectors.
//# Note that the start values are kept in ascending order, so that
//# makes searching easier.
void TableExprRange::mixAnd (const TableExprRange& that)
{
    //# Allocate vectors (long enough) to hold the result.
    uInt nrres=0;
    Vector<double> stres (sval_p.nelements() + that.sval_p.nelements());
    Vector<double> endres(sval_p.nelements() + that.sval_p.nelements());
    uInt i,j;
    //# Loop through all intervals of this.
    for (i=0; i<sval_p.nelements(); i++) {
	for (j=0; j<that.sval_p.nelements(); j++) {
	    if (that.sval_p(j) > eval_p(i)) {
		break;                              // that past this; next this
	    }
	    if (that.eval_p(j) >= sval_p(i)) {      // overlap
		stres(nrres)  = max (sval_p(i), that.sval_p(j));
		endres(nrres) = min (eval_p(i), that.eval_p(j));
		nrres++;
	    }
	}
    }
    //# Now copy the result (nrres elements of course) into this.
    sval_p.resize(nrres);
    eval_p.resize(nrres);
    if (nrres > 0) {
	sval_p = stres (Slice(0,nrres));
	eval_p = endres(Slice(0,nrres));
    }
}


void TableExprRange::mixOr (const TableExprRange& that)
{
    //# Allocate vectors (long enough) to hold the result.
    uInt nrres=0;
    Vector<double> stres (sval_p.nelements() + that.sval_p.nelements());
    Vector<double> endres(sval_p.nelements() + that.sval_p.nelements());
    uInt i;
    uInt j=0;
    //# Loop through all intervals of this.
    //# Store in the result, while inserting the that intervals,
    //# in order of start-value.
    for (i=0; i<sval_p.nelements(); i++) {
	while (j < that.sval_p.nelements()  &&  that.sval_p(j) < sval_p(i)) {
	    stres(nrres)  = that.sval_p(j);
	    endres(nrres) = that.eval_p(j);
	    nrres++;
	    j++;
	}
	stres(nrres)  = sval_p(i);
	endres(nrres) = eval_p(i);
	nrres++;
    }
    //# Append possible remaining that intervals.
    while (j < that.sval_p.nelements()) {
	stres(nrres)  = that.sval_p(j);
	endres(nrres) = that.eval_p(j);
	nrres++;
	j++;
    }
    //# Now combine overlapping intervals and store result in temporary.
    Vector<double> stmp(nrres);
    Vector<double> etmp(nrres);
    j=0;
    stmp(0) = stres(0);                             // first interval
    etmp(0) = endres(0);
    for (i=1; i<nrres; i++) {
	if (stres(i) <= etmp(j)) {                  // overlap
	    if (endres(i) > etmp(j)) {
		etmp(j) = endres(i);                // higher end-value
	    }
	}else{
	    j++;                                    // no overlap,
	    stmp(j) = stres(i);                     // so insert interval
	    etmp(j) = endres(i);
	}
    }
    nrres = j+1;
    //# Now set vectors to their final length and store values in them.
    //# Note that (opposite to Block) the Vector resize function does not
    //# preserve the values in the vector, so therefore we could not use
    //# sval_p/eval_p, but had to use temporaries.
    sval_p.resize(nrres);
    eval_p.resize(nrres);
    if (nrres > 0) {
	sval_p = stmp(Slice(0,nrres));
	eval_p = etmp(Slice(0,nrres));
    }
}

} //# NAMESPACE CASACORE - END

