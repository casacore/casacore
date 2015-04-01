//# RefRows.cc: Class holding the row numbers in a RefTable
//# Copyright (C) 1998,1999,2001
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


#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

RefRows::RefRows (const Vector<uInt>& rowNumbers, Bool isSliced,
		  Bool collapse)
: itsRows   (rowNumbers),
  itsNrows  (rowNumbers.nelements()),
  itsSliced (isSliced)
{
    if (itsSliced) {
	AlwaysAssert (itsNrows%3 == 0, AipsError);
	itsNrows = 0;
    } else if (collapse) {
	//# Try to turn individual row numbers into slices.
	//# Stop doing that when the number of elements in the
	//# resulting array would exceed the input length, because
	//# in that case we gain not anything at all.
	Vector<uInt> rows(itsNrows+3);
	uInt start = 0;
	uInt end = 0;
	uInt incr = 0;
	uInt nv = 0;
	uInt nr = 0;
	for (uInt i=0; i<itsNrows  &&  nr<itsNrows; i++) {
	    uInt value = rowNumbers(i);
	    if (nv == 0) {
		start = value;
		nv++;
	    } else if (nv == 1) {
		if (value <= start) {
		    rows(nr++) = start;
		    rows(nr++) = start;
		    rows(nr++) = 1;
		    start = value;
		} else {
		    end = value;
		    incr = end - start;
		    nv++;
		}
	    } else if (value-end == incr) {
		end = value;
		nv++;
	    } else {
		rows(nr++) = start;
		if (nv > 2) {
		    rows(nr++) = end;
		    rows(nr++) = incr;
		    start = value;
		    nv = 1;
		} else {
		    rows(nr++) = start;
		    rows(nr++) = 1;
		    start = end;
		    end = value;
		    incr = end - start;
		    nv = 2;
		}
	    }
	}
	// Great, our result is smaller than the input. So use the result
	// after filling in the last slice.
	if (nr < itsNrows) {
	    rows(nr++) = start;
	    if (nv == 1) {
		rows(nr++) = start;
		rows(nr++) = 1;
	    } else {
		rows(nr++) = end;
		rows(nr++) = incr;
	    }
	    rows.resize (nr, True);
	    itsRows.reference (rows);
	    itsSliced = True;
	}
    }
}

RefRows::RefRows (uInt start, uInt end, uInt incr)
: itsRows   (3),
  itsNrows  (1 + (end-start)/incr),
  itsSliced (True)
{
    AlwaysAssert (start<=end, AipsError);
    itsRows(0) = start;
    itsRows(1) = end;
    itsRows(2) = incr;
}

RefRows::RefRows (const RefRows& other)
: itsRows   (other.itsRows),
  itsNrows  (other.itsNrows),
  itsSliced (other.itsSliced)
{}

// Assignment (copy semantics).
RefRows& RefRows::operator= (const RefRows& other)
{
    if (this != &other) {
	itsRows.resize (other.itsRows.nelements());
	itsRows   = other.itsRows;
	itsNrows  = other.itsNrows;
	itsSliced = other.itsSliced;
    }
    return *this;
}

RefRows::~RefRows()
{}

Bool RefRows::operator== (const RefRows& other) const
{
    return (itsSliced == other.itsSliced
              &&  itsRows.nelements() == other.itsRows.nelements()
              &&  allEQ (itsRows, other.itsRows));
}

uInt RefRows::fillNrows() const
{
    uInt nr = 0;
    uInt n = itsRows.nelements();

    for (uInt i=0; i<n; i+=3) {
	nr += 1 + (itsRows(i+1) - itsRows(i)) / itsRows(i+2);
    }
    ((RefRows*)this)->itsNrows = nr;
    return nr;
}

Vector<uInt> RefRows::convert (const Vector<uInt>& rootRownrs) const
{
    uInt n = nrow();
    Vector<uInt> rownrs(n);
    if (itsSliced) {
	uInt nr = 0;
        RefRowsSliceIter iter(*this);
        while (! iter.pastEnd()) {
            uInt rownr = iter.sliceStart();
            uInt end = iter.sliceEnd();
            uInt incr = iter.sliceIncr();
            while (rownr <= end) {
		DebugAssert (rownr <= rootRownrs.nelements(), AipsError);
		rownrs(nr++) = rootRownrs(rownr);
		rownr += incr;
     	    }
	    iter++;
        }
    } else {
        for (uInt i=0; i<n; i++) {
	    DebugAssert (itsRows(i) <= rootRownrs.nelements(), AipsError);
	    rownrs(i) = rootRownrs(itsRows(i));
	}
    }
    return rownrs;
}

Vector<uInt> RefRows::convert() const
{
    if (!itsSliced) {
        return itsRows;
    }
    uInt n = nrow();
    Vector<uInt> rownrs(n);
    uInt nr = 0;
    RefRowsSliceIter iter(*this);
    while (! iter.pastEnd()) {
        uInt rownr = iter.sliceStart();
	uInt end = iter.sliceEnd();
	uInt incr = iter.sliceIncr();
	while (rownr <= end) {
	    rownrs(nr++) = rownr;
	    rownr += incr;
	}
	iter++;
    }
    return rownrs;
}


RefRowsSliceIter::RefRowsSliceIter (const RefRows& rows)
: itsRows   (rows.rowVector()),
  itsSliced (rows.isSliced())
{
    reset();
}

void RefRowsSliceIter::reset()
{
    itsPos = 0;
    itsPastEnd = True;
    if (itsPos < itsRows.nelements()) {
	itsPastEnd = False;
	next();
    }
}

void RefRowsSliceIter::next()
{
    if (itsPastEnd) {
	throw (AipsError ("RefRowsSliceIter::next - past end"));
    }
    if (itsPos >= itsRows.nelements()) {
	itsPastEnd = True;
    } else {
	itsStart = itsRows(itsPos++);
	if (itsSliced) {
	    itsEnd  = itsRows(itsPos++);
	    itsIncr = itsRows(itsPos++);
	} else {
	    itsEnd  = itsStart;
	    itsIncr = 1;
	}
    }
}

} //# NAMESPACE CASACORE - END

