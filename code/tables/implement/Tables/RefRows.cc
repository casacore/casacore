//# RefRows.cc: Class holding the row numbers in a RefTable
//# Copyright (C) 1998
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


#include <aips/Tables/RefRows.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


RefRows::RefRows (const Vector<Int>& rowNumbers, Bool collapse)
: itsRows  (rowNumbers),
  itsNrows (0)
{
    uInt n = rowNumbers.nelements();
    if (collapse  &&  n>0) {
	itsNrows = n;
	itsRows.resize (0);
	itsRows.resize (n);
	if (rowNumbers(0) < 0) {
	    throw (AipsError ("RefRows::RefRows - invalid row specs"));
	}
	Int start, end, incr;
	uInt nv = 0;
	uInt nr = 0;
	for (uInt i=0; i<n; i++) {
	    Int value = rowNumbers(i);
	    if (value < 0) {
		throw (AipsError ("RefRows::RefRows - invalid row specs"));
	    }
	    if (nv == 0) {
		start = value;
		nv++;
	    } else if (nv == 1) {
		if (value <= start) {
		    itsRows(nr++) = start;
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
		itsRows(nr++) = start;
		if (nv > 2) {
		    itsRows(nr++) = -end;
		    if (incr != 1) {
			itsRows(nr++) = -incr;
		    }
		}
		start = value;
		nv = 1;
	    }
	}
	itsRows(nr++) = start;
	if (nv == 2) {
	    itsRows(nr++) = end;
	} else if (nv > 2) {
	    itsRows(nr++) = -end;
	    if (incr != 1) {
		itsRows(nr++) = -incr;
	    }
	}
	itsRows.resize (nr, True);
    }
}

RefRows::RefRows (const Vector<uInt>& rowNumbers)
: itsNrows (rowNumbers.nelements())
{
    uInt n = rowNumbers.nelements();
    itsRows.resize (n);
    Int start, end, incr;
    uInt nv = 0;
    uInt nr = 0;
    for (uInt i=0; i<n; i++) {
	Int value = rowNumbers(i);
	if (nv == 0) {
	    start = value;
	    nv++;
	} else if (nv == 1) {
	    if (value <= start) {
		itsRows(nr++) = start;
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
	    itsRows(nr++) = start;
	    if (nv > 2) {
		itsRows(nr++) = -end;
		if (incr != 1) {
		    itsRows(nr++) = -incr;
		}
		start = value;
		nv = 1;
	    } else {
		start = end;
		end = value;
		incr = end - start;
		nv = 2;
	    }
	}
    }
    itsRows(nr++) = start;
    if (nv == 2) {
	itsRows(nr++) = end;
    } else if (nv > 2) {
	itsRows(nr++) = -end;
	if (incr != 1) {
	    itsRows(nr++) = -incr;
	}
    }
    itsRows.resize (nr, True);
}

RefRows::RefRows (uInt start, uInt end, uInt incr)
: itsRows  (3),
  itsNrows (1 + (end-start)/incr)
{
    AlwaysAssert (start<=end, AipsError);
    itsRows(0) = start;
    itsRows(1) = -end;
    itsRows(2) = -incr;
}

RefRows::RefRows (const RefRows& other)
: itsRows (other.itsRows)
{}

// Assignment (copy semantics).
RefRows& RefRows::operator= (const RefRows& other)
{
    if (this != &other) {
	itsRows.resize (other.itsRows.nelements());
	itsRows = other.itsRows;
    }
    return *this;
}

RefRows::~RefRows()
{}

Bool RefRows::operator== (const RefRows& other) const
{
    return ToBool(itsRows.nelements() == other.itsRows.nelements()
              &&  allEQ (itsRows.ac(), other.itsRows.ac()));
}

uInt RefRows::fillNrows() const
{
    uInt n = 0;
    RefRowsSliceIter iter(*this);
    while (! iter.pastEnd()) {
	n += 1 + (iter.sliceEnd() - iter.sliceStart()) / iter.sliceIncr();
    }
    ((RefRows*)this)->itsNrows = n;
    return n;
}



RefRowsSliceIter::RefRowsSliceIter (const RefRows& rows)
: itsRows (rows.rowVector())
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
        itsEnd = itsStart;
        itsIncr = 1;
	if (itsPos < itsRows.nelements()  &&  itsRows(itsPos) < 0) {
	    itsEnd = -itsRows(itsPos++);
	    if (itsPos < itsRows.nelements()  &&  itsRows(itsPos) < 0) {
		itsIncr = -itsRows(itsPos++);
	    }
	}
    }
}




RefRowsRowIter::RefRowsRowIter (const RefRows& rows)
: itsRows (rows.rowVector())
{
    reset();
}

void RefRowsRowIter::reset()
{
    itsPos = 0;
    itsRow = 0;
    itsEnd = 0;
    itsIncr = 1;
    itsPastEnd = True;
    if (itsPos < itsRows.nelements()) {
	itsPastEnd = False;
	next();
    }
}

void RefRowsRowIter::nextSlice()
{
    if (itsPastEnd) {
	throw (AipsError ("RefRowsRowIter::next - past end"));
    }
    if (itsPos >= itsRows.nelements()) {
	itsPastEnd = True;
    } else {
	itsRow = itsRows(itsPos++);
	itsEnd = itsRow;
	itsIncr = 1;
	if (itsPos < itsRows.nelements()  &&  itsRows(itsPos) < 0) {
	    itsEnd = -itsRows(itsPos++);
	    if (itsPos < itsRows.nelements()  &&  itsRows(itsPos) < 0) {
		itsIncr = -itsRows(itsPos++);
	    }
	}
    }
}
