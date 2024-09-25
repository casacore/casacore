//# ConcatRows.cc: Class holding the row numbers in a ConcatTable
//# Copyright (C) 2008
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# Includes
#include <casacore/tables/Tables/ConcatRows.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/BinarySearch.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  void ConcatRows::add (rownr_t nrow)
  {
    if (Int64(nrow) + itsRows[itsNTable] >= Int64(65536)*65536) {
      throw TableError ("Concatenation of tables exceeds 2**32 rows");
    }
    itsNTable++;
    itsRows.resize (itsNTable+1);
    itsRows[itsNTable] = itsRows[itsNTable-1] + nrow;
  }

  void ConcatRows::findRownr (rownr_t rownr) const
  {
    if (rownr >= itsRows[itsNTable]) {
      throw TableError ("ConcatTable: rownr " + String::toString(rownr) +
			" past nr of rows (=" +
			String::toString(itsRows[itsNTable]) + ')');
    }
    Bool found;
    Int inx = binarySearchBrackets (found, itsRows, rownr, itsNTable);
    if (!found) {
      inx--;
    }
    DebugAssert (inx>=0  &&  static_cast<uInt>(inx)<itsNTable, AipsError);
    itsLastStRow   = itsRows[inx];
    itsLastEndRow  = itsRows[inx+1];
    itsLastTableNr = inx;
  }



  ConcatRowsIter::ConcatRowsIter (const ConcatRows& rows)
    : itsRows  (&rows),
      itsChunk (3),
      itsStart (0),
      itsEnd   (rows.nrow()),
      itsIncr  (1),
      itsTabNr (0)
  {
    itsPastEnd  = (itsEnd==0);
    itsChunk[0] = 0;
    itsChunk[1] = rows.ntable()>0 ? rows[0]-1 : 0;
    itsChunk[2] = 1;
  }

    // Construct the iterator on a ConcatRows object for the given row range.
  ConcatRowsIter::ConcatRowsIter (const ConcatRows& rows,
				  rownr_t start, rownr_t end, rownr_t incr)
    : itsRows  (&rows),
      itsChunk (3),
      itsStart (start),
      itsEnd   (std::min(end+1, rows.nrow())),
      itsIncr  (incr),
      itsTabNr (0)
  {
    if (itsStart >= itsEnd) {
      itsPastEnd = True;
    } else {
      itsPastEnd = False;
      rows.mapRownr (itsTabNr, itsChunk[0], start);
      itsChunk[1] = std::min(rows[itsTabNr], itsEnd) - 1 - rows[itsTabNr-1];
      itsChunk[2] = itsIncr;
    }
  }

  void ConcatRowsIter::next()
  {
    if (!itsPastEnd) {
      if (itsTabNr+1 >= itsRows->ntable()  ||  (*itsRows)[itsTabNr] >= itsEnd) {
	itsPastEnd = True;
      } else {
	itsChunk[0] = 0;
	if (itsIncr != 1) {
	  rownr_t rem = ((*itsRows)[itsTabNr] - itsStart) % itsIncr;
	  if (rem != 0) {
	    itsChunk[0] = itsIncr - rem;
	  }
	}
	itsChunk[1] = std::min((*itsRows)[itsTabNr+1], itsEnd) - 1 -
	  (*itsRows)[itsTabNr];
	++itsTabNr;
      }
    }
  }

} //# NAMESPACE CASACORE - END
