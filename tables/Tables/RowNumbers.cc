//# RowNumbers.cc: Vector of row numbers
//# Copyright (C) 2019
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
#include <casacore/tables/Tables/RowNumbers.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>

namespace casacore { //#Begin namespace casacore

  RowNumbers::RowNumbers (const std::vector<rownr_t>& rows)
  {
    // Resize the Vector in the parent class and convert to it.
    resize (rows.size());
    std::copy (rows.begin(), rows.end(), this->cbegin());
  }

  RowNumbers::RowNumbers (const Vector<uInt>& rows)
  {
    // Resize the Vector in the parent class and convert to it.
    resize (rows.size());
    convertArray (*this, rows);
  }

  RowNumbers::RowNumbers (const std::vector<uInt>& rows)
  {
    // Resize the Vector in the parent class and convert to it.
    resize (rows.size());
    std::copy (rows.begin(), rows.end(), this->cbegin());
  }

  Array<rownr_t>& RowNumbers::operator= (const Array<rownr_t>& other)
  {
    Vector<rownr_t>::operator= (other);
    return *this;
  }

  Vector<uInt> RowNumbers::convertRownrVector (const Vector<rownr_t>& rows64)
  {
    AlwaysAssert (allLE (rows64, rownr_t(std::numeric_limits<uInt>::max())), AipsError);
    Vector<uInt> rows(rows64.size());
    convertArray (rows, rows64);
    return rows;
  }
  
} //#End namespace casacore
