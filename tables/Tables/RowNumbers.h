//# RowNumbers.h: Vector of row numbers
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef TABLES_ROWNUMBERS_H
#define TABLES_ROWNUMBERS_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //#Begin namespace casacore

  class RowNumbers: public Vector<rownr_t>
  {
  public:
    // Construct an empty RowNumbers Vector.
    RowNumbers()
    {}

    // Construct with the given length.
    explicit RowNumbers (size_t n)
      : Vector<rownr_t> (n)
    {}
    
    // Construct from a Vector of row numbers.
    RowNumbers (const Vector<rownr_t>& rows)
      : Vector<rownr_t> (rows)
    {}

    // Construct from a std::vector of row numbers.
    RowNumbers (const std::vector<rownr_t>& rows);

    // Array<T> has this virtual function, so also define in this
    // class to avoid 'virtual function override' warning.
    virtual Array<rownr_t>& operator= (const Array<rownr_t>& other);

    // Construct from a Vector or std::vector of old style row numbers.
#ifdef IMPLICIT_CTDS_32BIT
    RowNumbers (const Vector<uInt>& rows);
    RowNumbers (const std::vector<uInt>& rows);
#else
    explicit RowNumbers (const Vector<uInt>& rows);
    explicit RowNumbers (const std::vector<uInt>& rows);
#endif
    
    // Conversion operator to convert Vector<rownr_t> to Vector<uInt>.
    // This is for backward compatibility of Table::rowNumbers.
#ifdef IMPLICIT_CTDS_32BIT
    operator Vector<uInt>() const
#else
    explicit operator Vector<uInt>() const
#endif
      { return convertRownrVector (*this); }

    // Do the actual conversion.
    // An exception is thrown if a row number exceeds 32 bits.
    static Vector<uInt> convertRownrVector (const Vector<rownr_t>&);
  };
  
} //#End namespace casacore

#endif
