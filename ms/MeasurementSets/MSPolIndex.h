//# MSPolIndex: index or lookup in an MS POLARIZATION subtable
//# Copyright (C) 2000,2001,2002
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
//#
//# $Id$

#ifndef MS_MSPOLINDEX_H
#define MS_MSPOLINDEX_H

//# includes
#include <casacore/casa/aips.h>
#include <casacore/ms/MeasurementSets/MSPolarization.h>
#include <casacore/ms/MeasurementSets/MSPolColumns.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# forward declarations

// <summary>
// Class to handle lookup or indexing into an MS POLARIZATION subtable
// </summary>

// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> MSPolarization
// </prerequisite>
//
// <etymology>
// From "MeasurementSet", "POLARIZATION subtable" and "index".
// </etymology>
//
// <synopsis>
// This class provides lookup and indexing into an MS POLARIZATION
// subtable. These services include returning rows numbers
// (which for the POLARIZATION subtable are POLARIZATION_ID's) associated 
// with specific data in the subtable.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// Collect together all subtable indexing and lookup for the
// POLARIZATION subtable, for encapsulation and efficiency.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//

class MSPolarizationIndex 
{
public:
  // Construct from an MS POLARIZATION subtable
  MSPolarizationIndex(const MSPolarization& polarizationTable);

  // Null destructor
  virtual ~MSPolarizationIndex() {}

  // Look up POLARIZATION_ID's for a given set of polarization correlation
  // types and receptor cross-products
  Vector<Int> matchCorrTypeAndProduct(const Vector<Int>& corrType,
				      const Matrix<Int>& corrProduct);
  // ///////////////////  Add for MS selection //////////////////////////////
  // Only Look up POLARIZATION_ID's for a given set of polarization correlation
  // types
  Vector<Int> matchCorrType(const Vector<Int>& corrType,Bool exactMatch=True);

private:
  // Disallow null constructor
  MSPolarizationIndex();

  // POLARIZATION subtable column accessor
  ROMSPolarizationColumns msPolarizationCols_p;

  // Vector cache of polarization id's
  Vector<Int> polarizationIds_p;
  Int nrows_p;
};


} //# NAMESPACE CASACORE - END

#endif
    
