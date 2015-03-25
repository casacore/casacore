//# MSFieldIndex: index or lookup in a MeasurementSet FIELD subtable
//# Copyright (C) 2000,2001
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

#ifndef MS_MSFIELDINDEX_H
#define MS_MSFIELDINDEX_H

//# includes
#include <casacore/casa/aips.h>
#include <casacore/ms/MeasurementSets/MSField.h>
#include <casacore/ms/MSSel/MSSelectionError.h>
#include <casacore/ms/MeasurementSets/MSFieldColumns.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# forward declarations

// <summary>
// Class to handle lookup or indexing into a MS FIELD subtable
// </summary>

// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> MSField
// </prerequisite>
//
// <etymology>
// From "MeasurementSet", "FIELD subtable" and "index".
// </etymology>
//
// <synopsis>
// This class provides lookup and indexing into an MS FIELD
// subtable. These services include returning rows numbers
// (which for the FIELD subtable are FIELD_ID's) associated 
// with specific data in the subtable.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// Collect together all subtable indexing and lookup for the
// FIELD subtable, for encapsulation and efficiency.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//

class MSFieldIndex 
{
public:
  // Construct from an MS FIELD subtable
  MSFieldIndex(const MSField &field);

  // Null destructor
  virtual ~MSFieldIndex() {}

  // Look up a single name in FIELD.NAME or FIELD.CODE
  Vector<Int> matchFieldNameOrCode(const String& name);
  // Look up FIELD_ID's for a given field name, or set of field names
  Vector<Int> matchFieldName(const String& name);
  Vector<Int> matchFieldName(const Vector<String>& names);

  //ADD for file name wildcard selection
  Vector<Int> matchSubFieldName(const String& name);

  // Look up FIELD_ID's for a given pattern/regex for source name/code
  Vector<Int> matchFieldRegexOrPattern(const String& pattern,
				       const Bool regex=False);
  Vector<Int> matchFieldNameRegexOrPattern(const String& pattern,
					   const Bool regex=False);
  Vector<Int> matchFieldCodeRegexOrPattern(const String& pattern,
					   const Bool regex=False);
  // Look up FIELD_ID's for a given source id
  Vector<Int> matchSourceId(const Int& sourceId);
  Vector<Int> matchSourceId(const Vector<Int>& sourceIds);
  Vector<Int> validateIndices(const Vector<Int>& sourceIds);

  // Add for field code selection
  Vector<Int> matchFieldCode(const String& code);

  Vector<Int> maskFieldIDs(const Vector<Int>& ids);

  Vector<Int> matchFieldIDLT(const Int n);
  Vector<Int> matchFieldIDGT(const Int n);
  Vector<Int> matchFieldIDGTAndLT(const Int n0, const int n1);
private:
  // Disallow null constructor
  MSFieldIndex();

  // FIELD subtable column accessor
  ROMSFieldColumns msFieldCols_p;

  // Vector cache of field id's
  Vector<Int> fieldIds_p;
  Int nrows_p;
};


} //# NAMESPACE CASACORE - END

#endif
    
