//# MSSourceIndex: index into a MeasurementSet SOURCE subtable
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

#ifndef MS_MSSOURCEINDEX_H
#define MS_MSSOURCEINDEX_H

#include <casacore/casa/aips.h>
#include <casacore/ms/MSSel/MSTableIndex.h>
#include <casacore/ms/MeasurementSets/MSSourceColumns.h>

#include <casacore/casa/Containers/RecordField.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# forward declarations
class MSSource;

// <summary>
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> MSTableIndex
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//

class MSSourceIndex : public MSTableIndex
{
public:
    // no index attached, use the attach function or assignment operator to change that
  MSSourceIndex();

  // construct one using the indicated SOURCE table
  MSSourceIndex(const MSSource &source);

  // construct one from another
  MSSourceIndex(const MSSourceIndex &other);

  virtual ~MSSourceIndex();

  MSSourceIndex &operator=(const MSSourceIndex &other);

  void attach(const MSSource &source);

  // access to the source ID key, throws an exception if isNull() is false
  int32_t &sourceId() {return *sourceId_p;}

  // access to the spectral window ID key, throws an 
  // exception if isNull() is false
  int32_t &spectralWindowId() {return *spwId_p;}

  // Match a source name or list of source names to a set of SOURCE_ID's
  Vector<int32_t> matchSourceName(const String& name);
  Vector<int32_t> matchSourceName(const Vector<String>& names);

  //add for source code selection
  Vector<int32_t> matchSourceCode(const String& code);

  //Return rows matching a SourceID
  RowNumbers getRowNumbersOfSourceID(const int32_t sid);


protected:
  // the specialized compare function to pass to the
  // <linkto class=ColumnsIndex>ColumnsIndex</linkto> object.  This supports -1
  // values for the SPECTRAL_WINDOW_ID
  static int32_t compare (const Block<void*>& fieldPtrs,
                      const Block<void*>& dataPtrs,
                      const Block<int32_t>& dataTypes,
                      rownr_t index);
  
private:
  // Pointer to local MSSourceColumns object
  MSSourceColumns* msSourceCols_p;
  
  RecordFieldPtr<int32_t> sourceId_p, spwId_p;

  void attachIds();

};


} //# NAMESPACE CASACORE - END

#endif
    
