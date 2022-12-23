//# SSMDirColumn.h: A Column for Direct Arrays in the Standard Storage Manager
//# Copyright (C) 2000
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

#ifndef TABLES_SSMDIRCOLUMN_H
#define TABLES_SSMDIRCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/SSMColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations


// <summary>
// A Direct Array Column in the Standard Storage Manager.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tStandardStMan.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=SSMBase>SSMBase</linkto>
//   <li> <linkto class=SSMColumn>SSMColumn</linkto>
//   <li> <linkto class=SSMStringHandler>SSMStringHandler</linkto>
// </prerequisite>

// <etymology>
// SSMDirColumn represents a Direct Array Column in the 
// Standard Storage Manager.
// </etymology>

// <synopsis>
// SSMDirColumn handles the access to a column containing direct
// arrays of the various data types.
// <br>
// It is derived from <linkto class=SSMColumn>SSMColumn</linkto>
// and uses most of its functions. The only thing done differently
// in this class is that it maintains no cache.
// Furthermore fixed length strings are not handled specially.
// All string arrays are stored in the special string buckets.
// </synopsis>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class SSMDirColumn : public SSMColumn
{
public:
  // Create a SSMDirColumn object with the given parent.
  // It initializes the various variables.
  // It keeps the pointer to its parent (but does not own it).
  SSMDirColumn (SSMBase* aParent, int aDataType, uInt aColNr);
  
  virtual ~SSMDirColumn();

  // Forbid copy constructor.
  SSMDirColumn (const SSMDirColumn&) = delete;
  
  // Forbid assignment.
  SSMDirColumn& operator= (const SSMDirColumn&) = delete;

  // An array of 'fixed length' strings is not handled specially,
  // thus this function is ignored.
  // It is needed to override the bahviour of the base class.
  virtual void setMaxLength (uInt maxLength);

  // Get an array value in the given row.
  virtual void getArrayV (rownr_t rownr, ArrayBase& dataPtr);
  
  // Put an array value in the given row.
  virtual void putArrayV (rownr_t rownr, const ArrayBase& dataPtr);

  // Remove the given row from the data bucket and possibly string bucket.
  virtual void deleteRow (rownr_t aRowNr);


protected:
  // Read the array data for the given row into the data buffer.
  void getValue (rownr_t aRowNr, void* data);
};



} //# NAMESPACE CASACORE - END

#endif
