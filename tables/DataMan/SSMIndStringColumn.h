//# SSMIndStringColumn.h: An Indirect String Array Column in the SSM
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
//#
//# $Id$

#ifndef TABLES_SSMINDSTRINGCOLUMN_H
#define TABLES_SSMINDSTRINGCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/SSMDirColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations


// <summary>
// An Indirect String Array Column in the Standard Storage Manager.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tSSMStringHandler.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=SSMBase>SSMBase</linkto>
//   <li> <linkto class=SSMDirColumn>SSMColumn</linkto>
//   <li> <linkto class=SSMStringHandler>SSMStringHandler</linkto>
// </prerequisite>

// <etymology>
// SSMIndStringColumn represents an Indirect String Array Column in the 
// Standard Storage Manager.
// </etymology>

// <synopsis>
// SSMIndStringColumn handles indirect variable shaped string arrays.
// Note that indirect fixed shape string arrays are handled by
// <linkto class=SSMDirColumn>SSMDirColumn</linkto>.
// <p>
// All string array access is handled by class
// <linkto class=SSMStringHandler>SSMStringHandler</linkto>, so
// SSMIndStringColumn is merely an interface to this class.
// The only thing it does is accessing the bucketnr, offset, and length
// in the data bucket.
// </synopsis>
  
// <motivation>
// The reason that indirect string arrays are handled here instead of
// in <linkto class=SSMIndColumn>SSMIndColumn</linkto> is that the string
// buckets are more disk space efficient when string arrays are frequently
// updated.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class SSMIndStringColumn : public SSMDirColumn
{
public:
  // Create a SSMIndStringColumn object with the given parent.
  // It initializes the various variables.
  // It keeps the pointer to its parent (but does not own it).
  SSMIndStringColumn (SSMBase* aParent, int aDataType, uInt aColNr);
  
  virtual ~SSMIndStringColumn();

  // Get an array value in the given row.
  // An exception is thrown if no array is defined in this row.
  virtual void getArrayStringV (uInt rownr, Array<String>* dataPtr);
  
  // Put an array value in the given row.
  virtual void putArrayStringV (uInt rownr, const Array<String>* dataPtr);

  // Set the shape of the array in the given row.
  void setShape (uInt aRowNr, const IPosition& aShape);
  
  // Get the shape of the array in the given row.
  virtual IPosition shape (uInt aRowNr);
  
  // This storage manager can handle changing array shapes.
  Bool canChangeShape() const;

  // Is the shape defined (i.e. is there an array) in this row?
  virtual Bool isShapeDefined (uInt aRowNr);

  // Get the dimensionality of the item in the given row.
  virtual uInt ndim (uInt aRowNr);


private:
  // Forbid copy constructor.
  SSMIndStringColumn (const SSMIndStringColumn&);
  
  // Forbid assignment.
  SSMIndStringColumn& operator= (const SSMIndStringColumn&);  
};



} //# NAMESPACE CASACORE - END

#endif




