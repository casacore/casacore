//# SSMIndStringColumn.h: A Column for Indirect String Arrays in the 
//# Standard Storage Manager
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

#if !defined(AIPS_SSMINDSTRINGCOLUMN_H)
#define AIPS_SSMINDSTRINGCOLUMN_H


//# Includes
#include <aips/aips.h>
#include <aips/Tables/SSMDirColumn.h>

//# Forward declarations


// <summary>
// An Indirect String Array Column in the Standard Storage Manager.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=SSMBase>SSMBase</linkto>
//   <li> <linkto class=SSMColumn>SSMColumn</linkto>
// </prerequisite>

// <etymology>
// SSMIndStringColumn represents an indirect String Array Column in the 
// Standard Storage Manager.
// </etymology>

// <synopsis>
// </synopsis>
  
// <motivation>
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class SSMIndStringColumn : public SSMDirColumn
{
public:
  SSMIndStringColumn (SSMBase* aParent, int aDataType, uInt aColNr);
  
  virtual ~SSMIndStringColumn();

  // Get an array value in the given row.
  virtual void getArrayStringV   (uInt rownr, Array<String>* dataPtr);
  
  // Put an array value in the given row.
  virtual void putArrayStringV   (uInt rownr, const Array<String>* dataPtr);

  // Set the shape of the array in the given row and allocate the array
  // in the file.
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

#endif




