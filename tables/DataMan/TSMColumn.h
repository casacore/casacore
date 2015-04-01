//# TSMColumn.h: A column in the Tiled Storage Manager
//# Copyright (C) 1995,1996,1997,1999
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

#ifndef TABLES_TSMCOLUMN_H
#define TABLES_TSMCOLUMN_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/StManColumn.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TiledStMan;
class TSMDataColumn;
class TSMCoordColumn;
class TSMIdColumn;


// <summary>
// A column in the Tiled Storage Manager
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=StManColumn>StManColumn</linkto>
//   <li> <linkto class=TiledStMan>TiledStMan</linkto>
// </prerequisite>

// <etymology>
// TSMColumn handles a column for the Tiled Storage Manager.
// </etymology>

// <synopsis> 
// TSMColumn serves 2 purposes:
// <ol>
//  <li> It is the initial object for all columns in TiledStMan.
//  <li> It serves as a base class for the specialized TiledStMan
//       column classes dealing with data, coordinates and id values.
// </ol>
// The protocol used for creating the derived
// <linkto class=TSMDataColumn>TSMDataColumn</linkto>,
// <linkto class=TSMCoordColumn>TSMCoordColumn</linkto>, and
// <linkto class=TSMIdColumn>TSMIdColumn</linkto>
// objects is somewhat complicated. It works as follows:
// <br>
// When the table is set up, a TSMColumn object gets created for all
// columns in a TiledStMan storage manager. The TiledStMan initialization
// function lets each TSMColumn object create its specialized TSMXXColumn
// object (using make{Coord,Id,Data}Column). At the end of the setup process
// the TSMColumn objects are deleted and the DataManagerColumn pointers
// in the BaseColumn objects get replaced by those to the specialized
// objects. In that way no needless virtual function calls are done.
// </synopsis> 

// <motivation>
// TSMColumn is needed for the initial DataManagerColumn setup process.
// It is also useful as a base class for all TiledStMan column objects.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TSMColumn : public StManColumn
{
public:

    // Create a column of the given type.
    // It will maintain a pointer to its parent storage manager.
    TSMColumn (TiledStMan* stman, int dataType, const String& columnName);

    // Frees up the storage.
    virtual ~TSMColumn();

    // Get the name of the column.
    const String& columnName() const;

    // Return the data type of the column.
    virtual int dataType() const;

    // Set the fixed shape of the column.
    void setShapeColumn (const IPosition& shape);

    // Get the fixed shape of the column.
    const IPosition& shapeColumn() const;

    // Make a TSM data column object.
    // Add the pixel length to the total data pixel length.
    TSMDataColumn* makeDataColumn();

    // Make a TSM coordinate column object.
    TSMCoordColumn* makeCoordColumn (uInt axesNumber);

    // Make a TSM id column object.
    TSMIdColumn* makeIdColumn();

    // Unlink the underlying column.
    // It clears the pointer and returns its original value.
    // This is used to get a pointer directly to the underlying TSMXXColumn
    // object in the BaseColumn classes. In that way only 1 instead
    // of 2 virtual function calls are needed for a get or put.
    TSMColumn* unlink();


protected:
    // The storage manager.
    TiledStMan* stmanPtr_p;
    // The data type of the data (as defined in DataType.h).
    int   dtype_p;
    // The name of the column.
    String name_p;
    // The fixed shape of the column.
    IPosition columnShape_p;
    // The specialized column object (i.e. data, coordinate or id).
    TSMColumn* colPtr_p;

    // The copy constructor can only be used to copy a derived class.
    TSMColumn (const TSMColumn& that);

private:
    // Forbid assignment.
    TSMColumn& operator= (const TSMColumn&);
};


inline const String& TSMColumn::columnName() const
    { return name_p; }

inline const IPosition& TSMColumn::shapeColumn() const
    { return columnShape_p; }




} //# NAMESPACE CASACORE - END

#endif
