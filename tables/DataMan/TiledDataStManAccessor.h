//# TiledDataStManAccessor.h: Gives access to some TiledDataStMan functions
//# Copyright (C) 1994,1995,1996,1997,1999
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

#ifndef TABLES_TILEDDATASTMANACCESSOR_H
#define TABLES_TILEDDATASTMANACCESSOR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/TiledStManAccessor.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TiledDataStMan;
class Table;
class String;
class IPosition;
class Record;


// <summary>
// Give access to some TiledDataStMan functions
// </summary>

// <use visibility=export>

// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
// <li> <linkto class=ROTiledStManAccessor>ROTiledStManAccessor</linkto>
// <li> <linkto class=TiledDataStMan>TiledDataStMan</linkto>
// </prerequisite>

// <synopsis>
// The Table system has one or more storage managers underneath.
// These storage managers are invisible and there is no way to
// get access to them.
// However, the <linkto class=TiledDataStMan>TiledDataStMan</linkto>
// storage manager is quite specific. It has a few functions which
// need to be called by the user, in particular the functions
// defining and extending a hypercube.
// <p>
// The class TiledDataStManAccessor gives the user the means to
// access a TiledDataStMan object and to call the functions mentioned above.
// It can only be constructed for a table opened for read/write (because
// the functions in it need write access).
// </synopsis> 

// <motivation>
// In principle a pointer to TiledDataStMan could be used.
// However, that would give access to all public functions.
// Furthermore it could not distinguish between read/write and readonly
// tables. 
// </motivation>

// <example>
// <srcblock>
// // Open a table for write.
// Table table ("someName", Table::Update);
// // Get access to the tiled data storage manager with name UVdata.
// TiledDataStManAccessor accessor (Table, "UVdata");
// // Add a hypercube to it (requires addition of rows).
// // Define the values of the ID and coordinate columns.
// // (The coordinate vectors have to be filled in one way or another).
// Vector<double> timeVector(70);
// Vector<double> baselineVector(60);
// Vector<double> freqVector(50);
// Vector<double> polVector(4);
// Record values;
// values.define ("ID", 4);                 // ID=4
// values.define ("time", timeVector);
// values.define ("baseline", baselineVector);
// values.define ("freq", freqVector);
// values.define ("pol", polVector);
// table.addRow (4200);
// accessor.addHypercube (IPosition(4,4,50,60,70),    // cube shape
//                        IPosition(4,4,5,6,7),       // tile shape
//                        values);                    // id/coord values
// </srcblock>
// </example>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//  <li> A base class RO_Tiled(Data)StManAccessor may once be needed
//       for access to a tiled data storage manager in a readonly table
// </todo>



class TiledDataStManAccessor : public ROTiledStManAccessor
{
public:

    // Construct the object for the data manager in the table.
    // An exception is thrown if the data manager type does not
    // match the type of this TiledDataStManAccessor object.
    // Also an exception is thrown if the table is not open for read/write.
    TiledDataStManAccessor (const Table& table, const String& dataManagerName);
    TiledDataStManAccessor ();

    ~TiledDataStManAccessor();

    // Copy constructor (reference semantics).
    TiledDataStManAccessor (const TiledDataStManAccessor& that);

    // Assignment (reference semantics).
    TiledDataStManAccessor& operator= (const TiledDataStManAccessor& that);

    // Add a hypercube.
    // The possible coordinate- and id-values have to be given in the
    // record (where the field names should be equal to the
    // coordinate and id column names).
    void addHypercube (const IPosition& cubeShape,
		       const IPosition& tileShape,
		       const Record& values);

    // Extend the hypercube with the given number of elements in
    // the last dimension.
    // The record should contain the id values (to get the correct
    // hypercube) and coordinate values for the elements added.
    void extendHypercube (uInt incrInLastDim, const Record& values);


private:
    //# Declare the data members.
    TiledDataStMan* tiledDataManPtr_p;
};




} //# NAMESPACE CASACORE - END

#endif
