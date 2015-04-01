//# MSFlagCmd.h: The MeasurementSet FLAG_CMD Table
//# Copyright (C) 1999,2000
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

#ifndef MS_MSFLAGCMD_H
#define MS_MSFLAGCMD_H

#include <casacore/casa/aips.h>
#include <casacore/ms/MeasurementSets/MSTable.h>
#include <casacore/ms/MeasurementSets/MSFlagCmdEnums.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> 
// A Table intended to hold a MeasurementSet FLAG_CMD table.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">

// <prerequisite>
//   <li> <linkto class="MeasurementSet:description">MeasurementSet</linkto> 
//   <li> <linkto class="MSTable">MSTable</linkto> 
// </prerequisite>
//
// <etymology>
// MSFlagCmd stands for the MeasurementSet FLAG_CMD table.
// </etymology>
//
// <synopsis> 
// An MSFlagCmd is a table intended to hold the FLAG_CMD table for
// the MeasurementSet. It has an identical set of member functions as
// the main MeasurementSet class, except (currently) for the default
// calibration members. For further info and examples see the 
// MeasurementSet class.
// </synopsis> 
//
// <example>
// See the MeasurementSet for an example of how to access and use this class.
// </example>
//
// <motivation>
// It was found that subtables and the main table of the MeasurementSet have
// a lot in common, therefore they derive their interface from the same
// base class. Each subtable has its own class to keep the enum definitions
// and conversion functions in separate scopes.
// </motivation>
//
// <todo asof="1999/01/13">
// see MeasurementSet.
// </todo>

class MSFlagCmd:public MSFlagCmdEnums,
                public MSTable<MSFlagCmdEnums::PredefinedColumns,
		               MSFlagCmdEnums::PredefinedKeywords>
{
public:

    // This constructs an empty MSFlagCmd.
    MSFlagCmd ();

    // These constructors mirror the Table ones with additional checking
    // on validity (verifying that the MSFlagCmd will have the required columns
    // and keywords)
    // An exception is thrown if the constructed Table is not a valid MSFlagCmd
    // <thrown>
    //   <li> AipsError
    // </thrown>
    // <group name=tableLikeConstructors>
    MSFlagCmd (const String &tableName, TableOption = Table::Old);
    MSFlagCmd (const String &tableName, const String &tableDescName,
		    TableOption = Table::Old);
    MSFlagCmd (SetupNewTable &newTab, uInt nrrow = 0,
		    Bool initialize = False);
    MSFlagCmd (const Table &table);
    MSFlagCmd (const MSFlagCmd &other);
    // </group>

    // As with tables, the destructor writes the table if necessary.
    // Additional checking is done here to verify that all required
    // columns are still present.
    // If it is NOT valid, it will write the table and then throw an exception.
    // <thrown>
    //   <li> AipsError
    // </thrown>
    ~MSFlagCmd();

    //  Assignment operator, reference semantics
    MSFlagCmd& operator=(const MSFlagCmd&);

    // Make a special copy of this Table which references all columns from
    // this Table except those mentioned; those are empty and writable.
    // Each forwarded column has the same writable status as the underlying
    // column. The mentioned columns all use the AipsIO storage manager.
    // This function is inherited from MSTable and unlikely to be of use,
    // except in the class MeasurementSet (see comment there)..
    MSFlagCmd referenceCopy(const String& newTableName,
			  const Block<String>& writableColumns) const;

    // Initialize the statics appropriately. This does not need to be
    // called by users, it is called by the implementation class
    // MSTableImpl.
    static void init();

private:

    // required by the need to throw an exception in the destructor
    Bool hasBeenDestroyed_p;
};


} //# NAMESPACE CASACORE - END

#endif





