//# MSTableImpl.h: Helper class to implement common functions for MS Tables
//# Copyright (C) 1996,2000,2001,2002
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

#ifndef MS_MSTABLEIMPL_H
#define MS_MSTABLEIMPL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/casa/Utilities/Fallible.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class SetupNewTable;

// <summary> 
// An implementation class for the MeasurementSet to share code.
// </summary>

// <use visibility=local>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">

// <prerequisite>
//   <li> <linkto class=MeasurementSet:description">MeasurementSet</linkto>
// </prerequisite>
//
// <etymology>
// The MSTableImpl implements non-templated static functions shared by
// all MSTable objects.
// </etymology>
//
// <synopsis> 
// MSTableImpl is only for internal use by the MeasurementSet base class
// MSTable.
// </synopsis> 
//
// <example>
// </example>
//     
//
// <motivation>
// The reasons for existance for this class are: sharing of code between
// the various MeasurementSet Tables and avoiding duplicate code in the
// instantiations of MSTable
// </motivation>
//
// <todo asof="1996/2/22">
// </todo>

class MSTableImpl 
{
public:
    // Convert a name to a ColEnum, 
    static Int mapType(const SimpleOrderedMap<Int,String>& map,
		       const String &name);

    // add a column to a TableDesc
    // An exception is thrown for an invalid data type.  This indicates a 
    // programming error in this class when this occurs.
    // If option!=0 shape is used to set the shape of the column and
    // option defines the type of column (ColumnDesc::Fixed/Direct).
    // If option==0, shape is ignored and ndim is used to specify the 
    // array dimension if any.
    //  If refCol is not empty, a column with variable reference will be
    // created, note that refCol should already exist in td.
    // <thrown>
    //   <li> AipsError
    // </thrown>
    static void addColumnToDesc(TableDesc &td, const String& colName,
				Int colDType, const String& colComment,
				const String& colUnit,
				const String& colMeasure, 
				Int ndim, const IPosition & shape,
				Int option, const String& refCol);

    // add a keyword to a TableDesc
    // An exception is thrown for an invalid data type.  This indicates a 
    // missing data type in the code..
    // <thrown>
    //   <li> AipsError
    // </thrown>
    static void addKeyToDesc(TableDesc& td, const String& keyName,
			     Int keyDType, const String& keyComment);

    // add a MeasureColumn for the specified Measure, with default reference
    static void addMeasColumn(TableDesc &td, const String& colName,
			      const String& colMeasure, const String& refCol);

    // Add the compress option for the given column to the TableDesc.
    static void addColumnCompression (TableDesc&, const String& colName,
				      Bool autoScale, const String& type);

    // Setup the compression data managers if needed.
    static SetupNewTable& setupCompression (SetupNewTable&);

    // Define an entry in the column maps
    static void colMapDef(SimpleOrderedMap<Int,String>& colMap,
			  SimpleOrderedMap<Int,Int>& colDTypeMap,
			  SimpleOrderedMap<Int,String>& colCommentMap,
			  SimpleOrderedMap<Int,String>& colUnitMap,
			  SimpleOrderedMap<Int,String>& colMeasureTypeMap,
			  Int col,
			  const String& colName,
			  Int colType,
			  const String& colComment,
			  const String& colUnit,
			  const String& colMeasureType);

    // Define an entry in the keyword maps
    static void keyMapDef(SimpleOrderedMap<Int,String>& keyMap,
			  SimpleOrderedMap<Int,Int>& keyDTypeMap,
			  SimpleOrderedMap<Int,String>& keyCommentMap,
			  Int key,
			  const String& keyName,
			  Int keyType,
			  const String& keyComment);

    // tableDesc convenience functions
    // <group>
 
    // check that a TableDesc is valid 
    static Bool validate(const TableDesc& tabDesc, 
			 const TableDesc& requiredTD);
 
    // check that the keyword set is valid 
    static Bool validate(const TableRecord& tabRec, 
			 const TableDesc& requiredTD);
 
    // </group>

    // Return a table that references all columns in this table except for
    // those given in writableColumns, those are empty and writable.
    static Table referenceCopy(const Table& tab, const String& newTableName, 
			       const Block<String>& writableColumns);
    // Initialize all MeasurementSet static mappings
    static void init();
    
private:
    static Bool initialized_p;
};


} //# NAMESPACE CASACORE - END

#endif
