//# ScaRecordColDesc.h: Class for description of table scalar record columns
//# Copyright (C) 1998,2000
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

#ifndef TABLES_SCARECORDCOLDESC_H
#define TABLES_SCARECORDCOLDESC_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/BaseColDesc.h>
#include <casacore/casa/Containers/SimOrdMap.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class PlainColumn;
class ColumnSet;


// <summary>
// Class to define columns of scalar records in tables
// </summary>

// <reviewed reviewer="Wim Brouw" date="1998/12/09" tests="tTableDesc">
// </reviewed>

// <use visibility=export>

// <prerequisite>
//   <li> <linkto class=ColumnDesc>ColumnDesc</linkto>
//   <li> <linkto class=TableDesc>TableDesc</linkto>
//   <li> <linkto class=TableRecord>TableRecord</linkto>
// </prerequisite>

// <etymology>
//  This class builds descriptions of table columns where each cell (which
//  may also be called a row) will hold a scalar record value.
// </etymology>

// <synopsis> 
// ScalarRecordColumnDesc is the class for defining a
// table column containing scalar record values. The only record class
// supported is <linkto class=TableRecord>TableRecord</linkto>.
// <br>
// This class is similar to the templated class
// <linkto class=ScalarColumnDesc>ScalarColumnDesc</linkto> used
// to define column descriptions for scalars with a standard data type.
// <p>
// The data managers handle a record as an indirect Vector of uChar,
// because class
// <linkto class=ScalarRecordColumnData>ScalarRecordColumnData</linkto>
// converts a record to such a vector before passing it to the data manager.
// <p>
// This class is derived from
// <linkto class=BaseColumnDesc>BaseColumnDesc</linkto>, thus the functions
// in there also apply to this class.
// <br>
// Once a column description is setup satisfactorily, it must be added
// to a table description before it can be used by the table system.
// </synopsis>

// <example>
// <srcblock>
//     TableDesc tabDesc("tTableDesc", "1", TableDesc::New);
//
//     // Add a scalar integer column ac, define keywords for it
//     // and define a default value 0.
//     ScalarRecordColumnDesc<Int> acColumn("ac");
//     acColumn.rwKeywordSet().define ("scale", Complex(0));
//     acColumn.rwKeywordSet().define ("unit", "");
//     acColumn.setDefault (0);
//     tabDesc.addColumn (acColumn);
//
//     // Add another column, now with data type String..
//     // This can be added directly, because no special things like
//     // keywords or default values have to be set.
//     tabDesc.addColumn (ScalarRecordColumnDesc<String>("name", "comments"));
// </srcblock>
// </example>

// <motivation>
// This class resembles the templated class
// <linkto class=ScalarColumnDesc>ScalarColumnDesc</linkto>
// a lot, but is different enough to make that templated class not usable
// for records.
// <br>In principle it could have been a template specialization,
// but not all compilers support specializations so well.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//  <li> Introduce a class ArrayRecordColumnDesc to support arrays of records.
// </todo>


class ScalarRecordColumnDesc : public BaseColumnDesc
{
friend class ColumnDesc;

public:
    // Construct the column with the given name.
    // The data manager type defaults to the StandardStMan storage manager.
    // The data manager group defaults to the data manager type.
    explicit ScalarRecordColumnDesc (const String& name);

    // Construct the column with the given name and comment.
    // The data manager type defaults to the StandardStMan storage manager.
    // The data manager group defaults to the data manager type.
    ScalarRecordColumnDesc (const String& name, const String& comment);

    // Construct the column with the given name, comment, and
    // default data manager type and group.
    // A blank data manager group defaults to the data manager type.
    ScalarRecordColumnDesc (const String& name, const String& comment,
			    const String& dataManName,
			    const String& dataManGroup);

    // Copy constructor (copy semantics);
    ScalarRecordColumnDesc (const ScalarRecordColumnDesc&);

    ~ScalarRecordColumnDesc();

    // Assignment (copy semantics);
    ScalarRecordColumnDesc& operator= (const ScalarRecordColumnDesc&);

    // Clone this column description.
    virtual BaseColumnDesc* clone() const;

    // Get the name of this class. It is used by the registration process.
    virtual String className() const;

    // Create a Column object out of this.
    // This is used by class ColumnSet to construct a table column object.
    virtual PlainColumn* makeColumn (ColumnSet*) const;

    // Show the column.
    virtual void show (ostream& os) const;

    // Create the object from AipsIO (this function is registered
    // by ColumnDesc.cc).
    static BaseColumnDesc* makeDesc (const String& name);

private:
    // Put the object.
    virtual void putDesc (AipsIO&) const;

    // Get the object.
    virtual void getDesc (AipsIO&);
};



} //# NAMESPACE CASACORE - END

#endif
