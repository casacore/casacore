//# ScaColDesc.h: Templated class for description of table scalar columns
//# Copyright (C) 1994,1995,1996,1997,1998,2000
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

#ifndef TABLES_SCACOLDESC_H
#define TABLES_SCACOLDESC_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/BaseColDesc.h>
#include <casacore/casa/Containers/SimOrdMap.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class PlainColumn;
class ColumnSet;


// <summary>
// Templated class to define columns of scalars in tables
// </summary>

// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <use visibility=export>

// <prerequisite>
//   <li> BaseColumnDesc
//   <li> TableDesc
// </prerequisite>

// <etymology>
//  This class builds descriptions of table columns where each cell (which
//  may also be called a row) will hold a scalar value.
// </etymology>

// <synopsis> 
// ScalarColumnDesc is a templated class for defining a
// table column containing scalar values.
// Note that class
// <linkto class=ScalarRecordColumnDesc>ScalarRecordColumnDesc</linkto>
// has to be used to define the description of a column containing records.
// <p>
// The table values are handled by a data manager. This can be
// a storage manager to store the values in a file or it can be
// a virtual column engine to calculate them on-the-fly.
// Only the basic data types are allowed when storing in a file. These are:
//  Bool, uChar, Short, uShort, Int, uInt, float, double,
//  Complex, DComplex and String.
// <p>
// At table creation time (when a table gets created from a table
// description), each column needs to be bound to a data manager.
// If not done explicitly, the table system will bind a column to the
// default data manager defined in the column description.
// <p>
// A scalar column description consists of the following attributes:
// <ul>
//  <li> Name, which has to be unique and must also be different
//         from possible table keyword names.
//  <li> Data type, which is determined by the template parameter
//         (e.g. ArrayColumnDesc<Int>).
//  <li> A data type id, which tells the unique name of non-standard
//         data types (i.e. for data type == TpOther).
//  <li> Comment, which defaults to an empty string.
//         This serves purely as an informational string for the user.
//  <li> Default value, which is only possible for the standard data types.
//         It defaults to the undefined value defined in ValType.h.
//         When a row gets added to a table, it is possible to
//         initialize the column fields in the row with this default value.
//  <li> Default data manager, which will be used if a column
//         for a newly created table is not explicitly bound to a
//         data manager.
//  <li> Data manager group, which serves 2 purposes.
//         Firstly it can be used in class SetupNewTable to bind a group
//         of columns.
//         Secondly, when the default data managers are used, it
//         allows, for example, to have 2 StandardStMan storage managers.
//         One for one group of columns and one for another group of columns.
//  <li> Options. These are defined in ColumnDesc.h and can be combined
//         by or-ing them.
//         Currently only the Undefined flag applies to scalars.
//  <li> Default keyword set, which defaults to an empty set.
//         When a table column gets created from the description, it gets
//         a copy of this keyword set as its initial keyword set.
// </ul>
//
// There are several constructors, which allow to define most
// of the above mentioned attributes. Others, like the default keyword
// set, have to be defined explicitly.
// <p>
// This class is derived from BaseColumnDesc, thus the functions
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
//     ScalarColumnDesc<Int> acColumn("ac");
//     acColumn.rwKeywordSet().define ("scale", Complex(0));
//     acColumn.rwKeywordSet().define ("unit", "");
//     acColumn.setDefault (0);
//     tabDesc.addColumn (acColumn);
//
//     // Add another column, now with data type String..
//     // This can be added directly, because no special things like
//     // keywords or default values have to be set.
//     tabDesc.addColumn (ScalarColumnDesc<String>("name", "comments"));
// </srcblock>
// </example>

// <motivation>
// Several column description classes are needed to allow the user
// to define attributes which are special for each column type.
// For scalars the special attribute is the default value.
// They all have to be templated to support arbitrary data types.
// </motivation>

// <templating arg=T>
//  <li> Default constructor
//  <li> Copy constructor
//  <li> Assignment operator
//  <li> <src>static String dataTypeId();  // (not needed for builtin types)</src>
//       This should return the unique "name" of the class.
// </templating>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


template<class T>
class ScalarColumnDesc : public BaseColumnDesc
{
friend class ColumnDesc;

public:
    // Construct the column with the given name.
    // The data manager type defaults to the StandardStMan storage manager.
    // The data manager group defaults to the data manager type.
    // The possible options are defined in ColumnDesc.h.
    explicit ScalarColumnDesc (const String& name, int options = 0);

    // Construct the column with the given name and comment.
    // The data manager type defaults to the StandardStMan storage manager.
    // The data manager group defaults to the data manager type.
    // The possible options are defined in ColumnDesc.h.
    ScalarColumnDesc (const String& name, const String& comment,
		      int options = 0);

    // Construct the column with the given name, comment, and
    // default data manager type and group.
    // A blank data manager group defaults to the data manager type.
    // The possible options are defined in ColumnDesc.h.
    ScalarColumnDesc (const String& name, const String& comment,
		      const String& dataManName, const String& dataManGroup,
		      int options = 0);

    // Construct the column with the given name, comment, default
    // data manager type and group, and default value.
    // A blank data manager group defaults to the data manager type.
    // The possible options are defined in ColumnDesc.h.
    ScalarColumnDesc (const String& name, const String& comment,
		      const String& dataManName, const String& dataManGroup,
		      const T& defaultValue, int options = 0);

    // Copy constructor (copy semantics);
    ScalarColumnDesc (const ScalarColumnDesc<T>&);

    ~ScalarColumnDesc();

    // Assignment (copy semantics);
    ScalarColumnDesc<T>& operator= (const ScalarColumnDesc<T>&);

    // Clone this column description.
    BaseColumnDesc* clone() const;

    // Get the name of this class. It is used by the registration process.
    // The template argument gets part of the name.
    String className() const;

    // Set the default value.
    void setDefault (const T& defaultValue)
	{ defaultVal_p = defaultValue; }

    // Get the default value.
    const T& defaultValue() const
	{ return defaultVal_p; }

    // Create a Column object out of this.
    // This is used by class ColumnSet to construct a table column object.
    virtual PlainColumn* makeColumn (ColumnSet*) const;

    // Make a ConcatColumn object out of the description.
    virtual ConcatColumn* makeConcatColumn (ConcatTable*) const;

    // Show the column.
    void show (ostream& os) const;

    // Register the construction function of this class.
    void registerClass() const;

    // Create the object from AipsIO (this function is registered).
    static BaseColumnDesc* makeDesc (const String& name);

private:
    T  defaultVal_p;                        //# default value

    // Put the object.
    virtual void putDesc (AipsIO&) const;

    // Get the object.
    virtual void getDesc (AipsIO&);
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/Tables/ScaColDesc.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
