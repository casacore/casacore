//# ArrColDesc.h: Templated class to describe columns of arrays in tables
//# Copyright (C) 1994,1995,1996,1997,1999,2000
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

#ifndef TABLES_ARRCOLDESC_H
#define TABLES_ARRCOLDESC_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/BaseColDesc.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/Arrays/IPosition.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class PlainColumn;
class ColumnSet;
template<class T> class Array;

// <summary>
// Templated class for description of table array columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <prerequisite>
//   <li> BaseColumnDesc (and its prerequisites)
//   <li> TableDesc
// </prerequisite>

// <etymology>
// This class builds descriptions of table columns where each cell (which
// may also be called a row) will hold an array.
// </etymology>

// <synopsis> 
// ArrayColumnDesc is a templated class for defining a table column
// containing arrays.
//
// The table values are handled by a data manager. This can be
// a storage manager to store the values in a file or it can be
// a virtual column engine to calculate them on-the-fly.
// Only the basic data types are allowed when storing in a file. These are:
//  Bool, uChar, Short, uShort, Int, uInt, float, double,
//  Complex, DComplex and String.
//
// At table creation time (when a table gets created from a table
// description), each column needs to be bound to a data manager.
// If not done explicitly, the table system will bind a column to the
// default manager defined in the column description.
//
// An array column description consists of the following attributes:
// <ul>
//  <li> Name, which has to be unique and must also be different
//         from possible table keyword names.
//  <li> Data type, which is determined by the template parameter
//         (e.g. ArrayColumnDesc<Int>).
//  <li> A data type id, which tells the unique name of non-standard
//         data types (i.e. for data type == TpOther).
//  <li> Comment, which defaults to the empty string.
//         This serves purely as an informational string for the user.
//  <li> Dimensionality. If given, all arrays in the column need
//         to have that dimensionality.
//  <li> Shape. If given, all arrays in the column need to have
//         that shape.
//  <li> Default data manager, which will be used if a column
//         for a newly created table is not explicitly bound to a
//         datamanager.
//  <li> Data manager group, which serves 2 purposes.
//         Firstly it can be used in class SetupNewTable to bind a group
//         of columns.
//         Secondly, when the default data managers are used, it
//         allows, for example, to have 2 AipsIO storage managers.
//         One for one group of columns and one for another group of columns.
//  <li> Options. These are defined in ColumnDesc.h and can be combined
//       by logically or-ing them.
//       <ol>
//        <li>
//         ColumnDesc::FixedShape says that the arrays in all cells
//         of a column have the same shape. This shape must be defined
//         before a table is created. It does not tell if
//         the array is direct or indirect.
//         A FixedShape array is defined in every cell, while for
//         non-FixedShape arrays a cell can be empty.
//        <li>
//         ColumnDesc::Direct determines if an array is directly
//         stored in the table or if it is stored indirectly in a separate
//         file. Direct arrays enforce the FixedShape option.
//         Usually indirect arrays are only read in on command, while
//         direct arrays are held in memory. So the size of the
//         arrays is an important factor.
//       </ol>
//  <li> Default keyword set, which defaults to an empty set.
//         When a table column gets created from the description, it gets
//         a copy of this keyword set as its initial keyword set.
// </ul>
//
// There are several constructors, which allow the definition of most
// of the above mentioned attributes. Others, like the default keyword
// set, have to be defined explicitly.
//
// This class is derived from BaseColumnDesc, thus the functions
// in there also apply to this class.
//
// Once a column description is set up satisfactorily, it must be added
// to a table description before it can be used by the table system.
// </synopsis>

// <example>
// <srcblock>
//     TableDesc tabDesc("tTableDesc", "1", TableDesc::New);
//
//     // Now define array columns.
//     // This one is indirect and has no dimensionality mentioned yet.
//     // Define the keyword UNIT in it.
//     ArrayColumnDesc<Complex> arr1Column("Arr1", "comment for Arr1");
//     arr1Column.rwKeywordSet().define ("UNIT", "Jy");
//     tabDesc.addColumn (arr1Column);
//
//     // This one is indirect and has 3-dim arrays.
//     tabDesc.addColumn (ArrayColumnDesc<Int>("Arr2",
//                                             "comment for Arr2",
//                                             3));
//     // This one is direct and has 2-dim arrays with axis lengths 4 and 7.
//     tabDesc.addColumn (ArrayColumnDesc<uInt>("Arr3",
//                                              "comment for Arr1",
//        				        IPosition(2,4,7),
// 					        ColumnDesc::Direct));
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

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>

template<class T>
class ArrayColumnDesc : public BaseColumnDesc
{
friend class ColumnDesc;

public:
    // Construct the column with the given name and dimensionality.
    // The data manager type defaults to the StandardStman storage manager.
    // The data manager group defaults to the data manager type.
    // Ndim <=0 means that the number of dimensions is free and will
    // be defined when creating the table (rows). Ndim>0 means that
    // the arrays in this column must have the given dimensionality.
    // The possible options are defined in ColumnDesc.h.
    explicit ArrayColumnDesc (const String& name, Int ndim = -1,
			      int options = 0);

    // Construct the column with the given name, dimensionality, and comment.
    // The data manager type defaults to the StandardStman storage manager.
    // The data manager group defaults to the data manager type.
    // Ndim <=0 means that the number of dimensions is free and will
    // be defined when creating the table (rows). Ndim>0 means that
    // the arrays in this column must have the given dimensionality.
    // The possible options are defined in ColumnDesc.h.
    ArrayColumnDesc (const String& name, const String& comment,
		     Int ndim = -1, int options = 0);

    // Construct the column with the given name, dimensionality, comment,
    // and default data manager type and group.
    // A blank data manager group defaults to the data manager type.
    // Ndim <=0 means that the number of dimensions is free and will
    // be defined when creating the table (rows). Ndim>0 means that
    // the arrays in this column must have the given dimensionality.
    // The possible options are defined in ColumnDesc.h.
    ArrayColumnDesc (const String& name, const String& comment,
		     const String& dataManName, const String& dataManGroup,
		     Int ndim = -1, int options = 0);

    // Construct the column with the given name and shape.
    // The data manager type defaults to the StandardStman storage manager.
    // The data manager group defaults to the data manager type.
    // The possible options are defined in ColumnDesc.h.
    // This constructor can only be used for FixedShape arrays, because the
    // shape of other arrays can only be set per row.
    ArrayColumnDesc (const String& name,
		     const IPosition& shape, int options = 0);

    // Construct the column with the given name, shape, and comment.
    // The data manager type defaults to the StandardStman storage manager.
    // The data manager group defaults to the data manager type.
    // The possible options are defined in ColumnDesc.h.
    // This constructor can only be used for FixedShape arrays, because the
    // shape of other arrays can only be set per row.
    ArrayColumnDesc (const String& name, const String& comment,
		     const IPosition& shape, int options = 0);

    // Construct the column with the given name, shape, comment,
    // and default data manager type and group.
    // A blank data manager group defaults to the data manager type.
    // The possible options are defined in ColumnDesc.h.
    // This constructor can only be used for FixedShape arrays, because the
    // shape of other arrays can only be set per row.
    // If both ndim and shape are given as > 0, ndim should match the length
    // of shape.
    ArrayColumnDesc (const String& name, const String& comment,
		     const String& dataManName, const String& dataManGroup,
		     const IPosition& shape, int options = 0, int ndim=-1);

    // Copy constructor (copy semantics);
    ArrayColumnDesc (const ArrayColumnDesc<T>&);

    ~ArrayColumnDesc();

    // Assignment (copy semantics);
    ArrayColumnDesc<T>& operator= (const ArrayColumnDesc<T>&);

    // Clone this column description to another.
    BaseColumnDesc* clone() const;

    // Get the name of this class. It is used by the registration process.
    // The template argument gets part of the name.
    String className() const;

    // Create a Column object out of this.
    // This is used by class ColumnSet to construct a table column object.
    virtual PlainColumn* makeColumn (ColumnSet*) const;

    // Show the column.
    void show (ostream& os) const;

    // Register the construction function of this class.
    void registerClass() const;

    // Create the object from AipsIO (this function is registered).
    static BaseColumnDesc* makeDesc(const String& name);

protected:
    // Put the object.
    virtual void putDesc (AipsIO&) const;

    // Get the object.
    virtual void getDesc (AipsIO&);
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/Tables/ArrColDesc.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
