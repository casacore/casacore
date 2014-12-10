//# BaseColDesc.h: an abstract base class for column descriptions
//# Copyright (C) 1994,1995,1996,1997,1999,2000,2001
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

#ifndef TABLES_BASECOLDESC_H
#define TABLES_BASECOLDESC_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class AipsIO;
class ColumnDescSet;
class TableRecord;
class TableAttr;
class BaseColumn;
class PlainColumn;
class RefTable;
class RefColumn;
class ConcatTable;
class ConcatColumn;
class TableDesc;
class ColumnSet;


// <summary>
// An abstract base class for table column descriptions
// </summary>

// <use visibility=local>

// <reviewed reviewer="Paul Shannon" date="1994/08/11" tests="none">
// </reviewed>

// <prerequisite>
//   <li> Tables module (see especially Tables.h, the module header file)
//   <li> The description of letter/envelope class design, and its 
//          application to the family of classes of which BaseColumnDesc
//          is a part, in ColumnDesc.h
//   <li> TableDesc
// </prerequisite>

// <etymology>
//    "Base" indicates that this is a base class for the specialized
//    column description classes.
// </etymology>

// <synopsis> 
// BaseColumnDesc is an abstract class describing a column in a table.
// Various XXXXColumnDesc classes are derived from it to describe
// the various types of columns, among them ArrayColumnDesc<T>
// and ScalarcolumnDesc<T>. These derived classes are used to
// construct a column description which can be added to the TableDesc.
//
// BaseColumnDesc contains functions common to all kinds of column
// descriptions.
// It contains a TableRecord to attach simple keywords to the
// column description (e.g. to define a scale-factor). This keyword set
// serves as the initial keyword set for the column when a table
// gets instantiated from a table description.
//
// The ColumnDesc class is an envelope around BaseColumnDesc, which
// can be used to get information about existing column descriptions.
// </synopsis>

// <motivation>
// This abstract base class defines the common features required of all 
// concrete column description classes.  It also provides the hook (for 
// letter objects) required by ColumnDesc, the envelope class.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


class BaseColumnDesc
{
//# Allow ColumnDesc to call the private functions checkAdd, etc..
friend class ColumnDesc;   

public:
    // Construct the column base object.
    BaseColumnDesc (const String& name, const String& comment,
		    const String& dataManagerType,
		    const String& dataManagerGroup,
		    DataType, const String& dataTypeId,
		    Int options, uInt ndim, const IPosition& shape,
		    Bool isScalar, Bool isArray, Bool isTable);

    // Copy constructor (copy semantics).
    BaseColumnDesc (const BaseColumnDesc&);

    virtual ~BaseColumnDesc ();

    // Get access to the set of keywords.
    // <group>
    TableRecord& rwKeywordSet()
	{ return *keySetPtr_p; }
    const TableRecord& keywordSet() const
	{ return *keySetPtr_p; }
    // </group>

    // Show the column.
    virtual void show (ostream& os) const = 0;

    // Get the name of the column.
    const String& name() const
	{ return colName_p; }

    // Get the data type of the column.
    DataType dataType() const
	{ return dtype_p; }

    // Get the type id for non-standard data types (i.e. for TpOther).
    // For standard data types the returned string is empty.
    const String& dataTypeId() const
	{ return dtypeId_p; }

    // Get the type name of the default data manager.
    const String& dataManagerType() const
	{ return dataManType_p; }

    // Get the type name of the default data manager.
    // (allowing it to be changed)
    String& dataManagerType()
	{ return dataManType_p; }

    // Get the data manager group.
    const String& dataManagerGroup() const
	{ return dataManGroup_p; }

    // Get the data manager group.
    // (allowing it to be changed)
    String& dataManagerGroup()
	{ return dataManGroup_p; }

    // Set the data manager type and group to the default.
    // If <src>always==True</src> they are always set, otherwise only if empty.
    void setDefaultDataManager (Bool always);

    // Get comment string.
    const String& comment() const
	{ return comment_p; }

    // Get comment string (allowing it to be changed).
    String& comment()
	{ return comment_p; }

    // Get the options.
    Int options() const
	{ return option_p; }

    // Test if column is scalar, array or table.
    // <group>
    Bool isScalar() const
	{ return isScalar_p; }
    Bool isArray() const
	{ return isArray_p; }
    Bool isTable() const
	{ return isTable_p; }
    // </group>

    // Get the number of dimensions.
    Int ndim() const
	{ return nrdim_p; }

    // Get the predefined shape.
    // If not defined, a zero shape will be returned.
    const IPosition& shape() const
	{ return shape_p; }

    // Set the number of dimensions.
    // This is only allowed for arrays.
    // <src>ndim</src> can be zero to clear the number of dimensions
    // and the shape.
    // Otherwise it can only be used if the dimensionality has not been
    // defined yet.
    void setNdim (uInt ndim);

    // Set the predefined shape.
    // This is only allowed for arrays, for which the shape
    // has not been defined yet.
    // If the dimensionality has already been defined, it must match.
    // It will set the option FixedShape if not set yet.
    // <br> The first version leaves the <src>Direct</src> option as is.
    // The second version sets the <src>Direct</src> option as given.
    // <group>
    void setShape (const IPosition& shape);
    void setShape (const IPosition& shape, Bool directOption);
    // </group>

    // Set the options to the given value.
    // Option <src>ColumnDesc::Direct</src> forces <src>FixedShape</src>.
    // If <src>FixedShape</src> is not given (implicitly or explicitly),
    // the column can have no shape, so its shape is cleared.
    void setOptions (Int options);

    // Get the maximum value length.
    uInt maxLength() const
	{ return maxLength_p; }

    // Set the maximum value length.
    // So far, this is only possible for columns containing String values.
    // An exception is thrown if the column data type is not TpString.
    // Some storage managers support fixed length strings and can store
    // them more efficiently than variable length strings.
    void setMaxLength (uInt maxLength);

    // Get table description (in case column contains subtables).
    // <group>
    //# Use the non-const version to implement the const one.
    //# The cast is fully safe, because it returns a const object.
    const TableDesc* tableDesc() const
	{ return ((BaseColumnDesc*)this)->tableDesc(); }
    virtual TableDesc* tableDesc();
    // </group>

protected:
    String         colName_p;            //# column name
    String         comment_p;            //# comment
    String         dataManType_p;        //# default data manager type
    String         dataManGroup_p;       //# data manager group
    DataType       dtype_p;              //# datatype
    String         dtypeId_p;            //# datatype id for TpOther
    Int            option_p;             //# column options
    Int            nrdim_p;              //# #dimensions (<0 = unknown)
    IPosition      shape_p;              //# table array shape
    uInt           maxLength_p;          //# maximum value length (for strings)
    TableRecord*   keySetPtr_p;          //# set of keywords
    Bool           isScalar_p;           //# True = column contains scalars
    Bool           isArray_p;            //# True = column contains arrays
    Bool           isTable_p;            //# True = column contains tables

    // Assignment (copy semantics).
    BaseColumnDesc& operator= (const BaseColumnDesc&);

    // Put the object.
    // It uses putDesc to put the derived object.
    void putFile (AipsIO&, const TableAttr&) const;

    // Get the object.
    // It uses getDesc to get the derived object.
    void getFile (AipsIO&, const TableAttr&);

    // Put the derived object.
    virtual void putDesc (AipsIO&) const = 0;

    // Get the derived object.
    virtual void getDesc (AipsIO&) = 0;

    // Make a PlainColumn object out of the description.
    virtual PlainColumn* makeColumn (ColumnSet*) const = 0;

    // Make a RefColumn object out of the description.
    RefColumn* makeRefColumn (RefTable*, BaseColumn*) const;

    // Make a ConcatColumn object out of the description.
    // The default makes a ConcatColumn object.
    // Derived classes (ScalarColumnDesc) can make more specialized objects.
    virtual ConcatColumn* makeConcatColumn (ConcatTable*) const;

private:
    // Check if a column can be handled by ColumnDescSet.
    // This gives, for instance, the virtual column class the opportunity
    // to check if the used columns exist.
    // <group>
    virtual void checkAdd (const ColumnDescSet& cds) const;
    virtual void checkRename (const ColumnDescSet& cds,
			      const String& newName) const;
    // </group>

    // Take action after a column has been handled by ColumnDescSet.
    // This gives, for instance, the virtual column class the opportunity
    // to update the virtual column list.
    // <group>
    virtual void handleAdd (ColumnDescSet& cds);
    virtual void handleRename (ColumnDescSet& cds, const String& oldName);
    virtual void handleRemove (ColumnDescSet& cds);
    // </group>

    // This function allows each column to act upon a rename of another column.
    // If the old name is used internally, the column can update itself.
    virtual void renameAction (const String& newName, const String& oldName);

public:
    // Clone a column description (creating a new column description object).
    virtual BaseColumnDesc* clone() const = 0;

    // Get the underlying class name.
    virtual String className() const = 0;

    // Set the name of the column (for a rename).
    void setName (const String& name)
	{ colName_p = name; }
};




} //# NAMESPACE CASACORE - END

#endif
