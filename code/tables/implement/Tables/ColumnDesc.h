//# ColumnDesc.h: an envelope class for column descriptions in tables
//# Copyright (C) 1994,1995,1996,1997,1998
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

#if !defined(AIPS_COLUMNDESC_H)
#define AIPS_COLUMNDESC_H


//# Includes
#include <aips/aips.h>
#include <aips/Tables/BaseColDesc.h>
#include <aips/Containers/SimOrdMap.h>
#include <aips/Utilities/String.h>
#include <aips/Lattices/IPosition.h>

//# Forward Declarations
class PlainColumn;
class RefColumn;
class TableRecord;
class AipsIO;
#if defined(AIPS_STDLIB)
#include <iosfwd.h>
#else
imported class ostream;
#endif


// <summary>
// Envelope class for the description of a table column
// </summary>

// <use visibility=export>

// <reviewed reviewer="Paul Shannon" date="1994/08/11" tests="none">
// </reviewed>

// <prerequisite>
//   <li> Tables module (see especially Tables.h, the module header file)
//   <li> Envelope/Letter class design (see J. Coplien, Advanced C++)
// </prerequisite>

// <synopsis>
// Class ColumnDesc is an envelope for the letter class BaseColDesc
// and its derivations like
// <linkto class="ScalarColumnDesc:description">ScalarColumnDesc</linkto>,
// <linkto class="ScalarRecordColumnDesc:description">
//  ScalarRecordColumnDesc</linkto>.
// <linkto class="ArrayColumnDesc:description">ArrayColumnDesc</linkto>, and
// <linkto class="SubTableDesc:description">SubTableDesc</linkto>.
// ColumnDesc is meant to examine or slightly modify already existing
// column descriptions.
// It allows the retrieval of attributes like name, data type, etc..
// For non-const ColumnDesc objects it is possible to modify the
// attributes comment and keyword set.
//
// Since there are several types of columns, the class ColumnDesc
// cannot handle all details of those column types. Therefore,
// to create a column description, an instance of the specialized
// classes ArrayColumnDesc<T>, etc. has to be constructed.
// In there column type dependent things like array shape and
// default value can be defined.
//
// This class also enumerates the possible options which can be used
// when defining a column via classes like ScalarColumnDesc<T>.
// These options are:
// <dl>
//  <dt> FixedShape
//  <dd>
//     This is only useful for columns containing arrays and tables.
//     FixedShape means that the shape of the array or table must
//     be the same in each cell of the column.
//     If not given, the array or table shape may vary.
//     Option Direct forces FixedShape.
//  <dt> Direct
//  <dd>
//     This is only useful for columns containing arrays and tables.
//     Direct means that the data is directly stored in the table.
//     Direct forces option FixedShape.
//     If not given, the array or table is indirect, which implies
//     that the data will be stored in a separate file.
//  <dt> Undefined
//  <dd>
//     Undefined is only useful for scalars. If not given, all possible
//     values of the scalar have a meaning. If given, a value equal to
//     the default value in the column description is an undefined value.
//     The function TableColumn::isDefined will return False for such
//     values.
// </dl>
// </synopsis>

// <example>
// <srcblock>
//  TableDesc tableDesc("theTableDesc", TableDesc::New);
//  // Add a float scalar column.
//  tableDesc.addColumn (ScalarColumnDesc<float> ("NAME");
//  // Get the description of a column and change the comments.
//  // In order to change the comments, a reference must be used
//  // (because the ColumnDesc copy constructor and assign have copy
//  // semantics).
//  ColumnDesc& myColDesc = tableDesc.columnDesc ("aName");
//  myColDesc.comment() += "some more comments";
// </srcblock>
// </example>

// <motivation>
// When getting the description of an arbitrary column, a pointer to
// that description is needed to allow proper execution of virtual
// functions.
// An envelope class is needed to hide this from the user.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


class ColumnDesc
{
friend class ColumnDescSet;
friend class ColumnSet;
friend class BaseColumn;
friend class RefTable;

public:

    // Enumerate the possible column options.
    // They can be combined by adding (logical or-ing) them.
    enum Option {
	// direct table or array
	Direct=1,
	// undefined values are possible
	Undefined=2,
	// fixed array/table shape
	FixedShape=4
    };

    // Construct from a column description.
    // This constructor is merely for the purpose of the automatic
    // conversion of an object like ScalarColumnDesc<T> to
    // ColumnDesc when adding a column to the table description
    // using the function TableDesc::addColumn.
    ColumnDesc (const BaseColumnDesc&);

    // Copy constructor (copy semantics).
    ColumnDesc (const ColumnDesc& that);

    // Default constructor (needed for ColumnDescSet).
    ColumnDesc();

    ~ColumnDesc();

    // Assignment (copy semantics).
    ColumnDesc& operator= (const ColumnDesc& that);

    // Comparison.
    // Two descriptions are equal when their data types, value types
    // (scalar, array or table) and possible dimensionalities are equal.
    // <group>
    Bool operator== (const ColumnDesc&) const;
    Bool operator!= (const ColumnDesc&) const;
    // </group>

    // Get access to the set of keywords.
    // <group>
    TableRecord& rwKeywordSet()
	{ return colPtr_p->rwKeywordSet(); }
    const TableRecord& keywordSet() const
	{ return colPtr_p->keywordSet(); }
    // </group>

    // Get the name of the column.
    //# This should be inlined, but CFront turns it into a static.
    const String& name() const;
//#	{ return colPtr_p->name(); }

    // Get the data type of the column.
    // This always returns the type of a scalar, even when the column
    // contains arrays.
    DataType dataType() const
	{ return colPtr_p->dataType(); }

    // Get the true data type of the column.
    // Unlike dataType, it returns an array data type (e.g. TpArrayInt)
    // when the column contains arrays.
    DataType trueDataType() const;

    // Get the type id for non-standard data types (i.e. for TpOther).
    // For standard data types the returned string is empty.
    const String& dataTypeId() const
	{ return colPtr_p->dataTypeId(); }

    // Get the type name of the default data manager.
    const String& dataManagerType() const
	{ return colPtr_p->dataManagerType(); }

    // Get the data manager group.
    const String& dataManagerGroup() const
	{ return colPtr_p->dataManagerGroup(); }

    // Get comment string.
    const String& comment() const
	{ return colPtr_p->comment(); }

    // Get comment string (allowing it to be changed).
    String& comment()
	{ return colPtr_p->comment(); }

    // Get the options. The possible options are defined by the enum Option.
    // E.g.
    // <srcblock>
    //    const ColumnDesc& coldesc = tableDesc.getColumn ("column_name");
    //    if (coldesc.option() & ColumnDesc::Direct  ==  ColumnDesc::Direct) {
    //            // the column has the Direct flag set
    //    }
    // </srcblock>
    int options() const
	{ return colPtr_p->options(); }

    // Check if the column is defined with a fixed shape.
    // This is always true for scalars. For arrays it is true when
    // the FixedShape flag was set when the column was defined.
    Bool isFixedShape() const;

    // Test if column is a scalar.
    Bool isScalar() const
	{ return colPtr_p->isScalar(); }
    // Test if column is an array.
    Bool isArray() const
	{ return colPtr_p->isArray(); }
    // Test if column is a table.
    Bool isTable() const
	{ return colPtr_p->isTable(); }

    // Get the number of dimensions.
    Int ndim() const
	{ return colPtr_p->ndim(); }

    // Get the predefined shape.
    // If not defined, a zero shape will be returned.
    const IPosition& shape() const
	{ return colPtr_p->shape(); }

    // Set the number of dimensions.
    // This is only allowed for arrays, for which the dimensionality
    // has not been defined yet.
    void setNdim (uInt ndim)
	{ colPtr_p->setNdim (ndim); }

    // Set the predefined shape.
    // This is only allowed for arrays, for which the shape
    // has not been defined yet.
    // If the dimensionality has already been defined, it must match.
    // It will set the option FixedShape if not set yet.
    void setShape (const IPosition& shape)
	{ colPtr_p->setShape (shape); }

    // Get the maximum value length.
    uInt maxLength() const
	{ return colPtr_p->maxLength(); }

    // Set the maximum value length.
    // So far, this is only possible for columns containing String values.
    // An exception is thrown if the column data type is not TpString.
    // Some storage managers support fixed length strings and can store
    // them more efficiently than variable length strings.
    void setMaxLength (uInt maxLength)
	{ colPtr_p->setMaxLength (maxLength); }

    // Get table description (in case column contains subtables).
    // <group>
    const TableDesc* tableDesc() const
	{ return colPtr_p->tableDesc(); }
    TableDesc* tableDesc()
	{ return colPtr_p->tableDesc(); }
    // </group>

    // Show the column on cout.
    void show() const;

    // Show the column.
    void show (ostream& os) const;

    // Write into AipsIO.
    friend AipsIO& operator<< (AipsIO& ios, const ColumnDesc& cd)
	{ cd.putFile (ios, ""); return ios; }

    // Read from AipsIO.
    friend AipsIO& operator>> (AipsIO& ios, ColumnDesc& cd)
	{ cd.getFile(ios, False, "");  return ios;}

    // Show on ostream.
    friend ostream& operator<< (ostream& ios, const ColumnDesc& cd);

    // Serve as default function for registerMap (see below), which catches 
    // all unknown xxxColumnDesc class names.
    // <thrown>
    //   <li> TableUnknownDesc
    // </thrown>
    static BaseColumnDesc* unknownColumnDesc (const String& name);

protected:
    BaseColumnDesc* colPtr_p;
    Bool            allocated_p;    //# False = not allocated -> do not delete

    // Define a map which maps the name of the various xxxColumnDesc
    // classes to a static function constructing them.
    // This is used when reading a column description back; it in fact
    // determines the exact column type and is an easier thing to do
    // than an enormous switch statement.
    // The map is filled by the function registerColumnDesc upon the
    // first call of ColumnDesc::getFile.
    static Bool registrationDone_p;
public:
    static SimpleOrderedMap<String, BaseColumnDesc* (*)(const String&)> registerMap;


private:
    // Construct from a pointer (for class BaseColumn).
    ColumnDesc (BaseColumnDesc*);

    // Check if a column can be handled by ColumnDescSet.
    // It is called before the column gets actually added, etc..
    // <group>
    // Check if the column can be added to the table description.
    // It is implemented for a virtual column to check if the columns
    // it uses really exist.
    void checkAdd (const ColumnDescSet& cds) const
	{ colPtr_p->checkAdd (cds); }
    // Check when a column gets renamed in a table description.
    // It is not used.
    void checkRename (const ColumnDescSet& cds, const String& newName) const
	{ colPtr_p->checkRename (cds, newName); }
    // </group>

    // Take action after a column has been handled by ColumnDescSet.
    // It is called after the column has been actually added, etc..
    // This gives, for instance, the virtual column class the opportunity
    // to update the virtual column list.
    // <group>
    void handleAdd (ColumnDescSet& cds)
	{ colPtr_p->handleAdd (cds); }
    void handleRename (ColumnDescSet& cds, const String& oldName)
	{ colPtr_p->handleRename (cds, oldName); }
    void handleRemove (ColumnDescSet& cds)
	{ colPtr_p->handleRemove (cds); }
    // </group>

    // This function allows each column to act upon a rename of another column.
    // If the old name is used internally, the column can update itself.
    // It is called after handleRename has been called.
    void renameAction (const String& newName, const String& oldName)
	{ colPtr_p->renameAction (newName, oldName); }

    // Create a PlainColumn column object out of this column description.
    PlainColumn* makeColumn (ColumnSet* csp) const
	{ return colPtr_p->makeColumn (csp); }

    // Create a RefColumn column object out of this column description.
    RefColumn* makeRefColumn (RefTable* rtp, BaseColumn* bcp) const
	{ return colPtr_p->makeRefColumn (rtp, bcp); }

    // Set the name of the column.
    void setName (const String& name)
	{ colPtr_p->setName(name); }

    // Store the object in AipsIO.
    void putFile (AipsIO& ios, const String& parentTableName) const;

    // Get the object from AipsIO.
    void getFile (AipsIO&, Bool tableIsWritable,
		  const String& parentTableName);

    // Register all column types. In this way the getFile function
    // can construct the correct xxxColumnDesc object.
    void registerColumnDesc();
};

inline ColumnDesc::ColumnDesc()
: colPtr_p(0),
  allocated_p (False)
{}

#endif
