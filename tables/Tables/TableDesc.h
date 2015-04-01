//# TableDesc.h:  specify structure of Casacore tables
//# Copyright (C) 1994,1995,1996,1997,1999,2000,2001,2002
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

#ifndef TABLES_TABLEDESC_H
#define TABLES_TABLEDESC_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/ColDescSet.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableRecord;
class TableAttr;
class TabPath;
template<class T> class Vector;

// <summary>
// Define the structure of a Casacore table
// </summary>

// <use visibility=export>

// <reviewed reviewer="Paul Shannon" date="1994/08/11" tests="none">
// </reviewed>

// <prerequisite>
//   <li> column description classes
//   <li> TableRecord
// </prerequisite>

// <synopsis> 
// A TableDesc object contains the description, or structure, of a table.
// This description is required for the creation of a new table.  
// Descriptions are subsequently associated with every table and
// embedded in them.
//
// A table description consists of the following items:
// <ul>
//  <li> Name, which cannot be blank if the description is saved in a file.
//         The file name will be this name followed by .tabdsc.
//  <li> Version, which defaults to a blank string.
//         It serves merely as information for the user.
//  <li> Comment, which defaults to an empty string.
//         This serves purely as an informational string for the user.
//  <li> A set of column descriptions which has to be added to the
//         table description. A column description can be created using
//         the classes ScalarColumnDesc, etc..
//         At table creation it is determined by the user if a column
//         has to be stored using a storage manager or calculated
//         on-the-fly using a so-called virtual column engine.
//  <li> A keyword set, which is by default empty.
//         When a table is created from the description, it gets
//         a copy of this keyword set as its initial keyword set.
// </ul>
//
// A TableDesc object can be constructed with one of the following
// options:
// <ul>
//  <li> Old
//    Open an existing table description file as readonly.
//  <li> Update
//    Open an existing table description file as read/write
//    The TableDesc destructor will rewrite the possibly changed
//    description.
//  <li> New
//    Create a new table description file.
//    The TableDesc destructor will write the table description into the file.
//  <li> NewNoReplace
//    As option New, but an exception will be thrown if the table
//    description file already exists.
//  <li> Scratch
//    Create a temporary table description. The table description will
//    be lost when the TableDesc object is destructed.
//    This is useful to create a Table object without storing the
//    description separately.
//    Note that the Table object maintains its own description (i.e. it
//    copies the description when being constructed).
//  <li> Delete
//    Delete the table description file. This gets done by the destructor.
// </ul>
//
// More information is provided in the Tables module documentation.
// </synopsis>
 
// <example>
// <srcblock>
//     // First build the new description of a subtable.
//     // Define columns ra and dec (double).
//     TableDesc subTableDesc("tTableDesc_sub", "1", TableDesc::New);
//     subTableDesc.addColumn (ScalarColumnDesc<double>("ra"));
//     subTableDesc.addColumn (ScalarColumnDesc<double>("dec"));
//
//     // Now create a new table description
//     // Define a comment for the table description.
//     // Define a double keyword.
//     ColumnDesc colDesc1, colDesc2;
//     TableDesc td("tTableDesc", "1", TableDesc::New);
//     td.comment() = "A test of class TableDesc";
//     td.rwKeywordSet().define ("equinox", 1950.0);
//
//     // Define an integer column ab using the TableDesc::addColumn
//     // function which creates a scalar column description.
//     td.addColumn (ScalarColumnDesc<Int>("ab", "Comment for column ab"));
//
//     // Add a scalar integer column ac, define keywords for it
//     // and define a default value 0.
//     // Overwrite the value of keyword unit.
//     ScalarColumnDesc<Int> acColumn("ac");
//     acColumn.rwKeywordSet().define ("scale", Complex(0.0f));
//     acColumn.rwKeywordSet().define ("unit", "");
//     acColumn.setDefault (0);
//     td.addColumn (acColumn);
//     td["ac"].rwKeywordSet().define ("unit", "DEG");
//
//     // Add a scalar string column ad and define its comment string.
//     td.addColumn (ScalarColumnDesc<String>("ad","comment for ad"));
//
//     // Now define array columns.
//     // This one is indirect and has no dimensionality mentioned yet.
//     td.addColumn (ArrayColumnDesc<Complex>("Arr1","comment for Arr1"));
//     // This one is indirect and has 3-dim arrays.
//     td.addColumn (ArrayColumnDesc<Int>("A2r1","comment for Arr1",3));
//     // This one is direct and has 2-dim arrays with axes length 4 and 7.
//     td.addColumn (ArrayColumnDesc<uInt>("Arr3","comment for Arr1",
//        				   IPosition(2,4,7),
// 					   ColumnDesc::Direct));
//
//     // Add a columns containing tables.
//     td.addColumn (SubTableDesc("sub1", "subtable by name",
//                                "tTableDesc_sub"));
//
//     // Define hypercolumn "dataCube".
//     td.addColumn (ArrayColumnDesc<Complex>("data",2));
//     td.addColumn (ArrayColumnDesc<Int>("pol",1));
//     td.addColumn (ArrayColumnDesc<float>("freq",1));
//     td.addColumn (ScalarColumnDesc<float>("time"));
//     td.addColumn (ScalarColumnDesc<float>("baseline"));
//     td.defineHypercolumn ("dataCube", 4,
//                           stringToVector ("data"),
//                           stringToVector ("pol,freq,time,baseline"));
// }
// </srcblock>
// </example>

// <motivation>
// A table description specifies the structure, but not the contents,
// of a Casacore table.  Since many tables will have identical structure
// and different content, it makes good sense to separate structure 
// ("description") from content.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TableDesc
{
public:

    //# Enumerate the possible options for TableDesc.
    enum TDOption {Old=1, New, NewNoReplace, Scratch, Update, Delete};

    // The default constructor creates a table description with
    // option = Scratch and a blank name.
    TableDesc();

    // Create a table description object with the given name.
    // This name can be seen as the table type in the same way as a
    // class name is the data type of an object.
    // The name can only be blank when option=Scratch.
    // The default table description path is used for the description file.
    TableDesc (const String& type, TDOption = Old);

    // Create a table description object with the given name (i.e. table type)
    // and version.
    // The name can only be blank when option=Scratch.
    // The default table description path is used for the description file.
    TableDesc (const String& type, const String& version, TDOption = Old);

    // Create a table description object.
    // The given table description path is used for the description file.
    // The name can only be blank with option=Scratch.
    TableDesc (const String& type, const String& version,
	       const TabPath&, TDOption = Old);

    // Create a table description object with the given name (i.e. table type)
    // and version by copying the input table description.
    // If the given name or version is blank, it will be copied from
    // the input table description.
    // The default table description path is used for the description file.
    // The only options allowed are New, NewNoReplace and Scratch.
    TableDesc (const TableDesc&, const String& type, const String& version,
	       TDOption, Bool copyColumns=True);

    // Create a table description object with the given name (i.e. table type)
    // and version by copying the input table description.
    // If the given name or version is blank, it will be copied from
    // the input table description.
    // The given table description path is used for the description file.
    // The only options allowed are New, NewNoReplace and Scratch.
    TableDesc (const TableDesc&, const String& type, const String& version,
	       const TabPath&, TDOption, Bool copyColumns=True);

    // This copy constructor makes a copy of the table description
    // maintaining its name and version. By default a Scratch copy is made.
    // It serves as a shorthand for the constructor:
    // <br><src> TableDesc (const TableDesc&, "", "", TDOption); </src>
    TableDesc (const TableDesc&, TDOption = Scratch);

    // The destructor writes the table description if changed.
    ~TableDesc();

    // Test if a description file exists (i.e. isReadable).
    static Bool isReadable (const String& tableDescName);

    // Get access to the set of column descriptions.
    // In this way const <linkto class=ColumnDescSet>ColumnDescSet</linkto>
    // functions (e.g. isDisjoint) can be used.
    const ColumnDescSet& columnDescSet() const;

    // Add another table description to this table description.
    // It merges the column descriptions, the special keywordSet
    // (containing hypercolumn definitions) and the user keywordSet
    // (this last one is not added if the flag is False).
    // The two table descriptions have to be disjoint, i.e. no column
    // nor keyword should already exist. Otherwise an TableInvOper
    // exception is thrown and nothing gets added.
    void add (const TableDesc& other, Bool addKeywordSet = True);

    // Get access to the keyword set.
    // <group>
    TableRecord& rwKeywordSet();
    const TableRecord& keywordSet() const;
    // </group>

    // Get readonly access to the private set of keywords.
    const TableRecord& privateKeywordSet() const;

    // Add a column to the table description.
    // An exception is thrown if a keyword or column with this name
    // already exists.
    // Although this function has a <src>ColumnDesc</src> as argument,
    // it is usually needed to construct a more specialized object like
    // <src>ArrayColumnDesc<float></src>. A <src>ColumnDesc</src>
    // constructor converts that automatically to a <src>ColumnDesc</src>
    // object.
    // <srcblock>
    //   tableDesc.addColumn (ArrayColumnDesc<float> ("NAME"));
    // </srcblock>
    // On the other hand this function can also be used to add a
    // column description from another table as in:
    // <srcblock>
    //   tableDesc.addColumn (otherTableDesc.columnDesc("NAME"));
    // </srcblock>
    ColumnDesc& addColumn (const ColumnDesc&);

    // Add a column to the table description and give it another name.
    // This may be useful to use a description of another column.
    ColumnDesc& addColumn (const ColumnDesc&, const String& newname);

    // Remove a column.
    // An exception is thrown if the column does not exist.
    void removeColumn (const String& name);

    // Rename a column.
    // An exception is thrown if the old name does not exist or
    // if the name already exists.
    // <note role=caution>
    // Renaming a column should be done with care, because other
    // columns may be referring this column. Also a hypercolumn definition
    // might be using the old name.
    // </note>
    void renameColumn (const String& newname, const String& oldname);

    // Get number of columns.
    uInt ncolumn() const;

    // Test if a column with this name exists.
    Bool isColumn (const String& name) const;

    // Get a vector containing all column names.
    Vector<String> columnNames() const;

    // Get the column description by name or by index.
    // An exception is thrown if the column does not exist.
    // Function isColumn should be used to test if a column exists.
    // <group>
    const ColumnDesc& columnDesc (const String& name) const;
    const ColumnDesc& operator[] (const String& name) const;
    const ColumnDesc& columnDesc (uInt index) const;
    const ColumnDesc& operator[] (uInt index) const;
    ColumnDesc& rwColumnDesc (const String& name);
    ColumnDesc& rwColumnDesc (uInt index);
    // </group>

    // Get comment string.
    const String& comment() const;

    // Get comment string (allowing it to be changed).
    String& comment();

    // Show the table description on cout.
    void show() const;

    // Show the table description.
    void show (ostream& os) const;

    // Get the table type (i.e. name of table description).
    const String& getType() const;

    // Get the table description version.
    const String& version() const;

    // Define a hypercolumn.
    // A hypercolumn is a group of one or more data columns of which
    // the data is treated as one or more (regular) hypercubes.
    // The hypercolumn has coordinate axes (e.g. time, frequency)
    // which are columns in the table.
    // When the entire hypercolumn consists of multiple hypercubes,
    // ID-columns can be defined, which uniquely determine the
    // hypercube to be used.
    //  Note that only <linkto class=TiledDataStMan>TiledDataStMan</linkto>
    // requires the use of ID-columns.
    // A hypercolumn definition is needed to be able to use a Tiled
    // Storage Manager.
    //
    // The following has to be specified:
    // <dl>
    //  <dt> Hypercolumn name
    //  <dd> which is the name used to refer to the hypercolumn.
    //  <dt> ndim
    //  <dd> defining the dimensionality of the hypercolumn (and
    //       of its hypercube(s)).
    //  <dt> Data column names
    //  <dd> which are the columns containing the hypercube data.
    //       When multiple columns are used, the shapes of the data
    //       in their cells must be the same in the same row.
    //       All data columns must contain numeric or Bool scalars or arrays.
    //       <dl>
    //        <dt> array:
    //        <dd> Its dimensionality has to be less than or equal to the
    //             dimensionality of the hypercolumn. If equal, the
    //             array itself already forms the hypercube. That would
    //             mean that each row is a hypercube.
    //             If less, the arrays from multiple rows form a hypercube,
    //             adding one or more dimensions to the array dimensionality.
    //        <dt> scalar:
    //        <dd> The data from multiple rows form a hypercube.
    //             Not all tiled storage managers support scalars.
    //       </dl>
    //  <dt> Coordinate column names (optional)
    //  <dd> which are the columns containing the coordinates of the
    //       hypercubes. They must be (u)Int, float, double or (D)Complex.
    //       When given, the number of coordinate columns must match the
    //       dimensionality of the hypercolumn.
    //       <br>
    //       When the data column cells contain arrays, the first N coordinate
    //       columns must contain vector values, where N is the dimensionality
    //       of the data arrays.
    //       The remaining coordinate columns must contain scalar values.
    //  <dt> Id column names (optional)
    //  <dd> have to be given when a hypercolumn can consist of multiple
    //       hypercubes. They define the column(s) determining which
    //       hypercube has to be used for a data array.
    //       The id columns must contain scalar values ((u)Int, float,
    //       double, (D)Complex, String and/or Bool).
    // </dl>
    // It will be checked if the given columns exists and have
    // an appropriate type.
    // <br>
    // The default data manager type of the columns involved will be set
    // to TiledColumnStMan if all data columns have a fixed shape.
    // Otherwise they are set to TiledShapeStMan.
    // The storage manager group of all columns involved will be set to
    // the hypercolumn name. In that way binding columns to storage managers
    // during the table creation process is easier because a simple
    // <code>bindGroup</code> can be used.
    // <p>
    // For example:<br>
    // A table contains data matrices with axes pol and freq.
    // Those axes are defined in columns pol and freq containing
    // vectors with the same length as the corresponding axis.
    // The table also contains scalar columns time and baseline, which
    // superimpose dimensions upon the data. So the data will be stored
    // in a 4-d hypercube with axes pol,freq,time,baseline.
    // It would be defined as follows:
    // <srcblock>
    //     tableDesc.defineHypercolumn ("dataCube", 4,
    //                            stringToVector ("data"),
    //                            stringToVector ("pol,freq,time,baseline"));
    // </srcblock>
    // Note that the function <linkto group="ArrayUtil.h#stringToVector">
    // stringToVector</linkto> is very convenient for creating a vector
    // of Strings.
    // <group name=defineHypercolumn>
    void defineHypercolumn (const String& hypercolumnName,
			    uInt ndim,
			    const Vector<String>& dataColumnNames);
    void defineHypercolumn (const String& hypercolumnName,
			    uInt ndim,
			    const Vector<String>& dataColumnNames,
			    const Vector<String>& coordColumnNames);
    void defineHypercolumn (const String& hypercolumnName,
			    uInt ndim,
			    const Vector<String>& dataColumnNames,
			    const Vector<String>& coordColumnNames,
			    const Vector<String>& idColumnNames);
    // </group>

    // Test if the given hypercolumn exists.
    Bool isHypercolumn (const String& hypercolumnName) const;

    // Get the names of all hypercolumns.
    Vector<String> hypercolumnNames() const;

    // Get the columns involved in a hypercolumn.
    // It returns the dimensionality of the hypercolumn.
    // An exception is thrown if the hypercolumn does not exist.
    uInt hypercolumnDesc (const String& hypercolumnName,
			  Vector<String>& dataColumnNames,
			  Vector<String>& coordColumnNames,
			  Vector<String>& idColumnNames) const;

    // Adjust the hypercolumn definitions (for a RefTable).
    // It removes and/or renames columns as necessary.
    // Column names which are not part of the map are removed if
    // <src>keepUnknown==False</src>.
    // If all data columns of a hypercolumn are removed, the entire
    // hypercolumn is removed.
    void adjustHypercolumns (const SimpleOrderedMap<String,String>& old2new,
			     Bool keepUnknownData = False,
			     Bool keepUnknownCoord = False,
			     Bool keppUnknownId = False);

    // Remove ID-columns from the given hypercolumn definitions
    // and set their default data manager type to IncrementalStMan
    // and group to ISM_TSM.
    void removeIDhypercolumns (const Vector<String>& hcNames);

    // Remove given hypercolumn definition.
    // An exception is thrown if it is not a hypercolumn.
    void removeHypercolumnDesc (const String& hypercolumnName);

    // Check recursively if the descriptions of all subtables are known.
    void checkSubTableDesc() const;

    void renameHypercolumn (const String& newHypercolumnName,
			    const String& hypercolumnName);


private:
    String             name_p;          //# name of table description
    String             vers_p;          //# version of table description
    String             dir_p;           //# directory
    String             comm_p;          //# comment
    //# Note: the TableRecords are done as pointer, otherwise TableRecord.h
    //# needs to be included leading to a mutual include.
    TableRecord*       key_p;           //# user set of keywords
    TableRecord*       privKey_p;       //# Private set of keywords
    ColumnDescSet      col_p;           //# set of column names + indices
    Bool               swwrite_p;       //# True = description can be written
    TDOption           option_p;        //# Table desc. open option
    AipsIO             iofil_p;         //# File

    // Assignment is not supported, because it is impossible to define
    // its semantics. Does the data need to be written into a file
    // before being overwritten?
    // Declaring it private, makes it unusable.
    TableDesc& operator= (const TableDesc&);

    // Initialize the table description.
    void init (const TabPath&);

    // Initialize and copy a table description.
    void copy (const TableDesc&, const TabPath&, Bool copyColumns);

    // Throw an invalid hypercolumn exception.
    void throwHypercolumn (const String& hyperColumnName,
			   const String& message);


public:
    // Put the table description into the file.
    // The name can be used to write the TableDesc from a Table and
    // is used to set the names of subtables correctly.
    void putFile (AipsIO&, const TableAttr&) const;

    // Get the table description from the file.
    void getFile (AipsIO&, const TableAttr&);
};


//# Get number of columns.
inline uInt TableDesc::ncolumn () const
    { return col_p.ncolumn(); }

//# Test if column exists.
inline Bool TableDesc::isColumn (const String& name) const
    { return col_p.isDefined(name); }

//# Get a column description.
inline const ColumnDesc& TableDesc::columnDesc (const String& name) const
    { return col_p[name]; }
inline const ColumnDesc& TableDesc::operator[] (const String& name) const
    { return col_p[name]; }
inline const ColumnDesc& TableDesc::columnDesc (uInt index) const
    { return col_p[index]; }
inline const ColumnDesc& TableDesc::operator[] (uInt index) const
    { return col_p[index]; }
inline ColumnDesc& TableDesc::rwColumnDesc (const String& name)
    { return col_p[name]; }
inline ColumnDesc& TableDesc::rwColumnDesc (uInt index)
    { return col_p[index]; }


//# Return the name (ie. type) of the table description.
inline const String& TableDesc::getType () const
    { return name_p; }

//# Return the version of the table description.
inline const String& TableDesc::version () const
    { return vers_p; }

//# Get access to the sets of keywords.
inline TableRecord& TableDesc::rwKeywordSet ()
    { return *key_p; }
inline const TableRecord& TableDesc::keywordSet () const
    { return *key_p; }
inline const TableRecord& TableDesc::privateKeywordSet () const
    { return *privKey_p; }

//# Get the set of columns.
inline const ColumnDescSet& TableDesc::columnDescSet() const
    { return col_p; }

//# Add a column.
inline ColumnDesc& TableDesc::addColumn (const ColumnDesc& column)
    { return col_p.addColumn (column); }

inline ColumnDesc& TableDesc::addColumn (const ColumnDesc& column,
					 const String& newname)
    { return col_p.addColumn (column, newname); }

//# Remove a column.
inline void TableDesc::removeColumn (const String& name)
    { col_p.remove (name); }

//# Access the comment.
inline const String& TableDesc::comment () const
    { return comm_p; }

inline String& TableDesc::comment ()
    { return comm_p; }

inline void TableDesc::checkSubTableDesc () const
   { col_p.checkSubTableDesc(); }




} //# NAMESPACE CASACORE - END

#endif

