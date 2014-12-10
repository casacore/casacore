//# SubTabDesc.h: Description of columns containing tables
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

#ifndef TABLES_SUBTABDESC_H
#define TABLES_SUBTABDESC_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/BaseColDesc.h>
#include <casacore/casa/Containers/SimOrdMap.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class PlainColumn;
class ColumnSet;
class TableDesc;
class String;
class AipsIO;


// <summary>
// Description of columns containing tables
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableDesc
//   <li> BaseColumnDesc
// </prerequisite>

// <etymology>
// SubTableDesc holds a description of a subtable contained in the
// columns of the parent table.
// </etymology>

// <synopsis> 
// SubTableDesc describes a table column containing subtables.
// The semantics of subtables are described below.
// The column description is constructed using a table description
// describing the subtable. This subtable decription or its name is
// stored with the column description.
// When a table containing this column gets created, the subtable
// description gets copied and this copy is thereafter frozen.

// Constructing a column description for a subtable can be done
// in 3 ways:
// <ul>
//  <li> It can be constructed with the name of a table description
//         kept in a file. Only this name will be stored with the column
//         description. Only when the table column gets created,
//         it will read the newest version of this table description.
//         This is a completely dynamic way of defining the column.
//         When the subtable description in the file changes, this column
//         in newly created tables gets the latest version.
//  <li> It can be constructed with a given table description.
//         This means that a copy of that description will be made.
//         The frozen subtable description will be stored with the
//         column description.
//         This is a completely static way of defining the column.
//  <li> It can be constructed with a pointer to a table description.
//         This means that a copy will only be made when the column
//         description gets written. Thus changes to the subtable
//         description will as long as possible be reflected in the
//         column description.
//         This is a mix of the first two ways.
// </ul>
//
// A column can be direct or indirect.
// Direct columns will be written directly in the table file. All cells
// in the column must have the same description and it is therefore not
// possible to change a description.
// The subtables in indirect columns will be stored in separate files.
// The cells in indirect columns can contain different tables. 
// </synopsis> 

// <example>
// <srcblock>
//     // First build the new description of a subtable.
//     // Define keyword subkey (integer) having value 10.
//     // Define columns ra and dec (double).
//     TableDesc subTableDesc("tTableDesc_sub", "1", TableDesc::New);
//     subTableDesc.keywordSet().keysInt()("subkey") = 10;
//     subTableDesc.addColumn (TpDouble, "ra");
//     subTableDesc.addColumn (TpDouble, "dec");
//
//     // Now create a new table description
//     TableDesc td("tTableDesc", "1", TableDesc::New);
//
//     // Add columns containing subtables.
//     // This is done in 3 slighty different ways, which all have
//     // their own (dis)advantages.
//     // This is described in detail at the SubTableDesc constructors.
//     td.addColumn (SubTableDesc("sub1", "subtable by name","tTableDesc_sub"));
//     td.addColumn (SubTableDesc("sub2", "subtable copy",    subTableDesc));
//     td.addColumn (SubTableDesc("sub3", "subtable pointer", &subTableDesc));
// </srcblock>
// </example>

// <motivation>
// Several column description classes are needed to allow the user
// to define attributes which are special for each column type.
// For columns containing a table this is the table description.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> Probably only direct table descriptions should be allowed.
//          Indirect arrays can have a shape in the description
//          (although they can have #dim), so tables should behave
//          similarly.
// </todo>


class SubTableDesc : public BaseColumnDesc
{
public:
friend class ColumnDesc;

public:
    // Construct from a table description with the given name.
    // The description does not need to exist yet. Only when the
    // table gets created, the description will be read and must exist.
    // This means that the table description is not frozen; the most
    // recent description will be used when creating the column.
    SubTableDesc (const String& columnName, const String& comment,
		  const String& tableDescName, int options = 0);

    // Construct from the given table description, which will be copied
    // and frozen.
    SubTableDesc (const String& columnName, const String& comment,
		  const TableDesc&, int options = 0);

    // Construct from the given table description, which will be used
    // directly. The description gets frozen when the column is written.
    // Care should be taken, because the given table description must
    // not be deleted before the column description gets destructed.
    SubTableDesc (const String& columnName, const String& comment,
		  TableDesc*, int options = 0);

    // Copy constructor (copy semantics).
    SubTableDesc (const SubTableDesc&);

    ~SubTableDesc();

    // Assignment (copy semantics).
    SubTableDesc& operator= (const SubTableDesc&);

    // Clone this column description to another.
    BaseColumnDesc* clone() const;

    // Get the table description.
    // <thrown>
    //   <li> TableNoFile
    // </thrown>
    TableDesc* tableDesc();

    // Get the name of this class.
    String className() const;

    // Create a Column column object out of this.
    // This is used by class ColumnSet to construct a table column object.
    PlainColumn* makeColumn (ColumnSet*) const;

    // Show the column.
    void show (ostream& os) const;

    // Create the object from AipsIO (this function is registered).
    static BaseColumnDesc* makeDesc(const String& name);

protected:
    // Put the object.
    virtual void putDesc (AipsIO&) const;

    // Get the object.
    virtual void getDesc (AipsIO&);

private:
    TableDesc*  tabDescPtr_p;               //# pointer to Table Description
    String      tabDescTyp_p;               //# type of table description
    Bool        byName_p;                   //# True = TableDesc name is given
    Bool        allocSelf_p;                //# True = allocated tdptr itself
    Bool        shallowCopy_p;              //# True = make shallow copy
    //#                                         (is only set when !allocSelf)

    // Read table description (if passed by name).
    // If the table description is not found, a False value is returned.
    Bool readTableDesc();

    // Handle the addition of the subtable description (clear the flag).
    void handleAdd (ColumnDescSet&);
};



} //# NAMESPACE CASACORE - END

#endif
