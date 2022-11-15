//# TableUtil.h: Utility functions for tables
//# Copyright (C) 2022
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
//# You should have receied a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef TABLES_TABLEUTIL_H
#define TABLES_TABLEUTIL_H

#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableLock.h>
#include <casacore/tables/DataMan/TSMOption.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Utilities/DataType.h>

namespace casacore {

  // The TableUtil namespace contains several convenience functions operating
  // on Table objects. They make it very convenient to open, close or delete
  // main tables and subtables.
  // <p>
  // The function <src>openTable</src> makes it possible to open a subtable
  // of a table in a convenient way, even if the table is only a reference
  // to another table (e.g., a selection). The name can be given with colons as
  // 'maintab::subtab1::subtab2' meaning that subtab2 is opened and returned.
  // Of course, it can also be used to open a main table such as 'my.tab'.
  //
  // Similar to <src>openTable</src>, the function <src>createTable</src>
  // can be used to create a (sub)table, possibly using the :: notation.
  // <br><src>deleteTable</src> is similar to delete a (sub)table.

  namespace TableUtil {

    // Try to open a table. The name of the table can contain subtable names
    // using :: as separator. In this way it is possible to directly open a
    // subtable of a RefTable or ConcatTable, which is not possible if the
    // table name is specified with slashes.
    // <br>The open process is as follows:
    // <ul>
    //  <li> It is tried to open the table with the given name.
    //  <li> If unsuccessful, the name is split into its parts using ::
    //       The first part is the main table which will be opened temporarily.
    //       The other parts are the successive subtable names (usually one).
    //       Each subtable is opened by looking it up in the keywords of the
    //       table above. The final subtable is returned.
    // </ul>
    // <br>An exception is thrown if the table cannot be opened.
    // <example>
    // Open the ANTENNA subtable of an MS which might be a selection of
    // a real MS.
    // <srcblock>
    // Table tab(Table::openTable ("sel.ms::ANTENNA");
    // </srcblock>
    // </example>
    // <group>
    Table openTable (const String& tableName,
                     Table::TableOption = Table::Old,
                     const TSMOption& = TSMOption());
    Table openTable (const String& tableName,
                     const TableLock& lockOptions,
                     Table::TableOption = Table::Old,
                     const TSMOption& = TSMOption());
    // </group>

    // Create a table with the given name and description.
    // Datamanager information can be given in the Record.
    // The table name can be given with the :: notation meaning that a subtable
    // of the previous part is created. Depending on the TableOption, that subtable
    // can or cannot exist yet.
    // It defines the subtable keyword in the parent table.
    // <br>An exception is thrown if one of the parts cannot be opened.
    // <example>
    // Create the ANT subtable of an MS with some description and create the
    // table keyword ANT in sel.ms referring to the subtable.
    // It is replaced if already existing (not if TableOption::NewNoReplace is given).
    // <srcblock>
    // Table tab(Table::createTable ("sel.ms::ANT", someDesc, TableOption::New));
    // </srcblock>
    // </example>
    Table createTable (const String& tableName,
                       const TableDesc&,
                       Table::TableOption,
                       Table::TableType = Table::Plain,
                       const StorageOption& = StorageOption(),
                       const Record& dmInfo = Record(),
                       const TableLock& lockOptions = TableLock(),
                       rownr_t nrrow = 0,
                       Bool initialize = False,
                       Table::EndianFormat = Table::AipsrcEndian,
                       const TSMOption& = TSMOption());
    Table createSubTable (Table& parent, const String& subtableName,
                          const TableDesc& desc,
                          Table::TableOption,
                          const StorageOption& = StorageOption(),
                          const Record& dmInfo = Record(),
                          const TableLock& lockOptions = TableLock(),
                          rownr_t nrrow = 0,
                          Bool initialize = False,
                          Table::EndianFormat = Table::AipsrcEndian,
                          const TSMOption& = TSMOption());

    // Can the table be deleted?
    // If true, function deleteTable can safely be called.
    // If not, message contains the reason why (e.g. 'table is not writable').
    // It checks if the table is writable, is not open in this process
    // and is not open in another process.
    // If <src>splitColons=True</src> the table name can contain :: to
    // denote subtables.
    // <br>If <src>checkSubTables</src> is set, it also checks if
    // a subtable is not open in another process.
    // <br> <src>canDeleteSubTable</src> can be used to check a subtable of the
    // given parent.
    // <group>
    Bool canDeleteTable (const String& tableName,
                         Bool checkSubTables=False);
    Bool canDeleteTable (String& message, const String& tableName,
                         Bool checkSubTables=False, Bool splitColons=True);
    Bool canDeleteSubTable (String& message, const Table& parent,
                            const String& subtableName,
                            Bool checkSubTables=False);
    // </group>

    // Delete the table.
    // An exception is thrown if the table cannot be deleted because
    // its is not writable or because it is still open in this or
    // another process.
    // <br>If <src>checkSubTables</src> is set, it is also checked if
    // a subtable is used in another process.
    // <br> <src>deleteSubTable</src> can be used to delete a subtable of the
    // given parent.
    void deleteTable (const String& tableName,
                      Bool checkSubTables=False);
    void deleteSubTable (Table& parent, const String& subtableName,
                         Bool checkSubTables = False);

    // Return the layout of a table (i.e. description and #rows).
    // This function has the advantage that only the minimal amount of
    // information required is read from the table, thus it is
    // faster than a normal table open. The table name can be a subtable using ::.
    // <br> The number of rows is returned. The description of the table
    // is stored in desc (its contents will be overwritten).
    // <br> An exception is thrown if the table does not exist.
    rownr_t getLayout (TableDesc& desc, const String& tableName);

    // Get the table info of the table with the given name.
    // An empty object is returned if the table is unknown.
    // The table name can be a subtable using ::.
    TableInfo tableInfo (const String& tableName);

    // Get the full name (absolute path) of the given table name, which can
    // be a subtable specification using ::.
    String getFullName (const String& tableName);

    // Find the parent table of the last subtable in a table name containing
    // :: to indicate subtables.
    // It returns the Table object of that parent table and the name of
    // the last subtable. An empty Table is returned if the table name does
    // not contain subtable names.
    // In case of an error, an exception is thrown.
    std::pair<Table,String> findParentTable (const String& fullName,
                                             const TableLock& lockOptions=TableLock(),
                                             Table::TableOption option=Table::Old,
                                             const TSMOption& tsmOption=TSMOption());

  } //# NAMESPACE TableUtil - END
} //# NAMESPACE CASACORE - END

#endif

