//# TableUtil.cc: Utility functions for tables
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

//# Includes
#include <casacore/tables/Tables/TableUtil.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableAttr.h>
#include <casacore/tables/Tables/PlainTable.h>
#include <casacore/tables/Tables/RefTable.h>
#include <casacore/tables/Tables/ConcatTable.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/OS/Path.h>

namespace casacore {
  namespace TableUtil {

    Table openTable (const String& tableName,
                     Table::TableOption option,
                     const TSMOption& tsmOption)
    {
      return openTable (tableName, TableLock(), option, tsmOption);
    }

    Table openTable (const String& tableName,
                     const TableLock& lockOpt,
                     Table::TableOption tabOpt,
                     const TSMOption& tsmOpt)
    {
      // See if the table can be opened as such.
      if (Table::isReadable(tableName)) {
        return Table(tableName, lockOpt, tabOpt, tsmOpt);
      }
      // Try to find and open the last subtable by splitting at ::
      std::pair<Table,String> t(findParentTable(tableName, lockOpt, tabOpt, tsmOpt));
      if (t.first.isNull()) {
        // Note that tableName was already tried to be opened at beginning.
        throw TableError ("Table " + tableName + " does not exist");
      }
      // Find the last subtable in the parent.
      if (! t.first.keywordSet().isDefined(t.second)) {
        throw TableError ("Table name " + tableName + " is invalid (" +
                          "subtable " + t.second + " is unknown)");
      }
      return t.first.keywordSet().asTable (t.second);
    }

    Table createTable (const String& tableName, const TableDesc& desc,
                       Table::TableOption tabOpt,
                       Table::TableType tabType,
                       const StorageOption& storageOption,
                       const Record& dmInfo,
                       const TableLock& lockOptions,
                       rownr_t nrrow, Bool initialize,
                       Table::EndianFormat endian,
                       const TSMOption& tsmOpt)
    {
      // Find and open the one but last subtable and get the name of the last one.
      // An empty name results in a Scratch table.
      std::pair<Table,String> t;
      if (! tableName.empty()) {
        t = findParentTable (tableName);
      }
      if (! t.second.empty()) {
        return createSubTable (t.first, t.second, desc, tabOpt, storageOption,
                               dmInfo, lockOptions, nrrow, initialize, endian, tsmOpt);
      } else {
        // No subtable given, so create a main table.
        SetupNewTable newtab(tableName, desc,
                             tableName.empty() ? Table::Scratch : tabOpt,
                             storageOption);
        newtab.bindCreate (dmInfo);
        // Create the table.
        return Table(newtab, tabType, lockOptions, nrrow, initialize, endian, tsmOpt);
      }
    }

    Table createSubTable (Table& parent, const String& subName,
                          const TableDesc& desc,
                          Table::TableOption tabOpt,
                          const StorageOption& storageOption,
                          const Record& dmInfo,
                          const TableLock& lockOptions,
                          rownr_t nrrow, Bool initialize,
                          Table::EndianFormat endian,
                          const TSMOption& tsmOpt)
    {
      // See if the subtable and its keyword already exist.
      Int inx = parent.keywordSet().fieldNumber(subName);
      if (inx >= 0) {
        if (parent.keywordSet().type(inx) != TpTable) {
          throw TableError("Subtable " + subName + " cannot be created in " +
                           parent.tableName() +
                           "; a keyword with that name already exists");
        }
        if (tabOpt == Table::NewNoReplace) {
          throw TableError("Subtable " + parent.tableName() + '/' + subName +
                           " already exists");
        }
        // Remove the subtable.
        deleteSubTable (parent, subName);
      }
      // Setup creation of the subtable and attach data managers.
      SetupNewTable newtab(parent.tableName() + '/' + subName,
                           desc, tabOpt, storageOption);
      newtab.bindCreate (dmInfo);
      // Create the table and define the keyword for it in the parent.
      Table subtab(newtab, lockOptions, nrrow, initialize, endian, tsmOpt);
      parent.reopenRW();
      parent.rwKeywordSet().defineTable (subName, subtab);
      return subtab;
    }

    Bool canDeleteTable (const String& tableName, Bool checkSubTables)
    {
      String message;
      return canDeleteTable (message, tableName, checkSubTables);
    }
    
    Bool canDeleteTable (String& message, const String& tableName,
                         Bool checkSubTables, Bool splitColons)
    {
      if (splitColons) {
        std::pair<Table,String> t = findParentTable (tableName);
        if (! t.first.isNull()) {
          return canDeleteSubTable (message, t.first, t.second, checkSubTables);
        }
      }
      String tabName = Path(tableName).absoluteName();
      if (! Table::isWritable (tabName)) {
        message = "table is not writable";
        return False;
      }
      if (Table::isOpened (tabName)) {
        message = "table is still open in this process";
        return False;
      }
      Table table(tabName);
      if (table.isMultiUsed()) {
        message = "table is still open in another process";
        return False;
      }
      if (checkSubTables  &&  table.isMultiUsed(True)) {
        message = "a subtable of the table is still open in another process";
        return False;
      }
      return True;
    }

    Bool canDeleteSubTable (String& message, const Table& parent,
                            const String& subtableName,
                            Bool checkSubTables)
    {
      // Get the full table name of the subtable.
      // Note: the temporary Table object is deleted before canDeleteTable.
      // Otherwise isOpened() used internally would be true.
      const String fullName (parent.keywordSet().asTable(subtableName).tableName());
      return canDeleteTable (message, fullName, checkSubTables, False);
    }

    void deleteTable (const String& tableName, Bool checkSubTables)
    {
      // Check that the name is not empty, because making it absolute results in /
      if (tableName.empty()) {
        throw TableError
          ("Empty string provided for tableName; will not attempt delete.");
      }
      // See if the name represents a table.
      if (! Table::isReadable(tableName)) {
        // See if the name contains subtable names using ::
        std::pair<Table,String> t = findParentTable (tableName);
        if (! t.first.isNull()) {
          deleteSubTable (t.first, t.second, checkSubTables);
          return;
        }
      }
      // Delete the table (which fails if it is not a table or still in use).
      String tabName = Path(tableName).absoluteName();
      String message;
      if (! canDeleteTable (message, tabName, checkSubTables)) {
        throw (TableError ("Table " + tabName + " cannot be deleted: " +
                           message));
      }
      Table table(tabName, Table::Delete);
    }


    void deleteSubTable (Table& parent, const String& subtableName, Bool checkSubTables)
    {
      String message;
      if (! canDeleteSubTable (message, parent, subtableName, checkSubTables)) {
        throw (TableError ("Subtable " + subtableName + " in " +
                           parent.tableName() + " cannot be deleted: " +
                           message));
      }
      Table subtab = parent.keywordSet().asTable(subtableName);
      subtab.markForDelete();
      // If there, remove the keyword referring the subtable.
      Int inx = parent.keywordSet().fieldNumber(subtableName);
      if (inx >= 0) {
        if (parent.keywordSet().type(inx) == TpTable) {
          parent.reopenRW();
          parent.rwKeywordSet().removeField (subtableName);
        }
      }
    }

    //# The logic is similar to that in Table::open.
    rownr_t getLayout (TableDesc& desc, const String& tableName)
    {
      rownr_t nrow;
      uInt format;
      String tp;
      AipsIO ios (Table::fileName(getFullName(tableName)));
      uInt version = ios.getstart ("Table");
      if (version > 3) {
        throw TableError ("Table version " + String::toString(version) +
                          " not supported by TableUtil in this version of Casacore");
      }
      if (version > 2) {
        ios >> nrow;
      } else {
        uInt n;
        ios >> n;
        nrow = n;
      }
      ios >> format;
      ios >> tp;
      if (tp == "PlainTable") {
        PlainTable::getLayout (desc, ios);
      } else if (tp == "RefTable") {
        RefTable::getLayout (desc, ios);
      } else if (tp == "ConcatTable") {
        ConcatTable::getLayout (desc, ios);
      } else {
        throw (TableInternalError
               ("TableUtil::getLayout: unknown table kind " + tp));
      }
      ios.close();
      return nrow;
    }

    TableInfo tableInfo (const String& tableName)
    {
      return BaseTable::tableInfo (getFullName(tableName));
    }

    String getFullName (const String& tableName)
    {
      // See if a subtable is given using ::.
      String tabName;
      std::pair<Table,String> t = findParentTable (tableName);
      if (t.first.isNull()) {
        tabName = Path(tableName).absoluteName();
      } else {
        tabName = t.first.keywordSet().tableAttributes(t.second).name();
      }
      return tabName;
    }
    
    // Return Table and name of last part.
    std::pair<Table,String> findParentTable (const String& fullName,
                                             const TableLock& lockOpt,
                                             Table::TableOption tabOpt,
                                             const TSMOption& tsmOpt)
    {
      Table tab;
      String lastPart, msg;
      // Split the name on :: to get the main and subtable names.
      const Vector<String> names = stringToVector(fullName, std::regex("::"));
      AlwaysAssert (!names.empty(), AipsError);
      // Check that no empty parts are given.
      if (anyEQ (names, String())) {
        msg = "empty name part given";
      } else if (names.size() > 1) {
        // Subtable given; check if previous parts exist.
        // First check if main table exists.
        if (! Table::isReadable (names[0])) {
          msg = "main table " + names[0] + " does not exist";
        } else {
          tab = Table(names[0], lockOpt, tabOpt, tsmOpt);
          // Get name of last subtable.
          lastPart = names[names.size()-1];
          // Check if all subtables exist, except last one.
          for (uInt i=1; i<names.size()-1; ++i) {
            if (! tab.keywordSet().isDefined(names[i])) {
              msg = "subtable " + names[i] + " is unknown";
              break;
            }
            tab = tab.keywordSet().asTable (names[i]);
          }
        }
      }
      if (! msg.empty()) {
        throw TableError ("Table name " + fullName + " is invalid (" + msg + ')');
      }
      return std::make_pair (tab, lastPart);
    }
    
  } //# NAMESPACE TableUtil - END
} //# NAMESPACE CASACORE - END
