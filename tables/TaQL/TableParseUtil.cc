//# TableParseUtil.cc: Convenience functions for TableParse classes
//# Copyright (C) 1994-2022
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

//# Includes
#include <casacore/tables/TaQL/TableParseUtil.h>
#include <casacore/tables/TaQL/TableParseTableList.h>
#include <casacore/tables/TaQL/TableParseQuery.h>
#include <casacore/tables/TaQL/ExprDerNodeArray.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
  namespace TableParseUtil
  {
    // Handle a table name and create a Table object for it as needed.
    // This is quite complex, because a table name can be given in many ways:
    // 1. an ordinary name such as 'my.tab'
    // 2. a wildcarded name (for table concatenation) such as 'my*.tab'. Note that for
    //    such a case the alwaysOpen=False, so no Table object is created.
    // 3. a table number in the temporary table list such as $1
    // 4. a shorthand referring to another table at this or a higher query level
    // 5. :: or . indicating the first available table at this or a higher query level
    // 6. a Table object resulting from a nested query
    // 7. a subtable indicated by a keyword such as tabspec::sub or tabspec::sub1::sub2
    //    where tabspec can be a table name as in 1, 3, 4 or 5 above.
    //    - the subtable can be a table keyword as above, but also a column keyword
    //      such as shorthand.column::key. Note that a column can only be given after
    //      a shorthand to distinguish it from an ordinary table name.
    //      The first part before a . is tried as a shorthand and can be empty indicating
    //      the first available table as in 5.
    //    - keywords can be nested thus tab::key1.key2.key3
    //      It means that sh.col::a1.a2.s1::b1.s2::c1.c2.c3.s4 is a valid specification
    //      and indicates subtable s4 in subtable s3 in subtable s1 using nested
    //      keywords in column col. But this example is very esoteric.
    // In practice column keywords and nested keywords will hardly ever be used,
    // so usually something like my.ms::ANTENNA is the only 'complicated' spec used.
    Table getTable (Int tabnr, const String& name,
                    const Table& ftab,
                    const std::vector<const Table*>& tempTables,
                    const std::vector<TableParseQuery*>& stack,
                    Bool alwaysOpen)
    {
      // A table from a nested query.
      if (! ftab.isNull()) {
        return ftab;
      }
      // Split the name into its subtable parts using :: as separator.
      Table table;
      uInt stSub  = 0;
      uInt stPart = 0;
      Vector<String> subs = stringToVector(name, std::regex("::"));
      // No part, except first one, can be empty (unless :: is given).
      if (name != "::") {
        if (subs.size() == 0  ||
            (subs.size() > 1  &&  anyEQ(subs(Slice(1, subs.size()-1)), String()))) {
          throw TableInvExpr("'"+ name + "' is an invalid table name specification");
        }
      }
      // Split the first subtable name into parts using a dot as separator.
      // The first part can be empty, a shorthand or a temporary table number.
      // An empty part means the first available table.
      stPart = 1;          // indicate first part is handled.
      Vector<String> parts = stringToVector(subs[0], '.');
      if (parts.size() == 0  ||  parts[0].empty()) {
        table = TableParseTableList::findTable (String(), True, stack).table();
        if (table.isNull()) {
          throw TableInvExpr(":: or . is invalid in table name " + name +
                             ": no previous table available");
        }
      } else {
        if (tabnr >= 0) {
          // Temporary table number (1-based) given.
          if (tabnr < 1  ||  tabnr > Int(tempTables.size())
              ||  tempTables[tabnr-1] == 0) {
            throw (TableInvExpr ("Invalid temporary table number given in " + name));
          }
          table = *(tempTables[tabnr-1]);
        } else {
          // See if the first part is a shorthand.
          table = TableParseTableList::findTable (parts[0], True, stack).table();
          if (table.isNull()) {
            // It was not something like shorthand.column, thus try as a full name.
            // However, do not open if alwaysOpen=False.
            // In that case the table is opened when needed
            // (because the name can contain a wildcard as well).
            if (!alwaysOpen) {
              return table;
            }
            stPart = 0;
            stSub = 1;
            table = Table (subs[0]);
            if (table.isNull()) {
              throw TableInvExpr("Table " + subs[0] + " is unknown");
            }
          }
        }
      }
      // Okay; we have found the first table.
      AlwaysAssert (!table.isNull(), AipsError);
      // Now process all parts in all subtable names, where the first name or
      // first part might need to be skipped because already processed.
      const TableRecord* keywords = &(table.keywordSet());
      for (uInt k=stSub; k<subs.size(); ++k) {
        Vector<String> parts = stringToVector(subs[k], '.');
        for (uInt p=stPart; p<parts.size(); ++p) {
          // See if the first part is a column name. If so, it must be the last part
          // in this name, but not be the last name.
          if (k<subs.size()-1  &&  stPart==parts.size()-1  &&
              table.tableDesc().isColumn(parts[p])) {
            keywords = &(table.tableDesc()[parts[p]].keywordSet());
          } else if (subs[k] != ".") {
            // . indicates first available table.
            // The last keyword must be a Table; the others nested TableRecords.
            Int fieldNr = keywords->fieldNumber(parts[p]);
            if (fieldNr < 0) {
              throw TableInvExpr(parts[p] + " is an unknown keyword/subtable" +
                                 (p==1 && k<subs.size()-1 ? " (or column)" : "") +
                                 " in " + name);
            } else {
              DataType dtype = keywords->dataType (fieldNr);
              if (p == parts.size()-1) {
                if (dtype != TpTable) {
                  throw TableInvExpr(parts[p] + " is no table keyword in " + name);
                }
                table = keywords->asTable (fieldNr);
                keywords = &(table.keywordSet());
              } else {
                if (dtype != TpRecord) {
                  throw TableInvExpr(parts[p] + " is no record keyword in " + name);
                }
                keywords = &(keywords->subRecord (fieldNr));
              }
            }
          }
        }
        stPart = 0;
      }
      return table;
    }

    // This function can split a name.
    // The name can consist of an optional shorthand, a column or keyword name,
    // followed by zero or more subfield names (separated by dots).
    // In the future it should also be possible to have a subfield name
    // followed by a keyword name, etc. to cater for something like:
    //   shorthand::key.subtable::key.subsubtable::key.
    // If that gets possible, TableGram.ll should also be changed to accept
    // such a string in the scanner.
    // It is a question whether :: should be part of the scanner or grammar.
    // For columns one can go a bit further by accepting something like:
    //  col.subtable[select expression resulting in scalar]
    // which is something for the far away future.
    Bool splitName (String& shorthand, String& columnName,
                    Vector<String>& fieldNames,
                    const String& name,
                    Bool checkError,
                    Bool isKeyword,
                    Bool allowNoKey)
    {
      //# Make a copy, because some String functions are non-const.
      //# Usually the name consists of a columnName only, so use that.
      //# A keyword is given if :: is part of the name or if isKeyword is set.
      shorthand = "";
      columnName = name;
      String restName;
      Bool isKey = isKeyword;
      int j = columnName.index("::");
      Vector<String> fldNam;
      uInt stfld = 0;
      if (j >= 0) {
        // The name contains ::, thus represents a keyword name.
        isKey = True;
      } else if (isKey) {
        // It is a keyword, but no ::.
        j = -2;
      }
      if (isKey) {
        // There should be something after the ::
        // which can be multiple names separated by dots.
        // They represent the keyword name and possible subfields in case
        // the keyword is a record.
        restName = columnName.after(j+1);
        if (!allowNoKey && restName.empty()) {
          if (checkError) {
            throw (TableInvExpr ("No keyword given in name " + name));
          }
          return False;
        }
        fldNam = stringToVector (restName, '.');
        // The part before the :: can be empty, an optional shorthand,
        // and an optional column name (separated by a dot).
        if (j <= 0) {
          columnName = "";
        } else {
          Vector<String> scNames = stringToVector(columnName.before(j), '.');
          switch (scNames.size()) {
          case 2:
            shorthand = scNames(0);
            columnName = scNames(1);
            break;
          case 1:
            columnName = scNames(0);
            break;
          default:
            if (checkError) {
              throw TableInvExpr ("Name " + name + " is invalid: More"
                                  " than 2 name parts given before ::");
            }
            return False;
          }
        }
      } else {
        // The name is a column name optionally preceeded by a shorthand
        // and optionally followed by subfields in case the column contains
        // records. The separator is a dot.
        // A name like a.b is in principle ambiguous because:
        // - it can be shorthand.column
        // - it can be column.subfield
        // It is assumed to be a shorthand.
        // Users can use column.subfield by preceeding it with a dot
        // (.a.b always means column.subfield).
        fldNam = stringToVector (columnName, '.');
        if (fldNam.size() == 1) {
          stfld = 0;                      // one part simply means column
        } else if (fldNam(0).empty()) {
          stfld = 1;                      // .column was used
        } else {
          shorthand = fldNam(0);      // a known shorthand is used
          stfld = 1;
        }
        columnName = fldNam(stfld++);
        if (columnName.empty()) {
          if (checkError) {
            throw (TableInvExpr ("No column given in name " + name));
          }
          return False;
        }
      }
      fieldNames.resize (fldNam.size() - stfld);
      for (uInt i=stfld; i<fldNam.size(); i++) {
        if (fldNam(i).empty()) {
          if (checkError) {
            throw (TableInvExpr ("Name " + name +
                                 " has empty field names"));
          }
          return False;
        }
        fieldNames(i-stfld) = fldNam(i);
      }
      return isKey;
    }

    Table openParentTable (const String& fullName,
                           const String& subTableName,
                           const std::vector<const Table*>& tempTables,
                           const std::vector<TableParseQuery*>& stack)
    {
      // Remove ::subtableName from the full table name to get the parent's name.
      String tableName (fullName.substr(0,
                                        fullName.size() - subTableName.size() - 2));
      // Open the parent table.
      Table parent = getTable (-1, tableName, Table(), tempTables, stack, True);
      // Create the subtable and define the keyword in the parent referring it.
      String parentName = parent.tableName();
      if (parentName.empty()) {
        throw TableInvExpr("Parent table in " + fullName + " seems to be transient");
      }
      return parent;
    }

    void setRecFld (RecordInterface& rec,
                    const String& name,
                    const String& dtype,
                    const ValueHolder& vh)
    {
      String type = getTypeString (dtype, vh.dataType());
      if (isScalar(vh.dataType())) {
        if (type == "B") {
          rec.define (name, vh.asBool());
        } else if (type == "U1") {
          rec.define (name, vh.asuChar());
        } else if (type == "U4") {
          rec.define (name, vh.asuInt());
        } else if (type == "I2") {
          rec.define (name, vh.asShort());
        } else if (type == "I4") {
          rec.define (name, vh.asInt());
        } else if (type == "I8") {
          rec.define (name, vh.asInt64());
        } else if (type == "R4") {
          rec.define (name, vh.asFloat());
        } else if (type == "R8") {
          rec.define (name, vh.asDouble());
        } else if (type == "C4") {
          rec.define (name, vh.asComplex());
        } else if (type == "C8") {
          rec.define (name, vh.asDComplex());
        } else if (type == "S") {
          rec.define (name, vh.asString());
        } else {
          throw TableInvExpr ("TableParse::setRecFld - "
                              "unknown data type " + type);
        }
      } else {
        if (type == "B") {
          rec.define (name, vh.asArrayBool());
        } else if (type == "U1") {
          rec.define (name, vh.asArrayuChar());
        } else if (type == "U4") {
          rec.define (name, vh.asArrayuInt());
        } else if (type == "I2") {
          rec.define (name, vh.asArrayShort());
        } else if (type == "I4") {
          rec.define (name, vh.asArrayInt());
        } else if (type == "I8") {
          rec.define (name, vh.asArrayInt64());
        } else if (type == "R4") {
          rec.define (name, vh.asArrayFloat());
        } else if (type == "R8") {
          rec.define (name, vh.asArrayDouble());
        } else if (type == "C4") {
          rec.define (name, vh.asArrayComplex());
        } else if (type == "C8") {
          rec.define (name, vh.asArrayDComplex());
        } else if (type == "S") {
          rec.define (name, vh.asArrayString());
        } else {
          throw TableInvExpr ("TableParse::setRecFld - "
                              "unknown data type " + type);
        }
      }
    }

    String getTypeString (const String& typeStr, DataType type)
    {
      String out = typeStr;
      if (out.empty()) {
        switch (type) {
        case TpBool:
        case TpArrayBool:
          out = "B";
          break;
        case TpUChar:
        case TpArrayUChar:
          out = "U1";
          break;
        case TpUShort:
        case TpArrayUShort:
          out = "U2";          // github.com/ICRAR/skuareview
          break;
        case TpUInt:
        case TpArrayUInt:
          out = "U4";
          break;
        case TpShort:
        case TpArrayShort:
          out = "I2";
          break;
        case TpInt:
        case TpArrayInt:
          out = "I4";
          break;
        case TpInt64:
        case TpArrayInt64:
          out = "I8";
          break;
        case TpFloat:
        case TpArrayFloat:
          out = "R4";
          break;
        case TpDouble:
        case TpArrayDouble:
          out = "R8";
          break;
        case TpComplex:
        case TpArrayComplex:
          out = "C4";
          break;
        case TpDComplex:
        case TpArrayDComplex:
          out = "C8";
          break;
        case TpString:
        case TpArrayString:
          out = "S";
          break;
        default:
          throw TableInvExpr ("TableParse::getTypeString - "
                              "value has an unknown data type " +
                              String::toString(type));
        }
      }
      return out;
    }

    Block<String> getStoredColumns (const Table& tab)
    {
      Block<String> names;
      const TableDesc& tdesc = tab.tableDesc();
      for (uInt i=0; i<tdesc.ncolumn(); i++) {
        const String& colnm = tdesc[i].name();
        if (tab.isColumnStored(colnm)) {
          uInt inx = names.size();
          names.resize (inx + 1);
          names[inx] = colnm;
        }
      }
      return names;
    }

    TableExprNode getColSet (const Table& table)
    {
      // Check if there is only one column.
      const TableDesc& tableDesc = table.tableDesc();
      if (tableDesc.ncolumn() != 1) {
        throw (TableInvExpr ("Nested query should select 1 column"));
      }
      const ColumnDesc& colDesc = tableDesc.columnDesc(0);
      TableColumn tabcol (table, colDesc.name());
      TENShPtr tsnptr;
      if (colDesc.isScalar()) {
        switch (colDesc.dataType()) {
        case TpBool:
          tsnptr = new TableExprNodeArrayConstBool
            (ScalarColumn<Bool>(tabcol).getColumn());
          break;
        case TpUChar:
          tsnptr = new TableExprNodeArrayConstInt
            (ScalarColumn<uChar>(tabcol).getColumn());
          break;
        case TpShort:
          tsnptr = new TableExprNodeArrayConstInt
            (ScalarColumn<Short>(tabcol).getColumn());
          break;
        case TpUShort:
          tsnptr = new TableExprNodeArrayConstInt
            (ScalarColumn<uShort>(tabcol).getColumn());
          break;
        case TpInt:
          tsnptr = new TableExprNodeArrayConstInt
            (ScalarColumn<Int>(tabcol).getColumn());
          break;
        case TpUInt:
          tsnptr = new TableExprNodeArrayConstInt
            (ScalarColumn<uInt>(tabcol).getColumn());
          break;
        case TpInt64:
          tsnptr = new TableExprNodeArrayConstInt
            (ScalarColumn<Int64>(tabcol).getColumn());
          break;
        case TpFloat:
          tsnptr = new TableExprNodeArrayConstDouble
            (ScalarColumn<Float>(tabcol).getColumn());
          break;
        case TpDouble:
          tsnptr = new TableExprNodeArrayConstDouble
            (ScalarColumn<Double>(tabcol).getColumn());
          break;
        case TpComplex:
          tsnptr = new TableExprNodeArrayConstDComplex
            (ScalarColumn<Complex>(tabcol).getColumn());
          break;
        case TpDComplex:
          tsnptr = new TableExprNodeArrayConstDComplex
            (ScalarColumn<DComplex>(tabcol).getColumn());
          break;
        case TpString:
          tsnptr = new TableExprNodeArrayConstString
            (ScalarColumn<String>(tabcol).getColumn());
          break;
        default:
          throw (TableInvExpr ("Nested query column " + colDesc.name() +
                               " has unknown data type"));
        }
      } else {
        switch (colDesc.dataType()) {
        case TpBool:
          tsnptr = new TableExprNodeArrayConstBool
            (ArrayColumn<Bool>(tabcol).getColumn());
          break;
        case TpUChar:
          tsnptr = new TableExprNodeArrayConstInt
            (ArrayColumn<uChar>(tabcol).getColumn());
          break;
        case TpShort:
          tsnptr = new TableExprNodeArrayConstInt
            (ArrayColumn<Short>(tabcol).getColumn());
          break;
        case TpUShort:
          tsnptr = new TableExprNodeArrayConstInt
            (ArrayColumn<uShort>(tabcol).getColumn());
          break;
        case TpInt:
          tsnptr = new TableExprNodeArrayConstInt
            (ArrayColumn<Int>(tabcol).getColumn());
          break;
        case TpUInt:
          tsnptr = new TableExprNodeArrayConstInt
            (ArrayColumn<uInt>(tabcol).getColumn());
          break;
        case TpInt64:
          tsnptr = new TableExprNodeArrayConstInt
            (ArrayColumn<Int64>(tabcol).getColumn());
          break;
        case TpFloat:
          tsnptr = new TableExprNodeArrayConstDouble
            (ArrayColumn<Float>(tabcol).getColumn());
          break;
        case TpDouble:
          tsnptr = new TableExprNodeArrayConstDouble
            (ArrayColumn<Double>(tabcol).getColumn());
          break;
        case TpComplex:
          tsnptr = new TableExprNodeArrayConstDComplex
            (ArrayColumn<Complex>(tabcol).getColumn());
          break;
        case TpDComplex:
          tsnptr = new TableExprNodeArrayConstDComplex
            (ArrayColumn<DComplex>(tabcol).getColumn());
          break;
        case TpString:
          tsnptr = new TableExprNodeArrayConstString
            (ArrayColumn<String>(tabcol).getColumn());
          break;
        default:
          throw (TableInvExpr ("Nested query column " + colDesc.name() +
                               " has unknown data type"));
        }
      }
      //# Fill in the column unit (if defined).
      tsnptr->setUnit (TableExprNodeColumn::getColumnUnit (tabcol));
      return tsnptr;
    }

  }  // end namespace TableParseUtil
  
} //# NAMESPACE CASACORE - END
