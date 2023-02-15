//# TableParseProject.cc: Class holding the info of a TaQL projection command
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

#include <casacore/tables/TaQL/TableParseProject.h>
#include <casacore/tables/TaQL/TableParseQuery.h>
#include <casacore/tables/TaQL/TableParseGroupby.h>
#include <casacore/tables/TaQL/TableParseUpdate.h>
#include <casacore/tables/TaQL/TableParseUtil.h>
#include <casacore/tables/TaQL/ExprNodeUtil.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/tables/DataMan/DataManInfo.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/LinearSearch.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/ostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  TableParseProject::TableParseProject (const TableParseTableList& tableList)
    : tableList_p     (tableList),
      tableDesc_p     (new TableDesc()),
      nrSelExprUsed_p (0)
  {}

  //# Add a column name to the block of column names.
  //# Only take the part beyond the period.
  //# Extend the block each time. Since there are only a few column names,
  //# this will not be too expensive.
  void TableParseProject::handleColumn (Int stringType,
                                        const String& name,
                                        const TableExprNode& expr,
                                        const String& newName,
                                        const String& newNameMask,
                                        const String& newDtype,
                                        TableParseQuery& tpq)
  {
    if (expr.isNull()  &&  stringType >= 0) {
      // A wildcarded column name is given.
      handleWildColumn (stringType, name);
    } else {
      // A single column is given.
      Int nrcol = columnNames_p.size();
      columnNames_p.resize     (nrcol+1);
      columnNameMasks_p.resize (nrcol+1);
      columnExpr_p.resize      (nrcol+1);
      columnOldNames_p.resize  (nrcol+1);
      columnDtypes_p.resize    (nrcol+1);
      columnKeywords_p.resize  (nrcol+1);
      if (expr.isNull()) {
        // A true column name is given.
        String oldName;
        String str = name;
        Int inx = str.index('.');
        if (inx < 0) {
          oldName = str;
        } else {
          oldName = str.after(inx);
        }
        // Make an expression of the column or keyword name.
        columnExpr_p[nrcol] = handleKeyCol (str, True, tpq);
        if (columnExpr_p[nrcol].getTableInfo().table().isNull()) {
          // A keyword was given which is returned as a constant.
          nrSelExprUsed_p++;
        } else {
          // If a data type or shorthand is given, the column must be handled
          // as an expression.
          // The same is true if the same column is already used. In such a case
          // the user likely wants to duplicate the column with a different name.
          columnOldNames_p[nrcol] = oldName;
          if (!newDtype.empty()  ||  inx >= 0) {
            nrSelExprUsed_p++;
          } else {
            for (Int i=0; i<nrcol; ++i) {
              if (str == columnOldNames_p[i]) {
                nrSelExprUsed_p++;
                break;
              }
            }
          }
          // Get the keywords for this column (to copy unit, etc.)
          TableColumn tabcol(columnExpr_p[nrcol].getTableInfo().table(), oldName);
          columnKeywords_p[nrcol] = tabcol.keywordSet();
        }
      } else {
        // An expression is given.
        columnExpr_p[nrcol] = expr;
        nrSelExprUsed_p++;
      }
      columnDtypes_p[nrcol]    = newDtype;
      columnNames_p[nrcol]     = newName;
      columnNameMasks_p[nrcol] = newNameMask;
      if (newName.empty()) {
        columnNames_p[nrcol] = columnOldNames_p[nrcol];
      }
    }
  }

  //# Handle a wildcarded a column name.
  //# Add or remove to/from the block of column names as needed.
  void TableParseProject::handleWildColumn (Int stringType, const String& name)
  {
    Int nrcol  = columnNames_p.size();
    String str = name.substr(2, name.size()-3);    // remove delimiters
    Bool caseInsensitive = ((stringType & 1) != 0);
    Bool negate          = ((stringType & 2) != 0);
    Regex regex;
    int shInx = -1;
    // See if the wildcarded name has a table shorthand in it.
    String shorthand;
    if (name[0] == 'p') {
      if (!negate) {
        shInx = str.index('.');
        if (shInx >= 0) {
          shorthand = str.before(shInx);
          str       = str.after(shInx);
        }
      }
      regex = Regex::fromPattern (str);
    } else {
      if (!negate) {
        shInx = str.index("\\.");
        if (shInx >= 0) {
          shorthand = str.before(shInx);
          str       = str.after(shInx+1);
        }
      }
      if (name[0] == 'f') {
        regex = Regex(str);
      } else {
        // For regex type m prepend and append .* unless begin or end regex is given.
        if (str.size() > 0  &&  str[0] != '^') {
          str = ".*" + str;
        }
        if (str.size() > 0  &&  str[str.size()-1] != '$') {
          str = str + ".*";
        }
        regex = Regex(str);
      }
    }
    if (!negate) {
      // Find all matching columns.
      Table tab = tableList_p.findTable(shorthand, False).table();
      if (tab.isNull()) {
        throw TableInvExpr("Shorthand " + shorthand + " in wildcarded column " +
                           name + " not defined in FROM clause");
      }
      Vector<String> columns = tab.tableDesc().columnNames();
      // Add back the delimiting . if a shorthand is given.
      if (shInx >= 0) {
        shorthand += '.';
      }
      Int nr = 0;
      for (uInt i=0; i<columns.size(); ++i) {
        String col = columns[i];
        if (caseInsensitive) {
          col.downcase();
        }
        if (col.matches(regex)) {
          ++nr;
        } else {
          columns[i] = String();
        }
      }
      // Add them to the list of column names.
      columnNames_p.resize     (nrcol+nr);
      columnNameMasks_p.resize (nrcol+nr);
      columnExpr_p.resize      (nrcol+nr);
      columnOldNames_p.resize  (nrcol+nr);
      columnDtypes_p.resize    (nrcol+nr);
      columnKeywords_p.resize  (nrcol+nr);
      for (uInt i=0; i<columns.size(); ++i) {
        if (! columns[i].empty()) {
          // Add the shorthand to the name, so negation takes that into account.
          columnNames_p[nrcol++] = shorthand + columns[i];
        }
      }
    } else {
      // Negation of wildcard, thus remove columns if matching.
      // If the negated wildcard is the first one, assume * was given before it.
      if (nrcol == 0) {
        handleWildColumn (0, "p/*/");
        nrcol = columnNames_p.size();
      }
      // This is done until the last non-wildcarded column name.
      while (nrcol > 0) {
        --nrcol;
        if (! columnExpr_p[nrcol].isNull()) {
          break;
        }
        String col = columnNames_p[nrcol];
        if (!col.empty()) {
          if (caseInsensitive) {
            col.downcase();
          }
          if (col.matches(regex)) {
            columnNames_p[nrcol] = String();
          }
        }
      }
    }
  }

  //# Finish the additions to the block of column names
  //# by removing the deleted empty names and creating Expr objects as needed.
  Table TableParseProject::handleColumnFinish (Bool distinct,
                                               Bool hasResultSet,
                                               TableParseQuery& tpq)
  {
    // Remove the deleted column names.
    // Create Expr objects for the wildcarded names.
    Int nrcol = columnNames_p.size();
    if (nrcol > 0) {
      if (hasResultSet) {
        throw TableInvExpr("Expressions can be given in SELECT or GIVING, "
                           "not both");
      }
      Block<String> names(nrcol);
      Block<String> nameMasks(nrcol);
      Block<String> oldNames(nrcol);
      Block<TableExprNode> exprs(nrcol);
      Block<String> dtypes(nrcol);
      Block<TableRecord> keywords(nrcol);
      Int nr = 0;
      for (Int i=0; i<nrcol; ++i) {
        if (! (columnExpr_p[i].isNull()  &&  columnNames_p[i].empty())) {
          names[nr]     = columnNames_p[i];
          nameMasks[nr] = columnNameMasks_p[i];
          oldNames[nr]  = columnOldNames_p[i];
          exprs[nr]     = columnExpr_p[i];
          dtypes[nr]    = columnDtypes_p[i];
          keywords[nr]  = columnKeywords_p[i];
          // Create an Expr object if needed.
          if (exprs[nr].isNull()) {
            // That can only be the case if no old name is filled in.
            AlwaysAssert (oldNames[nr].empty(), AipsError);
            String name = names[nr];
            Int j = name.index('.');
            if (j >= 0) {
              name = name.after(j);
            }
            // Make an expression of the column name.
            exprs[nr]    = handleKeyCol (name, False, tpq);
            names[nr]    = name;
            oldNames[nr] = name;
            // Get the keywords for this column (to copy unit, etc.)
            TableColumn tabcol(exprs[nr].getTableInfo().table(), name);
            keywords[nr] = tabcol.keywordSet();
          }
          ++nr;
        }
      }
      names.resize    (nr, True);
      oldNames.resize (nr, True);
      exprs.resize    (nr, True);
      dtypes.resize   (nr, True);
      keywords.resize (nr, True);
      columnNames_p     = names;
      columnNameMasks_p = nameMasks;
      columnOldNames_p  = oldNames;
      columnExpr_p      = exprs;
      columnDtypes_p    = dtypes;
      columnKeywords_p  = keywords;
    }
    if (distinct  &&  columnNames_p.empty()) {
      throw TableInvExpr ("SELECT DISTINCT can only be given with at least "
                          "one column name");
    }
    // Make (empty) new table if select expressions were given.
    // This table is used when output columns are used in ORDERBY or HAVING.
    if (nrSelExprUsed_p > 0) {
      return makeProjectExprTable (tpq);
    }
    return Table();
  }

  //# Add a column specification.
  void TableParseProject::handleColSpec (const String& colName,
                                       const String& likeColName,
                                       const String& dtstr,
                                       const Record& spec,
                                       Bool isCOrder)
  {
    // Check if specific column info is given.
    DataType dtype = TpOther;
    Int options = 0;
    Int ndim = -1;
    IPosition shape;
    String dmType;
    String dmGroup;
    String comment;
    Vector<String> unit;
    TableRecord keywords;
    // See if the column is like another column.
    if (likeColName.empty()) {
      AlwaysAssert (! dtstr.empty(), AipsError);
    } else {
      // Use the description of the LIKE column.
      std::pair<ColumnDesc,Record> cdr = findColumnInfo (likeColName, colName);
      const ColumnDesc& cd = cdr.first;
      dtype = cd.dataType();
      options = cd.options();
      if (cd.isArray()) {
        ndim = cd.ndim();
      }
      shape = cd.shape();
      dmType = cd.dataManagerType();
      dmGroup = cd.dataManagerGroup();
      comment = cd.comment();
      keywords = cd.keywordSet();
      if (keywords.isDefined ("QuantumUnits")) {
        unit.reference (cd.keywordSet().asArrayString ("QuantumUnits"));
      }
      // Merge its dminfo into the overall one.
      DataManInfo::mergeInfo (dminfo_p, cdr.second);
    }
    if (! dtstr.empty()) {
      dtype = makeDataType (TpOther, dtstr, colName);
    }
    // Get the possible specifications (which override the LIKE column).
    for (uInt i=0; i<spec.nfields(); i++) {
      String name = spec.name(i);
      name.upcase();
      if (name == "NDIM") {
        ndim = spec.asInt(i);
      } else if (name == "SHAPE") {
        Vector<Int> ivec(spec.toArrayInt(i));
        Int nd = ivec.size();
        shape.resize (nd);
        if (isCOrder) {
          for (Int i=0; i<nd; ++i) {
            shape[i] = ivec[nd-i-1];
          }
        } else {
          shape = IPosition(ivec);
        }
        if (ndim < 0) {
          ndim = 0;
        }
      } else if (name == "DIRECT") {
        if (spec.asInt(i) == 1) {
          options = 1;
        }
      } else if (name == "DMTYPE") {
        dmType = spec.asString(i);
      } else if (name == "DMGROUP") {
        dmGroup = spec.asString(i);
      } else if (name == "COMMENT") {
        comment = spec.asString(i);
      } else if (name == "UNIT") {
        if (spec.dataType(i) == TpString) {
          unit.reference (Vector<String>(1, spec.asString(i)));
        } else {
          unit.reference (spec.asArrayString(i));
        }
      } else {
        throw TableInvExpr ("TableParseProject::handleColSpec - "
                            "column specification field name " + name +
                            " is unknown");
      }
    }
    // Now add the scalar or array column description.
    addColumnDesc (*tableDesc_p, dtype, colName, options, ndim, shape,
                   dmType, dmGroup, comment, keywords, unit, Record());
    Int nrcol = columnNames_p.size();
    columnNames_p.resize (nrcol+1);
    columnNames_p[nrcol] = colName;
  }

  void TableParseProject::handleAddCol (const Record& dmInfo, Table& table)
  {
    // Merge the given dminfo into the dataman-info of the columns.
    DataManInfo::mergeInfo (dminfo_p, dmInfo);
    DataManInfo::finalizeMerge (*tableDesc_p, dminfo_p);
    DataManInfo::adaptNames (dminfo_p, table);
    if (dminfo_p.empty()) {
      StandardStMan ssm;
      table.addColumn (*tableDesc_p, ssm);
    } else {
      table.addColumn (*tableDesc_p, dminfo_p);
    }
  }

  Table TableParseProject::makeProjectExprTable (TableParseQuery& tpq)
  {
    // Make a column description for all expressions.
    // Check if all tables involved have the same nr of rows as the first one.
    TableDesc td;
    for (uInt i=0; i<columnExpr_p.size(); i++) {
      // If no new name is given, make one (unique).
      String newName = columnNames_p[i];
      if (newName.empty()) {
        String nm = "Col_" + String::toString(i+1);
        Int seqnr = 0;
        newName = nm;
        Bool unique = False;
        while (!unique) {
          unique = True;
          for (uInt i=0; i<columnNames_p.size(); i++) {
            if (newName == columnNames_p[i]) {
              unique = False;
              seqnr++;
              newName = nm + "_" + String::toString(seqnr);
              break;
            }
          }
        }
        columnNames_p[i] = newName;
      }
      DataType dtype = makeDataType (columnExpr_p[i].dataType(),
                                     columnDtypes_p[i], columnNames_p[i]);
      addColumnDesc (td, dtype, newName, 0,
                     columnExpr_p[i].isScalar() ? -1:0,    //ndim
                     IPosition(), "", "", "",
                     columnKeywords_p[i],
                     Vector<String>(1, columnExpr_p[i].unit().getName()),
                     columnExpr_p[i].attributes());
      if (! columnNameMasks_p[i].empty()) {
        addColumnDesc (td, TpBool, columnNameMasks_p[i], 0,
                       columnExpr_p[i].isScalar() ? -1:0,    //ndim
                       IPosition(), "", "", "",
                       TableRecord(), Vector<String>(), Record());
      }
    }
    // Create the table.
    return tpq.createTable (td, 0, dminfo_p,
                            std::vector<const Table*>(),
                            std::vector<TableParseQuery*>());
  }

  void TableParseProject::makeProjectExprSel()
  {
    // Create/initialize the block of indices of projected columns used
    // elsewhere.
    projectExprSelColumn_p.resize (columnNames_p.size());
    std::fill (projectExprSelColumn_p.begin(),
               projectExprSelColumn_p.end(), False);
    // Set to True for the used columns.
    uInt ncol = 0;
    for (uInt i=0; i<projectExprSubset_p.size(); ++i) {
      AlwaysAssert (projectExprSubset_p[i] < projectExprSelColumn_p.size(),
                    AipsError);
      if (! projectExprSelColumn_p[projectExprSubset_p[i]]) {
        projectExprSelColumn_p[projectExprSubset_p[i]] = True;
        ncol++;
      }
    }
    // Resize the subset vector. It is not really used anymore, but the
    // tracing shows its size as the nr of pre-projected columns.
    projectExprSubset_p.resize (ncol, True);
  }

  void TableParseProject::initDescriptions (const TableDesc& desc,
                                            const Record& dminfo)
  {
    tableDesc_p = std::make_shared<TableDesc>(desc);
    dminfo_p    = dminfo;
  }

  DataType TableParseProject::makeDataType (DataType dtype, const String& dtstr,
                                            const String& colName)
  {
    if (! dtstr.empty()) {
      if (dtstr == "B") {
        if (dtype != TpOther  &&  dtype != TpBool) {
          throw TableInvExpr ("Expression of column " + colName +
                              " does not have data type Bool");
        }
        return TpBool;
      }
      if (dtstr == "S") {
        if (dtype != TpOther  &&  dtype != TpString) {
          throw TableInvExpr ("Expression of column " + colName +
                              " does not have data type String");
        }
        return TpString;
      }
      if (dtype == TpBool  ||  dtype == TpString) {
        throw TableInvExpr ("Expression of column " + colName +
                            " does not have a numeric data type");
      }
      // Any numeric data type can be converted to Complex.
      if (dtstr == "C4") {
        return TpComplex;
      } else if (dtstr == "C8") {
        return TpDComplex;
      }
      // Real numeric data types cannot have a complex value.
      if (dtype == TpComplex  ||  dtype == TpDComplex) {
        throw TableInvExpr ("Expression of column " + colName +
                            " does not have a real numeric data type");
      }
      if (dtstr == "U1") {
        return TpUChar;
      } else if (dtstr == "I2") {
        return TpShort;
      } else if (dtstr == "U2") {
        return TpUShort;
      } else if (dtstr == "I4") {
        return TpInt;
      } else if (dtstr == "U4") {
        return TpUInt;
      } else if (dtstr == "I8") {
        return TpInt64;
      } else if (dtstr == "R4") {
        return TpFloat;
      } else if (dtstr == "R8") {
        return TpDouble;
      } else if (dtstr == "EPOCH") {
        return TpQuantity;
      }
      throw TableInvExpr ("Datatype " + dtstr + " of column " + colName +
                          " is invalid");
    }
    if (dtype == TpOther) {
      throw TableInvExpr ("Datatype " + dtstr + " of column " + colName +
                          " is invalid (maybe a set with incompatible units)");
    }
    return dtype;
  }

  void TableParseProject::addColumnDesc (TableDesc& td,
                                       DataType dtype,
                                       const String& colName,
                                       Int options,
                                       Int ndim, const IPosition& shape,
                                       const String& dmType,
                                       const String& dmGroup,
                                       const String& comment,
                                       const TableRecord& keywordSet,
                                       const Vector<String>& unitName,
                                       const Record& attributes)
  {
    if (ndim < 0) {
      switch (dtype) {
      case TpBool:
        td.addColumn (ScalarColumnDesc<Bool> (colName, comment,
                                              dmType, dmGroup, options));
        break;
      case TpUChar:
        td.addColumn (ScalarColumnDesc<uChar> (colName, comment,
                                               dmType, dmGroup, 0, options));
        break;
      case TpShort:
        td.addColumn (ScalarColumnDesc<Short> (colName, comment,
                                               dmType, dmGroup, 0, options));
        break;
      case TpUShort:
        td.addColumn (ScalarColumnDesc<uShort> (colName, comment,
                                                dmType, dmGroup, 0, options));
        break;
      case TpInt:
        td.addColumn (ScalarColumnDesc<Int> (colName, comment,
                                             dmType, dmGroup, 0, options));
        break;
      case TpUInt:
        td.addColumn (ScalarColumnDesc<uInt> (colName, comment,
                                              dmType, dmGroup, 0, options));
        break;
      case TpInt64:
        td.addColumn (ScalarColumnDesc<Int64> (colName, comment,
                                               dmType, dmGroup, 0, options));
        break;
      case TpFloat:
        td.addColumn (ScalarColumnDesc<Float> (colName, comment,
                                               dmType, dmGroup, options));
        break;
      case TpDouble:
      case TpQuantity:
        td.addColumn (ScalarColumnDesc<Double> (colName, comment,
                                                dmType, dmGroup, options));
        break;
      case TpComplex:
        td.addColumn (ScalarColumnDesc<Complex> (colName, comment,
                                                 dmType, dmGroup, options));
        break;
      case TpDComplex:
        td.addColumn (ScalarColumnDesc<DComplex> (colName, comment,
                                                  dmType, dmGroup, options));
        break;
      case TpString:
        td.addColumn (ScalarColumnDesc<String> (colName, comment,
                                                dmType, dmGroup, options));
        break;
      default:
        AlwaysAssert (False, AipsError);
      }
    } else {
      // Giving a shape means fixed shape arrays.
      if (shape.size() > 0) {
        options |= ColumnDesc::FixedShape;
      }
      switch (dtype) {
      case TpBool:
        td.addColumn (ArrayColumnDesc<Bool> (colName, comment,
                                             dmType, dmGroup,
                                             shape, options, ndim));
        break;
      case TpUChar:
        td.addColumn (ArrayColumnDesc<uChar> (colName, comment,
                                              dmType, dmGroup,
                                              shape, options, ndim));
        break;
      case TpShort:
        td.addColumn (ArrayColumnDesc<Short> (colName, comment,
                                              dmType, dmGroup,
                                              shape, options, ndim));
        break;
      case TpUShort:
        td.addColumn (ArrayColumnDesc<uShort> (colName, comment,
                                               dmType, dmGroup,
                                               shape, options, ndim));
        break;
      case TpInt:
        td.addColumn (ArrayColumnDesc<Int> (colName, comment,
                                            dmType, dmGroup,
                                            shape, options, ndim));
        break;
      case TpUInt:
        td.addColumn (ArrayColumnDesc<uInt> (colName, comment,
                                             dmType, dmGroup,
                                             shape, options, ndim));
        break;
      case TpInt64:
        td.addColumn (ArrayColumnDesc<Int64> (colName, comment,
                                              dmType, dmGroup,
                                              shape, options, ndim));
        break;
      case TpFloat:
        td.addColumn (ArrayColumnDesc<Float> (colName, comment,
                                              dmType, dmGroup,
                                              shape, options, ndim));
        break;
      case TpDouble:
      case TpQuantity:
        td.addColumn (ArrayColumnDesc<Double> (colName, comment,
                                               dmType, dmGroup,
                                               shape, options, ndim));
        break;
      case TpComplex:
        td.addColumn (ArrayColumnDesc<Complex> (colName, comment,
                                                dmType, dmGroup,
                                                shape, options, ndim));
        break;
      case TpDComplex:
        td.addColumn (ArrayColumnDesc<DComplex> (colName, comment,
                                                 dmType, dmGroup,
                                                 shape, options, ndim));
        break;
      case TpString:
        td.addColumn (ArrayColumnDesc<String> (colName, comment,
                                               dmType, dmGroup,
                                               shape, options, ndim));
        break;
      default:
        AlwaysAssert (False, AipsError);
      }
    }
    // Write the keywords.
    ColumnDesc& cd = td.rwColumnDesc(colName);
    TableRecord keys (keywordSet);
    keys.merge (TableRecord(attributes),
                RecordInterface::OverwriteDuplicates);
    // If no keys defined for this column, define Epoch measure for dates.
    // This is done in the same way as the TableMeasures do.
    if (dtype == TpQuantity  &&  keys.empty()) {
      TableRecord r;
      r.define ("type", "epoch");
      r.define ("Ref", "UTC");
      keys.defineRecord ("MEASINFO", r);
    }
    cd.rwKeywordSet() = keys;
    // Write unit in column keywords (in TableMeasures compatible way).
    // Check if it is valid by constructing the Unit object.
    Vector<String> unit(unitName);
    if (dtype == TpQuantity  &&  (unit.empty()  ||  unit[0].empty())) {
      unit.reference (Vector<String>(1, "d"));
    }
    if (! unit.empty()  &&  ! unit[0].empty()) {
      if (! shape.empty()) {
        if (! (unit.size() == 1  ||  unit.size() == uInt(shape[0]))) {
          throw AipsError("Nr of units must be 1 or match the first axis");
        }
      }
      cd.rwKeywordSet().define ("QuantumUnits", unit);
    }
  }

  std::pair<ColumnDesc,Record> TableParseProject::findColumnInfo
  (const String& colName, const String& newColName) const
  {
    String columnName, shorthand;
    Vector<String> fieldNames;
    if (TableParseUtil::splitName (shorthand, columnName, fieldNames,
                                   colName, True, False, True)) {
      throw TableInvExpr ("Column name " + colName + " is a keyword, no column");
    }
    Table tab = tableList_p.findTable (shorthand, True).table();
    if (tab.isNull()) {
      throw TableInvExpr("Shorthand " + shorthand + " has not been defined");
    }
    Record dminfo = tab.dataManagerInfo();
    // Try to find the column in the info.
    // If found, create a dminfo record for this column only.
    Record dmrec;
    for (uInt i=0; i<dminfo.nfields(); ++i) {
      Record dm(dminfo.subRecord(i));
      if (dm.isDefined("COLUMNS")) {
        Vector<String> cols(dm.asArrayString("COLUMNS"));
        if (std::find(cols.begin(), cols.end(), columnName) != cols.end()) {
          dm.define ("COLUMNS", Vector<String>(1, newColName));
          dmrec.defineRecord (0, dm);
          break;
        }
      }
    }
    return std::make_pair (tab.tableDesc().columnDesc(columnName), dmrec);
  }


  void TableParseProject::checkTableProjSizes() const
  {
    // Check if all main tables used in non-constant select expressions
    // have the same size as the first table.
    // Note: the first table is a main table (not a join table).
    rownr_t nrow = tableList_p.firstTable().nrow();
    for (uInt i=0; i<columnExpr_p.size(); i++) {
      if (! columnExpr_p[i].getRep()->isConstant()) {
        std::vector<Table> tabs =
          TableExprNodeUtil::getNodeTables (columnExpr_p[i].getRep().get(), True);
        for (const Table& tab : tabs) {
          if (tab.nrow() != nrow) {
            throw TableInvExpr("Nr of rows of tables used in select "
                               "expressions must be equal to first table");
          }
        }
      }
    }
  }

  //# Lookup a field name in the table for which the shorthand is given.
  //# If no shorthand is given, use the first table.
  //# The shorthand and name are separated by a period.
  TableExprNode TableParseProject::handleKeyCol (const String& name, Bool tryProj,
                                                 TableParseQuery& tpq)
  {
    //# Split the name into optional shorthand, column, and optional keyword.
    String shand, columnName;
    Vector<String> fieldNames;
    Bool hasKey = TableParseUtil::splitName (shand, columnName, fieldNames,
                                             name, True, False, False);
    //# Use first table if there is no shorthand given.
    //# Otherwise find the table at the current level (no WITH tables).
    TableParsePair tabPair = tableList_p.findTable (shand, False);
    Table tab = tabPair.table();
    if (tab.isNull()) {
      throw (TableInvExpr("Shorthand " + shand + " has not been defined in FROM clause"));
      return 0;
    }
    //# If :: is not given, we have a column or keyword.
    if (!hasKey) {
      if (tryProj && shand.empty() && fieldNames.empty()) {
        // Only the column name is given; so first try if the column is
        // a new name of a projected column. It can also be a column created
        // from the mask of a masked array.
        Bool found;
        Int inx = linearSearchBrackets (found, columnNames_p, columnName,
                                        columnNames_p.size());
        if (!found) {
          inx = linearSearchBrackets (found, columnNameMasks_p, columnName,
                                      columnNameMasks_p.size());
        }
        if (found) {
          // If a table resulting from projection is used, take column from it
          // if it exists in it.
          const Table& projectExprTable = tpq.projectExprTable();
          if (!projectExprTable.isNull()  &&
              projectExprTable.tableDesc().isColumn (columnName)) {
            uInt nc = projectExprSubset_p.size();
            projectExprSubset_p.resize (nc+1);
            projectExprSubset_p[nc] = inx;
            return projectExprTable.col (columnName);
          } else if (!columnOldNames_p.empty()  &&
                     !columnOldNames_p[inx].empty()) {
            // Possibly the column is renamed, so use the old name.
            columnName = columnOldNames_p[inx];
          }
        }
      }
      // If it is a column, check if all tables used have the same size.
      // Note: the projected table (used above) should not be checked.
      // Don't do that for join tables.
      if (tab.tableDesc().isColumn (columnName)  &&  tabPair.joinIndex() < 0) {
        if (firstColTable_p.isNull()) {
          firstColTable_p = tab;
          firstColName_p  = name;
        } else {
          if (tab.nrow() != firstColTable_p.nrow()) {
            throw TableInvExpr ("Nr of rows (" + String::toString(tab.nrow()) +
                                ") in table column " + name +
                                " differs from column "+ firstColName_p + " (" +
                                String::toString(firstColTable_p.nrow()) + ')');
          }
        }
      }
      // Create column or keyword node.
      try {
        TableExprNode node(TableExprNode::keyCol (tabPair.getTableInfo(),
                                                  columnName, fieldNames));
        Bool isJoin = False;
        if (tabPair.joinIndex() >= 0) {
          // See if it is a column expr; it could have been a keyword.
          if (node.getRep()->exprType() == TableExprNodeRep::Variable) {
            node = TaQLJoinColumn::makeColumnNode (node.getRep(),
                                                   tpq.joins()[tabPair.joinIndex()]);
            isJoin = True;
          }
        }
        if (!isJoin) {
          tpq.addApplySelNode (node);
        }
        return node;
      } catch (const TableError&) {
        throw TableInvExpr(name + " is an unknown column (or keyword) in table "
                           + tab.tableName());
      }
    }
    //# If no column name, we have a table keyword.
    if (columnName.empty()) {
      return tab.key (fieldNames);
    }
    //# Otherwise we have a column keyword.
    TableColumn col (tab, columnName);
    return TableExprNode::newKeyConst (col.keywordSet(), fieldNames);
  }

  Table TableParseProject::project (const Table& tab)
  {
    // First do projection using the original column names.
    Table tabp = tab.project (columnOldNames_p);
    for (uInt i=0; i<columnNames_p.size(); i++) {
      // Rename column if new name is given to a column.
      if (columnNames_p[i] != columnOldNames_p[i]) {
        tabp.renameColumn (columnNames_p[i], columnOldNames_p[i]);
      }
    }
    return tabp;
  }

  void TableParseProject::makeUpdate (Bool useSel, TableParseQuery& tpq)
  {
    for (uInt i=0; i<columnExpr_p.size(); i++) {
      if (! columnExpr_p[i].isNull()) {
        if (projectExprSelColumn_p[i] == useSel) {
          tpq.addUpdate (new TableParseUpdate (columnNames_p[i],
                                               columnNameMasks_p[i],
                                               columnExpr_p[i], False));
        }
      }
    }
  }

  void TableParseProject::getAggrNodes (std::vector<TableExprNodeRep*>& aggr) const
  {
    for (uInt i=0; i<columnExpr_p.size(); ++i) {
      std::vector<TableExprNodeRep*> nodes =
        TableExprNodeUtil::getAggrNodes (columnExpr_p[i].getRep().get());
      aggr.insert (aggr.end(), nodes.begin(), nodes.end());
    }
  }

  void TableParseProject::setUpdateNames
  (std::vector<CountedPtr<TableParseUpdate>>& upd)
  {
    for (uInt i=0; i<upd.size(); i++) {
      upd[i]->setColumnName     (columnNames_p[i]);
      upd[i]->setColumnNameMask (columnNameMasks_p[i]);
    }
  }
  
  void TableParseProject::setColumnNames
    (const std::vector<CountedPtr<TableParseUpdate>>& upd)
  {
    columnNames_p.resize (upd.size());
    for (uInt i=0; i<upd.size(); i++) {
      columnNames_p[i] = upd[i]->columnName();
    }
    columnNameMasks_p.resize (columnNames_p.size());
  }

  void TableParseProject::setStoredColumns()
  {
    columnNames_p = TableParseUtil::getStoredColumns (tableList_p.firstTable());
    columnNameMasks_p.resize (columnNames_p.size());
  }

  void TableParseProject::checkCountColumns() const
  {
    if (columnExpr_p.empty()) {
      throw TableInvExpr ("No COUNT columns given");
    }
    for (uInt i=0; i<columnExpr_p.size(); i++) {
      TableParseGroupby::checkAggrFuncs (columnExpr_p[i]);
      if (!columnExpr_p[i].isScalar()) {
        throw TableInvExpr ("COUNT column " + columnNames_p[i] + " is not scalar");
      }
    }
  }
  
} //# NAMESPACE CASACORE - END
