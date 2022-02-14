//# DataManInfo.cc: Class with static functions to manipulate a datamanager info record
//# Copyright (C) 2001,2002,2003,2009
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
#include <casacore/tables/DataMan/DataManInfo.h>
#include <casacore/tables/DataMan/DataManAccessor.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>
#include <map>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

void DataManInfo::removeHypercolumns (TableDesc& tabDesc)
{
  tabDesc.adjustHypercolumns (std::map<String,String>());
}

void DataManInfo::adjustDesc (TableDesc& tdesc, const Record& dminfo)
{
  // Find out the columns and data manager groups of the fields.
  std::map<String,String> dmTypeMap;
  std::map<String,String> dmGroupMap;
  for (uInt i=0; i<dminfo.nfields(); i++) {
    const Record& sub = dminfo.asRecord (i);
    if (sub.isDefined("COLUMNS")) {
      String dmType = "";
      String dmGroup = "";
      if (sub.isDefined("TYPE")) {
	dmType = sub.asString ("TYPE");
      }
      if (sub.isDefined("NAME")) {
	dmGroup = sub.asString ("NAME");
      }
      Vector<String> cols = sub.asArrayString ("COLUMNS");
      for (uInt j=0; j<cols.nelements(); j++) {
	dmTypeMap.insert (std::make_pair(cols[j], dmType));
        dmGroupMap.insert (std::make_pair(cols[j], dmGroup));
      }
    }
  }
  // Exit if no columns in dminfo.
  if (dmTypeMap.empty()) {
    return;
  }
  // Change data manager type and group as needed.
  for (uInt i=0; i<tdesc.ncolumn(); i++) {
    ColumnDesc& cdesc = tdesc.rwColumnDesc(i);
    const String& name = cdesc.name();
    std::map<String,String>::iterator iter1 = dmTypeMap.find (name);
    if (iter1 != dmTypeMap.end()) {
      String v = iter1->second;
      if (! v.empty()) {
	cdesc.dataManagerType() = v;
      }
    }
    std::map<String,String>::iterator iter2 = dmGroupMap.find (name);
    if (iter2 != dmTypeMap.end()) {
      String v = iter2->second;
      if (! v.empty()) {
	cdesc.dataManagerGroup() = v;
      }
    }
  }
  // Remove hypercolumn definitions which are different from
  // data manager group in the column descriptions.
  Vector<String> hcNames = tdesc.hypercolumnNames();
  for (uInt i=0; i<hcNames.nelements(); i++) {
    Vector<String> dataNames, coordNames, idNames;
    tdesc.hypercolumnDesc (hcNames[i], dataNames, coordNames, idNames);
    Bool same = True;
    for (uInt j=0; j<dataNames.nelements(); j++) {
      const ColumnDesc& cdesc = tdesc[dataNames[j]];
      if (cdesc.dataManagerGroup() != hcNames[i]) {
	same = False;
	break;
      }
    }
    if (same) {
      for (uInt j=0; j<coordNames.nelements(); j++) {
	const ColumnDesc& cdesc = tdesc[dataNames[j]];
	if (cdesc.dataManagerGroup() != hcNames[i]) {
	  same = False;
	  break;
	}
      }
    }
    if (same) {
      for (uInt j=0; j<idNames.nelements(); j++) {
	const ColumnDesc& cdesc = tdesc[dataNames[j]];
	if (cdesc.dataManagerGroup() != hcNames[i]) {
	  same = False;
	  break;
	}
      }
    }
    if (!same) {
      tdesc.removeHypercolumnDesc (hcNames[i]);
    }
  }
}

void DataManInfo::adjustTSM (TableDesc& tabDesc, Record& dminfo)
{
  Vector<String> dataNames, coordNames, idNames;
  // Keep track of hypercolumns to be changed.
  Vector<String> hcChange;
  uInt nrhc = 0;
  // Loop through all hypercolumn descriptions.
  Vector<String> hcNames = tabDesc.hypercolumnNames();
  for (uInt i=0; i<hcNames.nelements(); i++) {
    // Find the hypercolumn in the dminfo.
    // If found, adjust if needed.
    for (uInt j=0; j<dminfo.nfields(); j++) {
      const Record& rec = dminfo.subRecord(j);
      if (rec.asString("NAME") == hcNames(i)) {
	if (rec.asString("TYPE") == "TiledDataStMan") {
	  // Replace TiledDataStMan by TiledShapeStMan.
	  Record& rwrec = dminfo.rwSubRecord(j);
	  rwrec.define("TYPE", "TiledShapeStMan");
	  // Get hypercolumn description.
	  tabDesc.hypercolumnDesc (hcNames(i), dataNames,
				   coordNames, idNames);
	  uInt nrid = idNames.nelements();
	  if (nrid > 0) {
	    // The hypercolumn definition contains ID columns, so it
	    // has to be changed later in the TableDesc.
	    hcChange.resize (nrhc+1, True);
	    hcChange(nrhc++) = hcNames(i);
	    // Keep the dminfo columns which are not an ID column.
	    Vector<String> colNames = rec.asArrayString("COLUMNS");
	    Vector<String> colsout(colNames.nelements());
	    uInt nrout = 0;
	    for (uInt k=0; k<colNames.nelements(); k++) {
	      Bool found = False;
	      for (uInt k1=0; k1<idNames.nelements(); k1++) {
		if (colNames(k) == idNames(k1)) {
		  found = True;
		  break;
		}
	      }
	      if (!found) {
		colsout(nrout++) = colNames(k);
	      }
	    }
	    colsout.resize (nrout, True);
	    rwrec.define ("COLUMNS", colsout);
	  }
	}	  
	break;
      }
    }
  }
  if (nrhc > 0) {
    tabDesc.removeIDhypercolumns (hcChange);
  }
}

Record DataManInfo::adjustStMan (const Record& dminfo, const String& dmType,
                                 Bool replaceMSM)
{
  Record newdm;
  for (uInt j=0; j<dminfo.nfields(); j++) {
    Record rec = dminfo.subRecord(j);
    // Get the data manager name and create an object for it.
    String exName = rec.asString("NAME");
    String exType = rec.asString("TYPE");
    DataManager* dmptr = DataManager::getCtor(exType) (exName, Record());
    if ((dmptr->isStorageManager()  &&
         !(dmptr->canAddRow()  ||  dmptr->isRegular()))  ||
        (replaceMSM  &&  exType == "MemoryStMan")) {
      // A non-writable storage manager; use given storage manager instead.
      rec.define ("TYPE", dmType);
      rec.define ("NAME", exName);
    }
    delete dmptr;
    newdm.defineRecord (j, rec);
  }
  return newdm;
}

void DataManInfo::mergeInfo (Record& dminfo1, const Record& dminfo2)
{
  // See for each new data manager what to do.
  for (uInt i2=0; i2<dminfo2.nfields(); ++i2) {
    Record dm2 = dminfo2.subRecord(i2);
    String type2 (dm2.isDefined("TYPE")  ?  dm2.asString("TYPE") : String());
    String name2 (dm2.isDefined("NAME")  ?  dm2.asString("NAME") : String());
    // Add the data manager to the first, but overwrite if already there.
    uInt dmindex1 = dminfo1.nfields();
    for (uInt i1=0; i1<dminfo1.nfields(); ++i1) {
      const Record& dm1 = dminfo1.subRecord(i1);
      // An empty or undefined type/name means use the other.
      String type1 (dm1.isDefined("TYPE")  ?  dm1.asString("TYPE") : String());
      String name1 (dm1.isDefined("NAME")  ?  dm1.asString("NAME") : String());
      if (type1.empty()) {
        type1 = type2;
      } else if (type2.empty()) {
        type2 = type1;
      }
      if (name1.empty()) {
        name1 = name2;
      } else if (name2.empty()) {
        name2 = name1;
      }
      if (type1==type2 && name1==name2) {
        dmindex1 = i1;
        // Use the old specs if undefined in new one.
        if (!dm2.isDefined("SPEC")  &&  dm1.isDefined("SPEC")) {
          dm2.defineRecord ("SPEC", dm1.subRecord("SPEC"));
        }
        mergeColumns (dminfo1, dmindex1, dm2);
        break;
      }
    }
    // Define the new dm
    dminfo1.defineRecord (dmindex1, dm2);
  }
}

void DataManInfo::mergeColumns (Record& dminfo, uInt dmindex, Record& dm)
{
  // Get the columns given in the new dm.
  Vector<String> cols;
  if (dm.isDefined("COLUMNS")) {
    cols.reference (dm.asArrayString("COLUMNS"));
  }
  if (! cols.empty()) {
    // Iterate over all dm-s to find the ones containing columns of the new dm.
    for (uInt i=0; i<dminfo.nfields(); ++i) {
      Record dm2 = dminfo.subRecord(i);
      if (dm2.isDefined("COLUMNS")) {
        Vector<String> cols2(dm2.asArrayString("COLUMNS"));
        if (! cols2.empty()) {
          std::vector<String> colsnew;
          // Keep columns not equal to a column in the new dm.
          for (auto col2 : cols2) {
            if (std::find (cols.begin(), cols.end(), col2) == cols.end()) {
              colsnew.push_back (col2);
            }
          }
          if (i != dmindex) {
            // dm is not the new one, so update the COLUMNS in it (if changed).
            if (colsnew.size() != cols2.size()) {
              dm2.define ("COLUMNS", Vector<String>(colsnew));
              dminfo.defineRecord (i, dm2);
            }
          } else {
            // dm is the new one. So add its columns and redefine them.
            colsnew.insert (colsnew.end(), cols.begin(), cols.end());
            dm.define ("COLUMNS", Vector<String>(colsnew));
          }
        }
      }
    }
  }
}

Record DataManInfo::finalizeMerge (const TableDesc& desc, const Record& dminfo)
{
  // Make a map of the data managers in the dminfo record, so possible
  // specifications can be used.
  // Also make a map of column to dminfo index.
  std::map<std::pair<String,String>, uInt> dmMap;
  std::map<String, uInt> colMap;
  for (uInt i=0; i<dminfo.nfields(); ++i) {
    const Record& dm = dminfo.subRecord(i);
    String type;
    if (dm.isDefined("TYPE")) {
      type = dm.asString ("TYPE");
    }
    if (! type.empty()) {
      String name;
      if (dm.isDefined("NAME")) {
        name = dm.asString ("NAME");
      }
      dmMap[std::make_pair(type,name)] = i;
      if (dm.isDefined("COLUMNS")) {
        Vector<String> cols(dm.asArrayString("COLUMNS"));
        for (auto col : cols) {
          colMap[col] = i;
        }
      }
    }
  }
  // Find out which columns share the same data manager by making a map
  // of data manager type/name to columns in the Table Description.
  std::map<std::pair<String,String>, std::vector<String>> descMap;
  for (uInt i=0; i<desc.ncolumn(); ++i) {
    const ColumnDesc& cd = desc[i];
    // Take the data manager type and name from dminfo if defined there.
    // Use type StandardStMan if none is given.
    String type = cd.dataManagerType();
    String name = cd.dataManagerGroup();
    auto iter = colMap.find (cd.name());
    if (iter != colMap.end()) {
      // The column is defined in dminfo; get name and type.
      const Record& dm = dminfo.subRecord(iter->second);
      if (dm.isDefined("TYPE")) {
        type = dm.asString("TYPE");
      }
      if (dm.isDefined("NAME")) {
        name = dm.asString("NAME");
      }
    }
    if (type.empty()) {
      type = "StandardStMan";
    }
    // Add the column to the vector.
    descMap[std::make_pair(type, name)].push_back (cd.name());
  }
  // Create a dminfo entry for each column set found above.
  // Use dm parameters if found.
  Record newdm;
  for (auto desc : descMap) {
    String type = desc.first.first;
    String name = desc.first.second;
    Record dm;
    // Try to find this type/name in the dminfo map to copy its specs.
    // Define the columns.
    auto iter = dmMap.find (std::make_pair(type, name));
    if (iter != dmMap.end()) {
      dm = dminfo.subRecord(iter->second);
    }
    dm.define ("COLUMNS", Vector<String>(desc.second));
    // Use the first column name one for a dm entry without a name.
    if (name.empty()) {
      name = desc.second[0];
    }
    // Ensure the name is unique.
    dm.define ("TYPE", type);
    dm.define ("NAME", uniqueName (newdm, name));
    // Add the the overall dminfo record.
    newdm.defineRecord (newdm.size(), dm);
  }
  return newdm;
}

void DataManInfo::makeUniqueNames (Record& dminfo)
{
  // Ensure that data manager names are unique by adding a suffix if needed.
  // Empty names are initially set to the name of the first column.
  // Start at the back, so the oldest entries keep their name.
  for (uInt i=dminfo.nfields(); i>0;) {
    --i;
    Record& dm = dminfo.rwSubRecord(i);
    String origName (dm.isDefined("NAME")  ?  dm.asString("NAME") : String());
    String name(origName);
    if (name.empty()) {
      name = "DM";      // use default DM in case no columns are defined
      if (dm.isDefined("COLUMNS")) {
        Vector<String> cols(dm.asArrayString("COLUMNS"));
        if (! cols.empty()) {
          name = cols[0];
        }
      }
    }
    // Make the name unique if needed.
    String newName = uniqueName (dminfo, name, i);
    if (newName != origName) {
      dm.define ("NAME", newName);
    }
  }
}

String DataManInfo::uniqueName (const Record& dminfo, const String& name,
                                Int excludeField)
{
  String newName = name;
  uInt suffix = 0;
  Bool unique = False;
  while (!unique) {
    unique = True;
    for (uInt i=0; i<dminfo.nfields(); ++i) {
      if (Int(i) != excludeField) {
        const Record& dm = dminfo.subRecord(i);
        if (dm.isDefined("NAME")) {
          String nm = dm.asString("NAME");
          if (newName == nm) {
            // Not unique, so add increased suffix and try again.
            newName = name + '_' + String::toString(++suffix);
            unique = False;
            break;
          }
        }
      }
    }
  }
  return newName;
}

void DataManInfo::adaptNames (Record& dminfo, const Table& tab)
{
  Record dmtab = tab.dataManagerInfo();
  for (uInt i=0; i<dminfo.nfields(); ++i) {
    Record& subinfo = dminfo.rwSubRecord(i);
    if (subinfo.isDefined("NAME")  &&  subinfo.isDefined("COLUMNS")) {
      Vector<String> cols(subinfo.asArrayString("COLUMNS"));
      if (! cols.empty()) {
        String name = subinfo.asString(i);
        for (uInt j=0; j<dmtab.nfields(); ++j) {
          const Record& subtab = dmtab.subRecord(j);
          if (subtab.isDefined("NAME")  &&  subtab.asString("NAME") == name) {
            // Add column name to DM name to make it unique.
            subinfo.define ("NAME", name + '_' + cols[0]);
          }
        }
      }
    }
  }
}

Vector<String> DataManInfo::removeDminfoColumns (Record& dminfo,
                                                 const Vector<String>& columns,
                                                 const String& keepType)
{
  Record newdm;
  // Find the given columns and remove them.
  // Keep track which columns are removed.
  Vector<String> remCols(columns.size());
  uInt ncols = 0;
  for (uInt j=0; j<dminfo.nfields(); j++) {
    Record rec = dminfo.subRecord(j);
    Vector<String> dmcols (rec.asArrayString("COLUMNS"));
    uInt ndmcol = dmcols.size();
    const String& dmtype = rec.asString ("TYPE");
    if (keepType.empty()  ||  dmtype.substr(0,keepType.size()) != keepType) {
      // This dmtype does not need to be kept, so columns can be removed.
      for (uInt i=0; i<columns.size(); ++i) {
        const String& col = columns[i];
        for (uInt j=0; j<ndmcol; ++j) {
          if (col == dmcols[j]) {
            // Column name matches, so remove it.
            // Add it to the vector of removed columns.
            remCols[ncols++] = col;
            --ndmcol;
            for (uInt k=j; k<ndmcol; ++k) {
              dmcols[k] = dmcols[k+1];
            }
          }
        }
      }
    }
    // Only use the dm if there are columns left.
    if (ndmcol > 0) {
      if (ndmcol != dmcols.size()) {
        dmcols.resize (ndmcol, True);
        rec.define ("COLUMNS", dmcols);
      }
      newdm.defineRecord (j, rec);
    }
  }
  dminfo = newdm;
  remCols.resize (ncols, True);
  return remCols;
}

void DataManInfo::setTiledStMan (Record& dminfo, const Vector<String>& columns,
                                 const String& dmType, const String& dmName,
                                 const IPosition& defaultTileShape)
{
  // Remove the columns.
  Vector<String> remCols (removeDminfoColumns (dminfo, columns, "Tiled"));
  // Add removed columns with a TiledStMan.
  if (remCols.size() > 0) {
    Record dm;
    dm.define("TYPE", dmType);
    dm.define("NAME", dmName);
    dm.define ("COLUMNS", remCols);
    Record spec;
    spec.define("DEFAULTTILESHAPE", defaultTileShape.asVector());
    dm.defineRecord ("SPEC", spec);
    dminfo.defineRecord (dminfo.nfields(), dm);
  }
}

void DataManInfo::showDataManStats (const Table& tab, ostream& os)
{
  Record dmInfo = tab.dataManagerInfo();
  // Loop through all data managers.
  // Not all of them might have a name, so use the first column in
  // each of them to construct the Accessor object.
  for (uInt i=0; i<dmInfo.nfields(); ++i) {
    String col = dmInfo.subRecord(i).asArrayString("COLUMNS").data()[0];
    RODataManAccessor acc(tab, col, True);
    os << "  Statistics for column " << col << " e.a.: ";
    Int64 pos = os.tellp();
    acc.showCacheStatistics (os);
    if (os.tellp() == pos) {
      // Nothing written, thus end the line.
      os << endl;
    }
  }
}


} //# NAMESPACE CASACORE - END

