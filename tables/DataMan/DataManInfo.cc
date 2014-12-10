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
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

void DataManInfo::removeHypercolumns (TableDesc& tabDesc)
{
  tabDesc.adjustHypercolumns (SimpleOrderedMap<String,String>(String()));
}

void DataManInfo::adjustDesc (TableDesc& tdesc, const Record& dminfo)
{
  // Find out the columns and data manager groups of the fields.
  SimpleOrderedMap<String,String> dmTypeMap("", tdesc.ncolumn());
  SimpleOrderedMap<String,String> dmGroupMap("", tdesc.ncolumn());
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
	dmTypeMap(cols[j]) = dmType;
	dmGroupMap(cols[j]) = dmGroup;
      }
    }
  }
  // Exit if no columns in dminfo.
  if (dmTypeMap.ndefined() == 0) {
    return;
  }
  // Change data manager type and group as needed.
  for (uInt i=0; i<tdesc.ncolumn(); i++) {
    ColumnDesc& cdesc = tdesc.rwColumnDesc(i);
    const String& name = cdesc.name();
    String* v = dmTypeMap.isDefined (name);
    if (v) {
      if (! v->empty()) {
	cdesc.dataManagerType() = *v;
      }
    }
    v = dmGroupMap.isDefined (name);
    if (v) {
      if (! v->empty()) {
	cdesc.dataManagerGroup() = *v;
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

} //# NAMESPACE CASACORE - END

