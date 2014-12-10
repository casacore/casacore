//# TableDesc.cc: Description of a table
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

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TabPath.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableAttr.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# This is the implementation of the class TableDesc.
//#
//# It uses the class TabPath to find which directory contains
//# the table description file.

//# Define the prefix to use for keywords representing hypercolumns.
static const String theHyperPrefix ("Hypercolumn_");


TableDesc::TableDesc()
: option_p (Scratch)
{
    init (TabPath());                      // use default search path
}

TableDesc::TableDesc (const String& nam, TDOption opt)
: name_p     (nam),
  option_p   (opt)
{
    init (TabPath());                      // use default search path
}

TableDesc::TableDesc (const String& nam, const String& version,
		      TDOption opt)
: name_p     (nam),
  vers_p     (version),
  option_p   (opt)
{
    init (TabPath());                      // use default search path
}

TableDesc::TableDesc (const String& nam, const String& version,
		      const TabPath& tdpath, TDOption opt)
: name_p     (nam),
  vers_p     (version),
  option_p   (opt)
{
    init (tdpath);                           // use given search path
}

TableDesc::TableDesc (const TableDesc& td, const String& nam,
		      const String& version, TDOption opt, Bool copyColumns)
: name_p     (nam),
  vers_p     (version),
  option_p   (opt)
{
    copy (td, TabPath(), copyColumns);       // use default search path
}

TableDesc::TableDesc (const TableDesc& td, const String& nam,
		      const String& version, const TabPath& tdpath,
		      TDOption opt, Bool copyColumns)
: name_p     (nam),
  vers_p     (version),
  option_p   (opt)
{
    copy (td, tdpath, copyColumns);          // use given search path
}

TableDesc::TableDesc (const TableDesc& td, TDOption opt)
: option_p   (opt)
{
    copy (td, TabPath(), True);              // use default search path
}


TableDesc::~TableDesc ()
{
    //# Write if the description if opened for output and if the
    //# description can be written.
    //# Delete will be done by destructor of AipsIO.
    if (swwrite_p) {
	if (option_p == Update  ||  option_p == New
        ||  option_p == NewNoReplace) {
	    putFile (iofil_p, TableAttr());         // write the description
	}                             
    }
    iofil_p.close ();
    delete key_p;
    delete privKey_p;
}


// <thrown>
//   <li> TableDescNoName
//   <li> TableDuplFile
//   <li> TableNoFile
//   <li> TableInvOpt
// </thrown>
void TableDesc::init (const TabPath& tdpath)
{
    //# Initialize some variables.
    swwrite_p = False;                         // writing is not possible yet
    //# If non-scratch, check if name is not blank and look if the
    //# description already exists.
    if (option_p == Scratch) {
	dir_p = "**SCRATCH**";
    }else{
	if (name_p.empty()) {
	    throw TableDescNoName();
	}
        Bool exsw = tdpath.found (name_p + ".tabdsc", dir_p);
        if (option_p == NewNoReplace) {
	    if (exsw) {
		throw (TableDuplFile("desc. " + name_p));// table already exists
	    }
	}else{
	    if (option_p != New  &&  !exsw) {
		throw (TableNoFile("desc." + name_p));   // table does not exist
	    }
	}
    }

    //# Determine the AipsIO option for this file.
    ByteIO::OpenOption fopt = ByteIO::Old;
    switch (option_p) {
    case TableDesc::Old:
	fopt = ByteIO::Old;
	break;
    case TableDesc::New:
	fopt = ByteIO::New;
	break;
    case TableDesc::NewNoReplace:
	fopt = ByteIO::NewNoReplace;
	break;
    case TableDesc::Scratch:
	fopt = ByteIO::Scratch;
	break;
    case TableDesc::Update:
	fopt = ByteIO::Update;
	break;
    case TableDesc::Delete:
	fopt = ByteIO::Delete;
	break;
    default:
	throw (TableInvOpt ("TableDesc",
	        "must be Old, New, NewNoReplace, Scratch, Update or Delete"));
    }

    //# Allocate the keyword sets.
    key_p = new TableRecord();
    privKey_p = new TableRecord();

    //# If non-scratch, open the file. Read it for new, update and delete.
    //# It can be closed immediately when old (i.e readonly).
    if (option_p != Scratch) {
	iofil_p.open (dir_p + name_p + ".tabdsc", fopt);
	if (option_p == Old  ||  option_p == Update  || option_p == Delete) {
	    getFile (iofil_p, TableAttr());                      // read file
	}
	if (option_p == Old  ||  option_p == Update) {
	    iofil_p.close ();
	    if (option_p == Update) {
		iofil_p.open (dir_p + name_p + ".tabdsc", fopt); // reposition
	    }
	}
    }
    swwrite_p = True;                          // writing is possible now
}


// <thrown>
//   <li> TableInvOpt
// </thrown>
void TableDesc::copy (const TableDesc& td, const TabPath& tdpath,
		      Bool copyColumns)
{
    //# Check the options; it has to be a new description.
    if (option_p != New  &&  option_p != NewNoReplace
    &&  option_p != Scratch) {
	throw (TableInvOpt ("TableDesc",
			    "must be New, NewNoReplace or Scratch"));
    }
    init (tdpath);
    if (name_p.empty()) {
	name_p = td.name_p;
    }
    if (vers_p.empty()) {
	vers_p = td.vers_p;
    }
    comm_p = td.comm_p;
    *key_p = *(td.key_p);
    *privKey_p = *(td.privKey_p);
    if (copyColumns) {
        col_p  = td.col_p;
    }
}


// Test if a description exists.
Bool TableDesc::isReadable (const String& tableDescName)
{
    File file(tableDescName + ".tabdsc");
    return file.isReadable();
}


// Get a vector with all column names.
Vector<String> TableDesc::columnNames() const
{
    Vector<String> names(ncolumn());
    for (uInt i=0; i<names.nelements(); i++) {
	names(i) = columnDesc(i).name();
    }
    return names;
}


void TableDesc::add (const TableDesc& that, Bool addKeywordSet)
{
    // First check if all sets are disjoint.
    if (! col_p.isDisjoint (that.col_p)) {
	throw (TableInvOper ("TableDesc::add; columns not disjoint"));
    }
    if (! privKey_p->description().isDisjoint (that.privKey_p->description())){
	throw (TableInvOper ("TableDesc::add; hypercolumns not disjoint"));
    }
    if (addKeywordSet) {
	if (! key_p->description().isDisjoint (that.key_p->description())) {
	    throw (TableInvOper ("TableDesc::add; keywords not disjoint"));
	}
    }
    col_p.add (that.col_p);
    privKey_p->merge (*that.privKey_p, RecordInterface::ThrowOnDuplicates);
    if (addKeywordSet) {
	key_p->merge (*that.key_p, RecordInterface::ThrowOnDuplicates);
    }
}


//# Show the table.
void TableDesc::show () const
{
    show (cout);
}
void TableDesc::show (ostream& os) const
{
    os << endl;
    os << "TableDesc " << name_p;
    os << "   version " << vers_p;
    os << "   (Directory " << dir_p << ")";
    os << endl;
    os << "---------" << endl;
    os << "  Comment: " << comm_p << endl;
    os << "  #Keywords = " << key_p->nfields() << endl;;
    os << key_p->description();
    os << "  #Columns  = " << ncolumn() << endl;;
    os << privKey_p->description();
    col_p.show (os);
}


void TableDesc::putFile (AipsIO& ios, const TableAttr& parentAttr) const
{
    ios.putstart ("TableDesc", 2);
    ios << name_p;
    ios << vers_p;
    ios << comm_p;
    key_p->putRecord (ios, parentAttr);
    ios << *privKey_p;
    col_p.putFile (ios, parentAttr);
    ios.putend ();
}

void TableDesc::getFile (AipsIO& ios, const TableAttr& parentAttr)
{
    uInt tvers = ios.getstart ("TableDesc");
    ios >> name_p;
    ios >> vers_p;
    ios >> comm_p;
    key_p->getRecord (ios, parentAttr);
    // Version 1 does not contain privKey_p.
    if (tvers != 1) {
	ios >> *privKey_p;
    }
    col_p.getFile (ios, parentAttr);
    ios.getend ();
}



//# Rename a column.
void TableDesc::renameColumn (const String& newname,
			      const String& oldname)
{
  // First rename the column itself.
  col_p.rename (newname, oldname);
  // Now adjust the hypercolumn descriptions.
  SimpleOrderedMap<String,String> old2new("", 1);
  // First fill the map with all columns and replace it for the new name.
  for (uInt i=0; i<ncolumn(); i++) {
    const String& nm = columnDesc(i).name();
    old2new.define (nm, nm);
  }
  old2new.define (oldname, newname);
  adjustHypercolumns (old2new);
}

void TableDesc::defineHypercolumn (const String& hypercolumnName,
				   uInt ndim,
				   const Vector<String>& dataColumnNames)
{
    Vector<String> columnNames;
    defineHypercolumn (hypercolumnName, ndim, dataColumnNames,
		       columnNames, columnNames);
}
void TableDesc::defineHypercolumn (const String& hypercolumnName,
				   uInt ndim,
				   const Vector<String>& dataColumnNames,
				   const Vector<String>& coordColumnNames)
{
    Vector<String> columnNames;
    defineHypercolumn (hypercolumnName, ndim, dataColumnNames,
		       coordColumnNames, columnNames);
}

void TableDesc::defineHypercolumn (const String& hypercolumnName,
				   uInt ndim,
				   const Vector<String>& dataColumnNames,
				   const Vector<String>& coordColumnNames,
				   const Vector<String>& idColumnNames)
{
    // Check if data and coordinate columns have been given.
    if (dataColumnNames.nelements() < 1) {
	throwHypercolumn (hypercolumnName, "dataColumnNames is empty");
    }
    if (ndim < 1) {
	throwHypercolumn (hypercolumnName, "ndim < 1");
    }
    uInt ncoord = coordColumnNames.nelements();
    if (ncoord != 0  &&  ncoord != ndim) {
	throwHypercolumn (hypercolumnName,
			  "#coordColumnNames mismatches ndim");
    }
    uInt i, j;
    // Check if the coordinate columns exist and are numeric
    // scalars or vectors. An empty coordinate name is allowed meaning
    // that the axis has no coordinate.
    // Get the number of vectors.
    uInt firstCoordSca = 0;
    uInt lastCoordVec = 0;
    for (i=0; i<ncoord; i++) {
	if (! coordColumnNames(i).empty()) {
	    if (!isColumn (coordColumnNames(i))) {
		throwHypercolumn (hypercolumnName, "coordColumn " +
				  coordColumnNames(i) + " does not exist");
	    }
	    const ColumnDesc& desc = columnDesc (coordColumnNames(i));
	    int dataType = desc.dataType();
	    if (dataType == TpString
	    ||  dataType == TpArrayString
	    ||  dataType == TpBool
	    ||  dataType == TpArrayBool
	    ||  dataType == TpTable
	    ||  dataType == TpRecord
	    ||  dataType == TpOther) {
		throwHypercolumn (hypercolumnName, "coordColumn " +
				  coordColumnNames(i) + " is not numeric");
	    }
	    if (dataType == TpChar   ||  dataType == TpArrayChar
	    ||  dataType == TpUChar  ||  dataType == TpArrayUChar
	    ||  dataType == TpShort  ||  dataType == TpArrayShort
	    ||  dataType == TpUShort ||  dataType == TpArrayUShort) {
		throwHypercolumn (hypercolumnName, "coordColumn " +
				  coordColumnNames(i) +
				  ": (u)Char and (u)Short not supported");
	    }
	    if (desc.isArray()) {
		if (desc.ndim() != 1) {
		    throwHypercolumn (hypercolumnName, "coordColumn " +
				      coordColumnNames(i) +
				      " is not a scalar or vector");
		}
		lastCoordVec = i+1;
	    } else {
		if (firstCoordSca == 0) {
		    firstCoordSca = i+1;
		}
	    }
	}
    }
    if (firstCoordSca > 0  &&  lastCoordVec > firstCoordSca) {
	throwHypercolumn (hypercolumnName,
			 "coordinate vectors have to describe the first axes");
    }
    // Check if the data columns exist and have fixed length
    // (i.e. no String, Table or Other type).
    // Also check if their dimensionality matches the number of
    // coordinate vectors (if coordinates are defined).
    // Find out if all data columns have FixedShape.
    Bool fixedShape = True;
    uInt cellNdim = 0;
    for (i=0; i<dataColumnNames.nelements(); i++) {
	if (!isColumn (dataColumnNames(i))) {
	    throwHypercolumn (hypercolumnName, "dataColumn " +
			      dataColumnNames(i) + " does not exist");
	}
	const ColumnDesc& desc = columnDesc (dataColumnNames(i));
	if (desc.dataType() == TpString
	||  desc.dataType() == TpArrayString
	||  desc.dataType() == TpTable
	||  desc.dataType() == TpOther) {
	    throwHypercolumn (hypercolumnName, "dataColumn " +
			      dataColumnNames(i) +
			      " is not numeric or boolean");
	}
	if (desc.isArray()  &&  desc.ndim() <= 0) {
	    throwHypercolumn (hypercolumnName,
			      "the dimensionality of data column " +
			      dataColumnNames(i) + " is undefined");
	}
	if (cellNdim == 0) {
	    cellNdim = desc.ndim();
	}
	if (Int(cellNdim) != desc.ndim()) {
	    throwHypercolumn (hypercolumnName,
			      "the dimensionality of data column " +
			      dataColumnNames(i) +
			      " mismatches that of previous data columns");
	}
        if (! desc.isFixedShape()) {
	    fixedShape = False;
	}
    }
    if ((firstCoordSca > 0  &&  firstCoordSca <= cellNdim)
    ||  lastCoordVec > cellNdim) {
	throwHypercolumn (hypercolumnName,
			  "the dimensionality of the data columns"
			  " mismatches nr of coordinate columns with a"
			  " vector value");
    }
    // Check if the ID columns exist and are scalars (not type Other).
    // Type (u)Char and (u)Short are not possible.
    for (i=0; i<idColumnNames.nelements(); i++) {
	if (!isColumn (idColumnNames(i))) {
	    throwHypercolumn (hypercolumnName, "idColumn " +
			      idColumnNames(i) + " does not exist");
	}
	const ColumnDesc& desc = columnDesc (idColumnNames(i));
	if (!desc.isScalar()) {
	    throwHypercolumn (hypercolumnName, "idColumn " +
			      idColumnNames(i) + " is not a scalar");
	}
	int dataType = desc.dataType();
	if (dataType == TpOther) {
	    throwHypercolumn (hypercolumnName, "idColumn " +
			      idColumnNames(i) + " has no standard data type");
	}
	if (dataType == TpChar   ||  dataType == TpUChar
	||  dataType == TpShort  ||  dataType == TpUShort) {
	    throwHypercolumn (hypercolumnName, "idColumn " + idColumnNames(i) +
			      ": (u)Char and (u)Short not supported");
	}
    }
    // Check if all names are used only once.
    // Copying them into one vector makes life easier.
    uInt nr = dataColumnNames.nelements() + coordColumnNames.nelements() +
	      idColumnNames.nelements();
    Vector<String> names(nr);
    names(Slice(0,dataColumnNames.nelements())) = dataColumnNames;
    nr = dataColumnNames.nelements();
    names(Slice(nr,coordColumnNames.nelements())) = coordColumnNames;
    nr += coordColumnNames.nelements();
    names(Slice(nr,idColumnNames.nelements())) = idColumnNames;
    nr += idColumnNames.nelements();
    for (i=0; i<nr; i++) {
	if (! names(i).empty()) {
	    for (j=i+1; j<nr; j++) {
		if (names(i) == names(j)) {
		    throwHypercolumn (hypercolumnName, "column name " +
				      names(i) + " is multiply used");
		}
	    }
	}
    }
    // Put all the stuff into a keyword set.
    // This will be attached to a table keyword with name Hypercolumn_"name".
    String keyName = theHyperPrefix + hypercolumnName;
    TableRecord set;
    set.define ("ndim", ndim);
    set.define ("data", dataColumnNames);
    set.define ("coord", coordColumnNames);
    set.define ("id", idColumnNames);
    privKey_p->defineRecord (keyName, set);
    // Set the default data manager to TiledShapeStMan or TiledColumnStMan.
    // (use TiledColumnStMan if all data columns have FixedShape).
    // Set default data manager group to hypercolumn name.
    String dmName = (fixedShape  ?  "TiledColumnStMan" : "TiledShapeStMan");
    for (i=0; i<names.nelements(); i++) {
        if (! names(i).empty()) {
	    ColumnDesc& desc = rwColumnDesc(names(i));
	    desc.dataManagerType() = dmName;
	    desc.dataManagerGroup() = hypercolumnName;
	}
    }
}

void TableDesc::throwHypercolumn (const String& name, const String& message)
{
    throw (TableInvHyperDesc (name, message));
}


Bool TableDesc::isHypercolumn (const String& name) const
{
    return privKey_p->isDefined (theHyperPrefix + name);
}

Vector<String> TableDesc::hypercolumnNames() const
{
    uInt i;
    uInt nhyp = 0;
    uInt nkey = privKey_p->nfields();
    for (i=0; i<nkey; i++) {
	if (privKey_p->type(i) == TpRecord) {
	    const String& key = privKey_p->description().name (i);
	    if (key.index (theHyperPrefix) == 0) {
		nhyp++;
	    }
	}
    }
    Vector<String> result(nhyp);
    if (nhyp > 0) {
	nhyp = 0;
	for (i=0; i<nkey; i++) {
	    if (privKey_p->type(i) == TpRecord) {
		const String& key = privKey_p->description().name (i);
		if (key.index (theHyperPrefix) == 0) {
		    result(nhyp) = String(key).from
			                   (int(theHyperPrefix.length()));
		    nhyp++;
		}
	    }
	}
    }
    return result;
}

uInt TableDesc::hypercolumnDesc (const String& name,
				 Vector<String>& dataColumnNames,
				 Vector<String>& coordColumnNames,
				 Vector<String>& idColumnNames) const
{
    const TableRecord& set = privKey_p->subRecord (theHyperPrefix + name);
    // Make vectors empty, so assignment will be possible.
    dataColumnNames.resize (0);
    coordColumnNames.resize (0);
    idColumnNames.resize (0);
    dataColumnNames = set.asArrayString ("data");
    coordColumnNames = set.asArrayString ("coord");
    idColumnNames = set.asArrayString ("id");
    return set.asuInt ("ndim");
}

void TableDesc::adjustHypercolumns
                        (const SimpleOrderedMap<String, String>& old2new,
			 Bool keepUnknownData, Bool keepUnknownCoord,
			 Bool keepUnknownId)
{
  Vector<String> hcNames = hypercolumnNames();
  Vector<String> dataNames, coordNames, idNames;
  for (uInt i=0; i<hcNames.nelements(); i++) {
    // Get hypercolumn description and delete it.
    uInt ndim = hypercolumnDesc (hcNames(i), dataNames, coordNames, idNames);
    privKey_p->removeField (theHyperPrefix + hcNames(i));
    // Rename/remove columns in the hypercolumn description.
    uInt nr = 0;
    for (uInt j=0; j<dataNames.nelements(); j++) {
      const String* newName = old2new.isDefined (dataNames(j));
      if (newName) {
	dataNames(nr++) = *newName;
      } else if (keepUnknownData) {
	nr++;
      }
    }
    // If no data columns left, there is no need to recreate the hypercolumn.
    if (nr > 0) {
      dataNames.resize (nr, True);
      nr = 0;
      for (uInt j=0; j<coordNames.nelements(); j++) {
	const String* newName = old2new.isDefined (coordNames(j));
	if (newName) {
	  coordNames(nr++) = *newName;
	} else if (keepUnknownCoord) {
	  nr++;
	}
      }
      // All coordinate columns are needed, so removal of one means
      // that they cannot be used anymore.
      // That also means their default storage manager has to be reset.
      if (nr != ndim) {
	for (uInt j=0; j<nr; j++) {
	  rwColumnDesc(coordNames(j)).setDefaultDataManager();
	}
	coordNames.resize (0);
      }
      nr = 0;
      for (uInt j=0; j<idNames.nelements(); j++) {
	const String* newName = old2new.isDefined (idNames(j));
	if (newName) {
	  idNames(nr++) = *newName;
	} else if (keepUnknownId) {
	  nr++;
	}
      }
      idNames.resize (nr, True);
      // Add the hypercolumn again.
      defineHypercolumn (hcNames(i), ndim, dataNames, coordNames, idNames);
    }
  }
}

void TableDesc::removeIDhypercolumns (const Vector<String>& hcNames)
{
  Vector<String> dataNames, coordNames, idNames;
  for (uInt i=0; i<hcNames.nelements(); i++) {
    // Get hypercolumn description and delete it.
    uInt ndim = hypercolumnDesc (hcNames(i), dataNames, coordNames, idNames);
    if (idNames.nelements() > 0) {
      for (uInt j=0; j<idNames.nelements(); j++) {
	ColumnDesc& cd = rwColumnDesc(idNames(j));
	cd.dataManagerType() = "IncrementalStMan";
	cd.dataManagerGroup() = "ISM_TSM";
      }
      privKey_p->removeField (theHyperPrefix + hcNames(i));
      defineHypercolumn (hcNames(i), ndim, dataNames, coordNames);
    }      
  }
}

void TableDesc::removeHypercolumnDesc (const String& hypercolumnName)
{
    if (! isHypercolumn(hypercolumnName)) {
        throw TableError ("Hypercolumn " + hypercolumnName +
			  " does not exist, thus cannot be removed");
    }
    // Remove all hypercolumns; start at the end to avoid invalid indices.
    privKey_p->removeField (theHyperPrefix + hypercolumnName);
}

void TableDesc::renameHypercolumn (const String& newHypercolumnName,
				   const String& hypercolumnName)
{
    if (! isHypercolumn(hypercolumnName)) {
	throw TableError ("Hypercolumn " + hypercolumnName +
			  " does not exist, thus cannot be renamed");
    }  
    if (newHypercolumnName == "") {
        throw TableError ("New hypercolumn name must be non-empty");
    }  
    // Get hypercolumn description
    Vector<String> dataNames, coordNames, idNames;
    uInt ndim = hypercolumnDesc (hypercolumnName, dataNames, coordNames, idNames);
    // delete the hypercolumn
    privKey_p->removeField (theHyperPrefix + hypercolumnName);
    // recreate it under new name (will also change the column descriptions)
    defineHypercolumn (newHypercolumnName, ndim, dataNames, coordNames, idNames);
    
}

} //# NAMESPACE CASACORE - END

