//# MSField.cc: The MeasurementSet FIELD Table
//# Copyright (C) 1996,1998,1999,2000
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

#include <casacore/ms/MeasurementSets/MSField.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColDescSet.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/ForwardCol.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Utilities/Regex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSField::MSField():hasBeenDestroyed_p(True) { }

MSField::MSField(const String &tableName, TableOption option) 
    : MSTable<MSFieldEnums>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSField(String &, TableOption) - "
			 "table is not a valid MSField"));
}

MSField::MSField(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<MSFieldEnums>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSField(String &, String &, TableOption) - "
			 "table is not a valid MSField"));
}

MSField::MSField(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : MSTable<MSFieldEnums>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSField(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid MSField"));
}

MSField::MSField(const Table &table)
    : MSTable<MSFieldEnums>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSField(const Table &) - "
			 "table is not a valid MSField"));
}

MSField::MSField(const MSField &other)
    : MSTable<MSFieldEnums>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSField(const MSField &) - "
			     "table is not a valid MSField"));
}

MSField::~MSField()
{
// check to make sure that this MSField is still valid
    if (!hasBeenDestroyed_p  &&  !validate()) {
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
        LogIO os;
        os << LogIO::WARN
           << "~MSField() - Table written is not a valid MSField"
           << LogIO::POST;
    }
    hasBeenDestroyed_p = True;
}


MSField& MSField::operator=(const MSField &other)
{
    if (&other != this) {
	MSTable<MSFieldEnums>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

MSTableMaps MSField::initMaps()
{
  MSTableMaps maps;
  // the PredefinedColumns
  // CODE
  colMapDef(maps, CODE, "CODE", TpString,
            "Special characteristics of field, "
            "e.g. Bandpass calibrator","","");
  // DELAY_DIR
  colMapDef(maps, DELAY_DIR, "DELAY_DIR", TpArrayDouble,
            "Direction of delay center (e.g. RA, DEC)" 
            "as polynomial in time.","rad","Direction");
  // EPHEMERIS_ID
  colMapDef(maps, EPHEMERIS_ID,"EPHEMERIS_ID", TpInt,
            "Ephemeris id, pointer to EPHEMERIS table","","");
  // FLAG_ROW
  colMapDef(maps, FLAG_ROW, "FLAG_ROW", TpBool,
            "Row Flag","","");
  // NAME
  colMapDef(maps, NAME, "NAME", TpString,
            "Name of this field","","");
  // NUM_POLY
  colMapDef(maps, NUM_POLY, "NUM_POLY", TpInt,
            "Polynomial order of _DIR columns","","");
  // PHASE_DIR 
  colMapDef(maps, PHASE_DIR, "PHASE_DIR", TpArrayDouble,
            "Direction of phase center (e.g. RA, DEC).",
            "rad","Direction");
  // REFERENCE_DIR 
  colMapDef(maps, REFERENCE_DIR, "REFERENCE_DIR", TpArrayDouble,
            "Direction of REFERENCE center (e.g. RA, DEC)."
            "as polynomial in time.","rad","Direction");
  // SOURCE_ID
  colMapDef(maps, SOURCE_ID, "SOURCE_ID", TpInt,
            "Source id","","");
  // TIME
  colMapDef(maps, TIME, "TIME", TpDouble,
            "Time origin for direction and rate","s","Epoch");
  
  // PredefinedKeywords

  // init requiredTableDesc
  // all required keywords
  uInt i;
  for (i = UNDEFINED_KEYWORD+1;
       i <= NUMBER_PREDEFINED_KEYWORDS; i++) {
    addKeyToDesc(maps, PredefinedKeywords(i));
  }
  // all required columns 
  // First define the columns with known dimensionality
  addColumnToDesc(maps, DELAY_DIR, 2);
  addColumnToDesc(maps, PHASE_DIR, 2);
  addColumnToDesc(maps, REFERENCE_DIR, 2);
  // Now define all other columns (duplicates are skipped)
  for (i = UNDEFINED_COLUMN+1; 
       i <= NUMBER_REQUIRED_COLUMNS; i++) {
    addColumnToDesc(maps, PredefinedColumns(i));
  }

  return maps;
}

	
MSField MSField::referenceCopy(const String& newTableName, 
			       const Block<String>& writableColumns) const
{
    return MSField(MSTable<MSFieldEnums>::
		     referenceCopy(newTableName,writableColumns));
}

Bool MSField::addEphemeris(const uInt id, const String& inputEphemTableName,
			   const String& comment){
  Bool rval=False;
  if( (inputEphemTableName.empty() && comment.empty()) 
      || Table::isReadable(inputEphemTableName) 
      ){
    // add the eph id column if it doesn't exist alreay
    const String& ephemerisId = MSField::columnName(MSField::EPHEMERIS_ID);
    if(!this->actualTableDesc().isColumn(ephemerisId)){
      if(this->isWritable()){
	try{
	  this->addColumn(ScalarColumnDesc<Int>(ephemerisId, "Ephemeris id, pointer to EPHEMERIS table"), False);
	}
        catch(...){
	  return False;
	}
	// initialize to -1
	ScalarColumn<Int> fld(*this, ephemerisId);
	for(uInt i=0; i<this->nrow(); i++){
	  fld.put(i,-1);
	}
	rval = True;
      }
      else{
	return False;
      }
    }
    if(Table::isReadable(inputEphemTableName)){
      Directory inputDir(inputEphemTableName);
      stringstream ss;
      ss << "/EPHEM" << id << "_" << comment << ".tab";
      String destTableName = Path(this->tableName()).absoluteName() + String(ss.str());
      removeEphemeris(id); // remove preexisting ephemerides with the same id
      inputDir.copy(destTableName);
      rval = True;
    }
  }
  return rval;
}

Bool MSField::removeEphemeris(const uInt id){

  Bool rval=True;
  Directory fieldDir(Path(this->tableName()).absoluteName());
  stringstream ss;
  ss << "EPHEM" << id << "_*.tab";
  Regex ephemTableRegex (Regex::fromPattern(ss.str()));
  Vector<String> candidates = fieldDir.find(ephemTableRegex, True, False); // followSymLinks=True, recursive=False
  for(uInt i=0; i<candidates.size(); i++){
    Table tTab(fieldDir.path().absoluteName()+"/"+candidates(i));
    tTab.markForDelete();
  }
  for(uInt i=0; i<candidates.size(); i++){
    if(Table::isReadable(candidates(i))){
      rval = False;
    }
  }
  return rval;
}


} //# NAMESPACE CASACORE - END

