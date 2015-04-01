//# DerivedMSCal.cc: Virtual column engine to return MS values
//# Copyright (C) 2010
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

#include <casacore/derivedmscal/DerivedMC/DerivedMSCal.h>
#include <casacore/derivedmscal/DerivedMC/DerivedColumn.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/Measures/MCBaseline.h>
#include <casacore/measures/Measures/Muvw.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore {

DerivedMSCal::DerivedMSCal()
{}

DerivedMSCal::DerivedMSCal (const Record&)
{}

DerivedMSCal::DerivedMSCal (const DerivedMSCal&)
  : VirtualColumnEngine(),
    itsEngine()
{}

DerivedMSCal::~DerivedMSCal()
{
  for (uInt i=0; i<ncolumn(); i++) {
    delete itsColumns[i];
  }
}

DataManager* DerivedMSCal::clone() const
{
  return new DerivedMSCal (*this);
}

String DerivedMSCal::dataManagerType() const
{
  return "DerivedMSCal";
}

Record DerivedMSCal::dataManagerSpec() const
{
  return Record();
}


DataManagerColumn* DerivedMSCal::makeScalarColumn (const String& name,
                                                   int,
                                                   const String&)
{
  DataManagerColumn* col;
  if (name == "HA") {
    col = new HourangleColumn(&itsEngine, -1);   // array center position
  } else if (name == "HA1") {
    col = new HourangleColumn(&itsEngine, 0);    // antenna1 position
  } else if (name == "HA2") {
    col = new HourangleColumn(&itsEngine, 1);    // antenna2 position
  } else if (name == "LAST") {
    col = new LASTColumn(&itsEngine, -1);
  } else if (name == "LAST1") {
    col = new LASTColumn(&itsEngine, 0);
  } else if (name == "LAST2") {
    col = new LASTColumn(&itsEngine, 1);
  } else if (name == "PA1") {
    col = new ParAngleColumn(&itsEngine, 0);
  } else if (name == "PA2") {
    col = new ParAngleColumn(&itsEngine, 1);
  } else {
    throw DataManError (name +
                        " is an unknown scalar column for DerivedMSCal");
  }
  itsColumns.push_back (col);
  return col;
}

DataManagerColumn* DerivedMSCal::makeIndArrColumn (const String& name,
                                                   int,
                                                   const String&)
{
  DataManagerColumn* col;
  if (name == "HADEC") {
    col = new HaDecColumn(&itsEngine, -1);
  } else if (name == "HADEC1") {
    col = new HaDecColumn(&itsEngine, 0);
  } else if (name == "HADEC2") {
    col = new HaDecColumn(&itsEngine, 1);
  } else if (name == "AZEL1") {
    col = new AzElColumn(&itsEngine, 0);
  } else if (name == "AZEL2") {
    col = new AzElColumn(&itsEngine, 1);
  } else if (name == "UVW_J2000") {
    col = new UVWJ2000Column(&itsEngine);
  } else {
    throw DataManError (name +
                        " is an unknown array column for DerivedMSCal");
  }
  itsColumns.push_back (col);
  return col;
}

void DerivedMSCal::prepare()
{
  itsEngine.setTable (table());
}

DataManager* DerivedMSCal::makeObject (const String&,
                                       const Record& spec)
{
  // This function is called when reading a table back.
  return new DerivedMSCal (spec);
}

void DerivedMSCal::registerClass()
{
  DataManager::registerCtor ("DerivedMSCal", makeObject);
}

Bool DerivedMSCal::canAddColumn() const
{
  return True;
}
Bool DerivedMSCal::canRemoveColumn() const
{
  return True;
}

void DerivedMSCal::addColumn (DataManagerColumn*)
{}
void DerivedMSCal::removeColumn (DataManagerColumn*)
{}

} //# end namespace
