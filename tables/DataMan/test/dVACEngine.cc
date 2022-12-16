//# dVACEngine.cc: Example virtual column engine to handle data type A
//# Copyright (C) 1994,1995,1996,1997,2001
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

//# Includes
#include "dVACEngine.h"
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/namespace.h>

VACExampleVACEngine::VACExampleVACEngine()
{}

VACExampleVACEngine::VACExampleVACEngine (const String& sourceColumnName,
					  const String& xTargetColumnName,
					  const String& yTargetColumnName,
					  const String& zTargetColumnName)
  : VACEngine<VACExample> (sourceColumnName),
    xTargetName_p (xTargetColumnName),
    yTargetName_p (yTargetColumnName),
    zTargetName_p (zTargetColumnName)
{}

VACExampleVACEngine::VACExampleVACEngine (const VACExampleVACEngine& that)
  : VACEngine<VACExample> (that),
    xTargetName_p (that.xTargetName_p),
    yTargetName_p (that.yTargetName_p),
    zTargetName_p (that.zTargetName_p)
{}

VACExampleVACEngine::~VACExampleVACEngine()
{}

DataManager* VACExampleVACEngine::clone() const
{
  DataManager* dmPtr = new VACExampleVACEngine (sourceColumnName(),
                                                xTargetName_p, yTargetName_p,
                                                zTargetName_p);
  if (dmPtr == 0) {
    throw (AllocError ("VACExampleVACEngine::clone()", 1));
  }
  return dmPtr;
}


// Store the target column names in the keywords of this column.
void VACExampleVACEngine::create64 (rownr_t)
{
  TableColumn src (table(), sourceColumnName());
  src.rwKeywordSet().define ("_xTargetName", xTargetName_p);
  src.rwKeywordSet().define ("_yTargetName", yTargetName_p);
  src.rwKeywordSet().define ("_zTargetName", zTargetName_p);
}

// Prepare the engine by allocating column objects
// for the used columns.
// Get their names from the keywords of this column.
void VACExampleVACEngine::prepare()
{
  TableColumn src (table(), sourceColumnName());
  xTargetName_p = src.keywordSet().asString ("_xTargetName");
  yTargetName_p = src.keywordSet().asString ("_yTargetName");
  zTargetName_p = src.keywordSet().asString ("_zTargetName");
  colx.attach (table(), xTargetName_p);
  coly.attach (table(), yTargetName_p);
  colz.attach (table(), zTargetName_p);
}

void VACExampleVACEngine::setShape (rownr_t rownr, const IPosition& shape)
{
  colx.setShape (rownr, shape);
  coly.setShape (rownr, shape);
  colz.setShape (rownr, shape);
}
Bool VACExampleVACEngine::isShapeDefined (rownr_t rownr)
{
  return colx.isDefined (rownr);
}
IPosition VACExampleVACEngine::shape (rownr_t rownr)
{
  return colx.shape (rownr);
}


void VACExampleVACEngine::getArray (rownr_t rownr, Array<VACExample>& value)
{
  Array<Int>    x;
  Array<float>  y;
  Array<String> z;
  colx.get (rownr, x);
  coly.get (rownr, y);
  colz.get (rownr, z);
  Array<VACExample>::iterator iter = value.begin();
  for (size_t i=0; i<x.size(); ++i) {
    iter->x() = x.data()[i];
    iter->y() = y.data()[i];
    iter->z() = z.data()[i];
    ++iter;
  }
}
void VACExampleVACEngine::putArray (rownr_t rownr, const Array<VACExample>& value)
{
  Array<Int>    x(value.shape());
  Array<float>  y(value.shape());
  Array<String> z(value.shape());
  Array<VACExample>::const_iterator iter = value.begin();
  for (size_t i=0; i<x.size(); ++i) {
    x.data()[i] = iter->x();
    y.data()[i] = iter->y();
    z.data()[i] = iter->z();
    ++iter;
  }
  colx.put (rownr, x);
  coly.put (rownr, y);
  colz.put (rownr, z);
}


DataManager* VACExampleVACEngine::makeObject (const String&, const Record&)
{
  DataManager* dmPtr = new VACExampleVACEngine();
  if (dmPtr == 0) {
    throw (AllocError ("VACExampleVACEngine::makeObject()", 1));
  }
  return dmPtr;
}
void VACExampleVACEngine::registerClass()
{
  DataManager::registerCtor ("VACExampleVACEngine", makeObject);
}
