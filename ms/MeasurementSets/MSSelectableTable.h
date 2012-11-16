// -*- C++ -*-
//# MSSelectableTable.h: The generic interface for tables that can be used with MSSelection
//# Copyright (C) 1996,1997,1998,1999,2001
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
//#
//# $Id$

#ifndef MS_MSSELECTABLETABLE_H
#define MS_MSSELECTABLETABLE_H

#include <casa/aips.h>
#include <casa/BasicSL/String.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/Cube.h>
#include <casa/Containers/OrderedMap.h>
#include <casa/Containers/MapIO.h>
#include <tables/Tables/ExprNode.h>
#include <ms/MeasurementSets/MeasurementSet.h>
#include <ms/MeasurementSets/MSMainEnums.h>
#include <ms/MeasurementSets/MSSelectionError.h>
namespace casa { //# NAMESPACE CASA - BEGIN

  class MSSelectableTable
  {
  public:
    MSSelectableTable()                       {}
    MSSelectableTable(const Table& table)     {table_p = &table;}
    virtual ~MSSelectableTable()              {}

    virtual void setTable(const Table& table) {table_p = &table;}
    const Table* table()                      {return table_p;}
    virtual Bool isMS()                       = 0;
    TableExprNode col(const String& colName)  {return table()->col(colName);}

    virtual const MSAntenna& antenna()        = 0;
    virtual const MSField& field()            = 0;
    virtual const MSSpectralWindow& spectralWindow() = 0;
    virtual const MSDataDescription& dataDescription() = 0;
    virtual const MSObservation& observation() = 0;

    virtual String columnName(MSMainEnums::PredefinedColumns nameEnum) = 0;
    virtual const MeasurementSet* asMS() = 0;
  protected:
    const Table *table_p;
  };

  class MSInterface: public MSSelectableTable
  {
  public:
    MSInterface()                                      {}
    MSInterface(const Table& table);
    virtual ~MSInterface()                             {}
    virtual const MSAntenna& antenna()                 {return asMS()->antenna();}
    virtual const MSField& field()                     {return asMS()->field();}
    virtual const MSSpectralWindow& spectralWindow()   {return asMS()->spectralWindow();}
    virtual const MSDataDescription& dataDescription() {return asMS()->dataDescription();}
    virtual const MSObservation& observation()         {return asMS()->observation();}
    virtual String columnName(MSMainEnums::PredefinedColumns nameEnum) {return MS::columnName(nameEnum);}
    virtual Bool isMS()                                {return True;}

    virtual const MeasurementSet *asMS(){return static_cast<const MeasurementSet *>(table());}
  };
} //# NAMESPACE CASA - END

#endif
