// -*- C++ -*-
//# MSSelectableMainColumn.h: The generic interface for tables that can be used with MSSelection
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

#ifndef MS_MSSELECTABLEMAINCOLUMN_H
#define MS_MSSELECTABLEMAINCOLUMN_H

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
#include <ms/MeasurementSets/MSSelectableTable.h>
#include <ms/MeasurementSets/MSMainColumns.h>
namespace casa { //# NAMESPACE CASA - BEGIN

  class MSSelectableMainColumn
  {
  public:
    MSSelectableMainColumn(const Table& msLikeTable)  {init(msLikeTable);}
    MSSelectableMainColumn() {table_p=NULL;}
    virtual ~MSSelectableMainColumn() {}

    virtual void init(const Table& msLikeTable) {table_p=&msLikeTable;}
    const Table* table()                      {return table_p;}
    virtual const ROArrayColumn<Bool>& flag() = 0;
    virtual Bool flagRow(const Int& i) = 0;
    virtual const ROScalarQuantColumn<Double>& exposureQuant() = 0;
    virtual const ROScalarQuantColumn<Double>& timeQuant() = 0;
    virtual const MeasurementSet *asMS() = 0;

  protected:
    const Table *table_p;
  };

  class MSMainColInterface: public MSSelectableMainColumn
  {
  public: 
    MSMainColInterface():MSSelectableMainColumn() {}
    MSMainColInterface(const Table& msAsTable): MSSelectableMainColumn(msAsTable)
    {init(msAsTable);}

    virtual ~MSMainColInterface() {delete msCols_p;}

    virtual void init(const Table& msAsTable)
    {MSSelectableMainColumn::init(msAsTable);ms_p = MeasurementSet(msAsTable); msCols_p=new ROMSMainColumns(ms_p);}
    virtual const ROArrayColumn<Bool>& flag() {return msCols_p->flag();}

    //    virtual const Bool flagRow(const Int& i) {return allTrue(msCols_p->flag()(i));}
    virtual Bool flagRow(const Int& i) {return msCols_p->flagRow()(i);}
    virtual const ROScalarQuantColumn<Double>& exposureQuant() {return msCols_p->exposureQuant();}
    virtual const ROScalarQuantColumn<Double>& timeQuant()     {return msCols_p->timeQuant();}

    virtual const MeasurementSet *asMS(){return static_cast<const MeasurementSet *>(table());}
  private:
    MeasurementSet ms_p;
    ROMSMainColumns *msCols_p;

  };

} //# NAMESPACE CASA - END

#endif
