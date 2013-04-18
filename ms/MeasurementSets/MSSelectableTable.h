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
#include <ms/MeasurementSets/MSSelectableMainColumn.h>
namespace casa { //# NAMESPACE CASA - BEGIN

// <summary> 
//
// MSSelectableTable: An interface class used by MSSelection module to
// access the sub-tables and main-table columns of MS-like tables.
//
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
// </prerequisite>
//
// <etymology>
// From "msselection" and "table".
// </etymology>
//
//<synopsis> 
// This is a pure virtual base-class to provide a table-type agnostic
// interface to the MSSelection class to access sub-tables and
// main-table columns of MS-like tables.
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// To allow use of the <linkto class=MSSelection>MSSelection<linkto> class
// for selection on any table that follows the general structure of the
// MS dataset.  Via this class, minor differences in the data
// layout can be hidden from the MSSelection module.  This also keeps
// MeasurementSet module from depending on other MS-like database
// implemention which may use the MSSelection module.  Such usage will
// need to implement a specialization of
// <linkto class=MSSelectableTable>MSSelectableTable</linkto> and
// use it to instantiate the
// <linkto class=MSSelection>MSSelection</linkto> object.
// </motivation>
//
// <todo asof="19/03/13">
// </todo>

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
    
    virtual MSSelectableMainColumn* mainColumns() = 0;

  protected:
    const Table *table_p;
  };

// <summary> 
//
// MSInterface: A specialization of MSSelectableTable for accessing
// MS.
//
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
// </prerequisite>
//
// <etymology>
//
// From "ms" and "interface".
//
// </etymology>
//
//<synopsis> 
//
// A class that can be passed around as MSSelectableTable, with most of
// the methods overloaded to work with the underlaying MS.
//
//</synopsis>
//
// <example>
// <srcblock>
//
// //
// // Fill in the expression in the various strings that are passed for
// // parsing to the MSSelection object later.
// //
// String fieldStr,timeStr,spwStr,baselineStr,
//   uvdistStr,taqlStr,scanStr,arrayStr, polnStr,stateObsModeStr,
//   observationStr;
// baselineStr="1&2";
// timeStr="*+0:10:0";
// fieldStr="CygA*";
// //
// // Instantiate the MS and the MSInterface objects.
// //
// MS ms(MSName),selectedMS(ms);
// MSInterface msInterface(ms);
// //
// // Setup the MSSelection thingi
// //
// MSSelection msSelection;
//
// msSelection.reset(msInterface,MSSelection::PARSE_NOW,
// 		    timeStr,baselineStr,fieldStr,spwStr,
// 		    uvdistStr,taqlStr,polnStr,scanStr,arrayStr,
// 		    stateObsModeStr,observationStr);
// if (msSelection.getSelectedMS(selectedMS))
//   cerr << "Got the selected MS!" << endl;
// else
//   cerr << "The set of expressions resulted into null-selection";
// </srcblock>
// </example>
//
// <motivation>
//
// To generalize the implementation of the MSSelection parsers.
//
// </motivation>
//
// <todo asof="19/03/13">
// </todo>

  class MSInterface: public MSSelectableTable
  {
  public:
    MSInterface():msMainCols_p(0)                   {}
    MSInterface(const Table& table);
    virtual ~MSInterface()                             {if (msMainCols_p) delete msMainCols_p;}
    virtual const MSAntenna& antenna()                 {return asMS()->antenna();}
    virtual const MSField& field()                     {return asMS()->field();}
    virtual const MSSpectralWindow& spectralWindow()   {return asMS()->spectralWindow();}
    virtual const MSDataDescription& dataDescription() {return asMS()->dataDescription();}
    virtual const MSObservation& observation()         {return asMS()->observation();}
    virtual String columnName(MSMainEnums::PredefinedColumns nameEnum) {return MS::columnName(nameEnum);}
    virtual Bool isMS()                                {return True;}

    virtual const MeasurementSet *asMS(){return static_cast<const MeasurementSet *>(table());}
    virtual MSSelectableMainColumn* mainColumns()
    {msMainCols_p = new MSMainColInterface(*table_p); return msMainCols_p;}
  private:
    MSMainColInterface *msMainCols_p;
  };
} //# NAMESPACE CASA - END

#endif
