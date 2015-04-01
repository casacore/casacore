//# MeasurementSets.h:  Handle storage and access of the telescope data
//# Copyright (C) 1996,1997
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

#ifndef MS_MEASUREMENTSETS_H
#define MS_MEASUREMENTSETS_H

#include <casacore/casa/aips.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
//
// <summary>
// Handle storage and access of telescope data
// </summary>

// <prerequisite>
//   <li> <linkto module="Tables:description">Tables</linkto> module
//   <li> <a href="../notes/229.html">Note 229</a>
// </prerequisite>
//
//
// <reviewed reviewer="Bob Garwood" date="1997/02/01" demos="">
// </reviewed>

// <etymology>
// The MeasurementSet is where all data are ultimately to be found
// in Casacore.  Since, this is a collection of 
// measurements (either actual or simulated), the term MeasurementSet
// seems appropriate. Often we use the abbreviation (and typedef) MS for
// MeasurementSet.
// </etymology>
//
// <synopsis>
// The MeasurementSets module handles storage of telescope data and access
// to it. The MeasurementSet is the class that gives access to the data.
//
// A <linkto class=MeasurementSet>MeasurementSet</linkto> (MS) 
// is a Table with subtables stored as keywords. 
// For each of these tables there is a separate class: the main table is
// MeasurementSet, the subtables are MSAntenna, 
// MSArray, MSFeed, MSField, MSObsLog, MSObservation, MSSource, 
// MSSpectralWindow, MSSysCal, MSWeather. 
//
// <h4> Class hierarchy and Table layout </h4>
// Each table has a number
// of predefined columns and keywords, a subset of which is required to be
// present. The column and keyword layout of each table is described in
// <a href="../notes/229.html">Note 229</a>
// and in a separate class which contains two enum definitions.
// The enum classes, e.g., 
// <linkto class=MSMainEnums>MSMainEnums</linkto> and 
// <linkto class=MSAntennaEnums>MSAntennaEnums</linkto>, just contain a
// PredefinedColumn (PDC) enum and a PredefinedKeyword (PDK) enum. These enum 
// definitions are used as template arguments for the generic class
// <linkto class=MSTable><src>MSTable<PDC,PDK></src></linkto> from which MeasurementSet 
// and all the subtable classes are derived. 
// Thus, e.g., the class MSAntenna is derived from <src>
// MSTable<MSAntennaEnums::PredefinedColumns, MSAntennaEnums::PredefinedKeywords></src>. 
// 
// The MSTable class
// provides a large number of common column and keyword helper functions.
// They are useful when creating a Table following the MeasurementSet  
// conventions from scratch and assist in following the agreed upon 
// column and keyword naming conventions.
// 
// MSTable in turn is derived from Table, thus all the MS table classes are
// Tables. Many operations on a MeasurementSet are Table operations. See the 
// <linkto module="Tables:description">Tables</linkto> module for a list of 
// those operations.
//
// The reason for this class hierarchy is to provide each of the table classes
// with a separate namespace for its column and keyword enums, so that
// e.g., they can all have a column named TIME, while sharing as much code
// as possible. The MSTable class forwards any substantial work
// to the MSTableImpl class which does the actual work, it is a purely
// implementation class with static functions not of interest to the user.
// 
// <h4> Access to columns </h4>
// To simplify the access of columns, and catch type errors in
// the column declarations for column access objects (TableColumns) at
// compile time, there is a helper class for each (sub)table (e.g., 
// MSFieldColumns). The helper class for the MeasurementSet, 
// MSColumns gives access to the main table columns and the helper objects 
// for all subtables. A read-only version of these classes is also
// provided (e.g., ROMSFieldColumns).
//
// At present these classes are separate from the Table classes, mainly
// to ensure that the member functions are only called on valid, completely
// constructed MeasurementSet Tables. They also represent a large amount
// of 'state' in the form of TableColumn objects of which only a couple
// may actually be used.
// 
// <h4> Units and Measures </h4>
// Columns in the MeasurementSet and its subtables can have several 
// keywords attached to describe the contents of the column.
// The UNIT keyword contains an ASCII string describing the unit of
// the values in the column. The unit member function (in MSTable) will
// return this unit string, it can be used to construct a 
// <linkto class=Unit>Unit</linkto> object for a particular column.
//
// The MEASURE_TYPE keyword gives the Casacore Measure that applies to the
// column (if any). See the 
//  <linkto module="Measures:description">Measures</linkto> module for a 
// list of available Measures and their use.
//
// The MEASURE_REFERENCE keyword gives (part of) the reference frame needed
// to interpret the values in a column. An example is J2000 for the POSITION
// column. A number of static functions in MeasurementSet are available to
// create a 'template' Measure for a column, which has the MEASURE_TYPE filled
// in. Currently the following functions exist: directionMeasure, 
// positionMeasure, epochMeasure and frequencyMeasure. They return a 
// Measure which can then be filled with a value from a particular row from
// the column to obtain, e.g., a proper MDirection Measure for the phase
// center. 
//
// <h4> Reference Tables </h4>
// Each of the MS classes has a member function
// referenceCopy which takes the name of a new Table and a list (Block) of
// column names to create a Table which references all columns in the
// original table, except for those listed. The listed columns are 
// new columns which can be written to without affecting the original Table.
// The main use of this is for the synthesis package where corrected and
// model visibilities are stored as new DATA columns in an MS which 
// references the raw MS for the other columns. Except for these special
// cases, the use of this function will be rare.
//
// <h4> DATA and FLOAT_DATA columns </h4>
// To accommodate both synthesis and single dish data efficiently, it was 
// decided that a MeasurementSet can have a Complex DATA column,
// a float FLOAT_DATA column or both. If it has only a FLOAT_DATA column, the
// corresponding DATA column can be created with the makeComplexData()
// member function. In special cases, both columns could be present but
// filled for different rows, with an extra index defined indicating in
// which column to look (e.g., multi-feed single dish with cross correlations).
// The details of this last scheme are yet to be worked out. 
// The table consistency checks (isValid()) do not require the presence 
// of either column.
//
// <h4> Unset values in MeasurementSet Tables </h4>
// For ID columns, the rule is that a value of -1 indicates that there is
// no corresponding subtable in which to look up details. An example is
// the PULSAR_ID column, which should be set to -1 if the optional 
// PULSAR subtable does not exist.
//
// The rules for non integer unset values in MS tables have not 
// settled down yet.
// For Floating point and Complex values the recommended practice is
// to set the values to the FITS and IEEE value NaN, 
// with a bit pattern of all 1's.
// In much of the present filler code unused values are filled with 0 instead.
//
// <h4> Table destruction </h4>
// Upon destruction, the table and all subtables are checked to see that the
// MeasurementSet remains valid, i.e., all required columns are present.
// An exception is thrown if not all required columns are present
// Nevertheless, the table will be flushed to disk if it is writable -
// preserving its state.
//
//
// <h4> MS shorthand </h4>
// While the class name, MeasurementSet, is descriptive, it is often
// too long for many common uses.  The typedef MS is provided as
// a convenient shorthand for MeasurementSet.  The example below uses this
// typedef.
// 
//
// </synopsis> 
//
// <example>
// This example illustrates creation and filling of the MeasurementSet.
// <srcblock>
//      // create the table descriptor
//      TableDesc simpleDesc = MS::requiredTableDesc()
//      // set up a new table
//      SetupNewTable newTab("simpleTab", simpleDesc, Table::New);
//      // create the MeasurementSet
//      MeasurementSet simpleMS(newTab);
//      // now we need to define all required subtables
//      // the following call does this for us if we don't need to
//      // specify details of Storage Managers or non-standard columns.
//      simpleMS.createDummySubtables(Table::New);
//      // fill MeasurementSet via its Table interface
//      // For example, construct one of the column access objects.
//      TableColumn feed(simpleMS, MS::columnName(MS::FEED1));
//      uInt rownr = 0;
//      // add a row
//      simpleMS.addRow();
//      // set the values in that row, e.g. the feed column
//      feed.putScalar(rownr,1);
//      // Access the position column in the ANTENNA subtable
//      ArrayColumn<Double> antpos(simpleMS.antenna(),
//                                 MSAntenna::columnName(MSAntenna::POSITION));
//      // Add a row to it and fill in the position
//      simpleMS.antenna().addRow();
//      Array<Double> position(3); 
//      position(0)=1.; position(1)=2.; position(2)=3.;
//      antpos.put(0,position);
//      // .
//      // For standard columns the above can be done more easily using 
//      // the MSColumns object.
//      // Create the MSColumns
//      MSColumns msc(simpleMS);
//      // and fill in the position
//      msc.antenna().position().put(0,position);
// </srcblock>
//
// </example>

// <example>
// This example illustrates read only access to an existing MeasurementSet
// and creation of an MDirection Measure for the phase center.
// <srcblock>
//      // Create the MeasurementSet from an existing Table on disk
//      MeasurementSet ms("myMS"); 
//      // Create the RO column access objects for main table and subtables
//      ROMSColumns msc(ms);
//      // show data from row 5
//      cout << msc.data()(5) << endl;
//      // show phase center for row 3 in field table
//      Vector<double> phaseCtr=msc.field().phaseCenter()(3);
//      cout << phaseCtr<<endl;
//      // now create a Measure for the phaseCenter
//      MDirection phaseCenterMeasure =
//         MS::directionMeasure(msc.field().phaseCenter());
//      // put the value from row 3 in the Measure and print it
//      phaseCenterMeasure.set(MVPosition(phaseCtr));
//      cout <<"phase center:"<< phaseCenterMeasure<<endl;
//
// </srcblock>
//
// </example>
//
// <motivation>
// The attempt is to define a single, extensible, Table format that will
// be able to cope with all, or at least most, radio telescope data.
// Having a single MeasurementSet should make it easier to combine data
// from different instruments. The format of the MeasurementSet,
// table with subtables, was chosen to be able to cope with items
// varying at different rates more efficiently than a 'flat' Table layout
// would allow.
// </motivation>

// <todo asof="1997/02/01">
//   <li> Incorporate the MSColumn classes in the MeasurementSet classes?
//   <li> Variable (row to row) ReferenceFrame (e.g., J2000 mixed with
//	galactic, different Frequency reference frames mixed in the
//	same MS, etc.). This could be done with a column named
//     "column_name"_MEASURE_REFERENCE for each column with varying
//     Measure reference frames.
// </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif
