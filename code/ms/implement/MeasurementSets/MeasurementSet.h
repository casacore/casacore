//# MeasurementSet.h: A Table to hold astronomical data (a set of Measurements)
//# Copyright (C) 1996,1997,1999,2000
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

#if !defined(AIPS_NEWMEASUREMENTSET_H)
#define AIPS_NEWMEASUREMENTSET_H

#include <aips/aips.h>
#include <aips/MeasurementSets/MSTable.h>
#include <aips/MeasurementSets/MSMainEnums.h>
#include <aips/MeasurementSets/MSAntenna.h>
#include <aips/MeasurementSets/MSDataDescription.h>
#include <aips/MeasurementSets/MSDoppler.h>
#include <aips/MeasurementSets/MSFeed.h>
#include <aips/MeasurementSets/MSField.h>
#include <aips/MeasurementSets/MSFlagCmd.h>
#include <aips/MeasurementSets/MSFreqOffset.h>
#include <aips/MeasurementSets/MSHistory.h>
#include <aips/MeasurementSets/MSObservation.h>
#include <aips/MeasurementSets/MSPointing.h>
#include <aips/MeasurementSets/MSPolarization.h>
#include <aips/MeasurementSets/MSProcessor.h>
#include <aips/MeasurementSets/MSSource.h>
#include <aips/MeasurementSets/MSSpectralWindow.h>
#include <aips/MeasurementSets/MSState.h>
#include <aips/MeasurementSets/MSSysCal.h>
#include <aips/MeasurementSets/MSWeather.h>
 
//# Forward Declarations, more could be if they weren't part of the
//# static classes 
class SetupNewTable;
template <class T> class Block;
class MDirection;
class MEpoch;
class MFrequency;
class MPosition;


//# forward declared so that the following typedef is up-front
class MeasurementSet;

// MeasurementSet is too cumbersome for a number of common uses,
// so we give a typedef here.
typedef MeasurementSet NewMS;

// <summary> 
// A Table intended to hold astronomical data (a set of Measurements).
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="tMeasurementSet.cc" demos="">

// <prerequisite>
// <ul>
//   <li> <linkto module="Tables:description">Tables</linkto> module
//   <li> <linkto class="MSTable">MSTable</linkto> 
// </ul>
// </prerequisite>
//
// <etymology>
// The MeasurementSet is where all data are ultimately to be found
// in AIPS++.  Since, this is a collection of 
// measurements (either actual or simulated), the term MeasurementSet
// seems appropriate.
// </etymology>
//
// <synopsis> 
// A MeasurementSet is a Table.  Most operations on a MeasurementSet are
// Table operations. See the <linkto module="Tables:description">Tables</linkto> 
// module for a list of those operations.  The member functions provided by this
// class are primarily convenience functions to help users follow the 
// agreed upon column and keyword naming conventions.  They are useful when
// creating a Table following the MeasurementSet conventions from
// scratch as well as when creating the column objects to access those
// columns.
//
// The standard way of accessing
// table columns is through Strings.  Mistakes in typing the column
// name will not be caught at compile time (and may not be caught at
// run time).  We have therefore decided to use an enumeration
// to specify columns so that many mistakes will be caught at compile
// time.  This requires functions to map to and from this enumeration
// to the strings that are ultimately used. 
//
// Upon destruction, the table is checked to see that the
// MeasurementSet remains valid, i.e., all required columns are present
// An exception is thrown if not all required columns are present
// Nevertheless, the table will be flushed to disk if it is writable -
// preserving its state.
//
// A MeasurementSet has a number of required subtables. These are stored
// as keywords in the Table. Access to these subtables is provided via
// member functions (e.g. antenna() for the ANTENNA table). All subtables
// have associated MeasurementSet-like classes defined for them (MSAntenna
// for the ANTENNA table) which provide analogous column and keyword mapping
// as provided here.
//
// While the class name, MeasurementSet, is descriptive, it is often
// too long for many common uses.  The typedef NewMS is provided as
// a convenient shorthand for MeasurementSet.  The example below uses this
// typedef.
// 
// Due to the inheritance scheme, it was necessary to separate the enumerations
// used by MeasurementSet into a separate class, 
// <linkto class=MSMainEnums>MSMainEnums</linkto>.
//
// </synopsis> 
//
// <example>
// This example illustrates a simple use of the MeasurementSet class.
// <srcblock>
//      // create the table descriptor
//      TableDesc simpleDesc = NewMS::requiredTableDesc()
//      // set up a new table
//      SetupNewTable newTab("simpleTab", simpleDesc, Table::New);
//      // create the MeasurementSet
//      MeasurementSet simpleNewMS(newTab);
//      // now we need to define all required subtables
//      // the following call does this for us if we don't need to
//      // specify details of Storage Managers for columns.
//      simpleNewMS.createDefaultSubtables(Table::New);
//      // fill MeasurementSet via its Table interface
//      // For example, construct one of the columns
//      TableColumn feed(simpleNewMS, NewMS::columnName(NewMS::FEED1));
//      uInt rownr = 0;
//      // add a row
//      simpleNewMS.addRow();
//      // set the values in that row, e.g. the feed column
//      feed.putScalar(rownr,1);
//      // Access a subtable
//      ArrayColumn<Double> antpos(simpleNewMS.antenna(),
//                                 MSAntenna::columnName(MSAntenna::POSITION));
//      simpleNewMS.antenna().addRow();
//      Array<Double> position(3); 
//      position(0)=1.; position(1)=2.; position(2)=3.;
//      antpos.put(0,position);
//      // etc.
// </srcblock>
//
// </example>
//
// <motivation>
// The Table module is more than adequate as a container of data.  
// However, in order for applications to be useful with data from 
// different sources, some conventions need to be adopted in the use 
// of Tables to store data.  The MeasurementSet is
// where those conventions are defined and, to some extent, enforced.
//
// There are a number of reasons why MeasurementSet is more
// than just a Table.
// <ul>
// <li> To provide one location where the column and keyword names, data 
//      types, and table comment strings are found.
// <li> To provide one location where the required table descriptor for
//      the MeasurementSet is found. 
// <li> To provide a means of verifying the validity of a MeasurementSet
//      at construction and destruction.
// <li> To allow application programmers to catch name or data type
//      mistakes at compile time rather than at run time.
// </ul>
// 
// </motivation>
//
// <todo asof="1996/2/22">
// <li> referenceCopy() should be more flexible with the storage managers used 
//      for the columns which are not merely references.
// <li> When ForwardColumnEngine is fixed so that it can deal with
//      tables already in the cache, modify the test program.  It may also
//      be necessary to modify referenceCopy().
// </todo>

class MeasurementSet : public MSTable<MSMainEnums::PredefinedColumns,
                                      MSMainEnums::PredefinedKeywords>,
		       public MSMainEnums
{
public:
  // This constructs an empty MeasurementSet, only useful to assign to
  // (it is not a valid NewMS yet).
  MeasurementSet ();

  // These constructors mirror the Table ones with additional checking
  // on validity (verifying that the NewMS will have the required columns
  // and keywords)
  // An exception is thrown if the constructed Table is not a valid NewMS
  // <thrown>
  //   <li> AipsError
  // </thrown>
  // <group name=tableLikeConstructors>

  MeasurementSet (const String &tableName, TableOption = Table::Old);
  MeasurementSet (const String &tableName, const TableLock& lockOptions,
		  TableOption = Table::Old);
  MeasurementSet (const String &tableName, const String &tableDescName,
		  TableOption = Table::Old);
  MeasurementSet (const String &tableName, const String &tableDescName,
		  const TableLock& lockOptions, TableOption = Table::Old);
  MeasurementSet (SetupNewTable &newTab, uInt nrrow = 0,
		  Bool initialize = False);
  MeasurementSet (SetupNewTable &newTab, const TableLock& lockOptions,
		  uInt nrrow = 0, Bool initialize = False);
  MeasurementSet (const Table &table);
  MeasurementSet (const MeasurementSet &other);
  // </group>

  // As with tables, the destructor writes the table if necessary.
  // Additional checking is done here to verify that all required
  // columns are still present.
  // If it is NOT valid, it will write the table and then throw an exception.
  // <thrown>
  //   <li> AipsError
  // </thrown>
  ~MeasurementSet();

  //  Assignment operator, reference semantics
  MeasurementSet& operator=(const MeasurementSet&);

  // Make a special copy of this NewMS which references all columns from
  // this NewMS except those mentioned; those are empty and writable.
  // Each forwarded column has the same writable status as the underlying
  // column. The mentioned columns all use the AipsIO storage manager.
  // The main use of this is for the synthesis package where corrected and
  // model visibilities are stored as new DATA columns in an NewMS which 
  // references the raw NewMS for the other columns. Except for these special
  // cases, the use of this function will be rare.
  MeasurementSet referenceCopy(const String& newTableName,
			       const Block<String>& writableColumns) const;

  // Return the name of each of the subtables. This should be used by the
  // filler to create the subtables in the correct location.
  // <group>
  String antennaTableName() const {return tableName()+"/ANTENNA";}
  String dataDescriptionTableName() const {
    return tableName()+"/DATA_DESCRIPTION";}
  String dopplerTableName() const { return tableName()+"/DOPPLER";}
  String feedTableName() const {return tableName()+"/FEED";}
  String fieldTableName() const {return tableName()+"/FIELD";}
  String flagCmdTableName() const {return tableName()+"/FLAG_CMD";}
  String freqOffsetTableName() const {return tableName()+"/FREQ_OFFSET";}
  String historyTableName() const {return tableName()+"/HISTORY";}
  String observationTableName() const {return tableName()+"/OBSERVATION";}
  String pointingTableName() const {return tableName()+"/POINTING";}
  String polarizationTableName() const {return tableName()+"/POLARIZATION";}
  String processorTableName() const {return tableName()+"/PROCESSOR";}
  String sourceTableName() const {return tableName()+"/SOURCE";}
  String spectralWindowTableName() const {
    return tableName()+"/SPECTRAL_WINDOW";}
  String stateTableName() const {return tableName()+"/STATE";}
  String sysCalTableName() const {return tableName()+"/SYSCAL";}
  String weatherTableName() const {return tableName()+"/WEATHER";}
  // </group>
    
  // Access functions for the subtables, using the NewMS-like interface for each
  // <group>
  MSAntenna& antenna() {return antenna_p;}
  MSDataDescription& dataDescription() {return dataDesc_p;}
  MSDoppler& doppler() {return doppler_p;}
  MSFeed& feed() {return feed_p;}
  MSField& field() {return field_p;}
  MSFlagCmd& flagCmd() {return flagCmd_p;}
  MSFreqOffset& freqOffset() {return freqOffset_p;}
  MSHistory& history() {return history_p;}
  MSObservation& observation() {return observation_p;}
  MSPointing& pointing() {return pointing_p;}
  MSPolarization& polarization() {return polarization_p;}
  MSProcessor& processor() {return processor_p;}
  MSSource& source() {return source_p;}
  MSSpectralWindow& spectralWindow() {return spectralWindow_p;}
  MSState& state() {return state_p;}
  MSSysCal& sysCal() {return sysCal_p;}
  MSWeather& weather() {return weather_p;}
  const MSAntenna& antenna() const {return antenna_p;}
  const MSDataDescription& dataDescription() const {return dataDesc_p;}
  const MSDoppler& doppler() const {return doppler_p;}
  const MSFeed& feed() const {return feed_p;}
  const MSField& field() const {return field_p;}
  const MSFlagCmd& flagCmd() const {return flagCmd_p;}
  const MSFreqOffset& freqOffset() const {return freqOffset_p;}
  const MSHistory& history() const {return history_p;}
  const MSObservation& observation() const {return observation_p;}
  const MSPointing& pointing() const {return pointing_p;}
  const MSPolarization& polarization() const {return polarization_p;}
  const MSProcessor& processor() const {return processor_p;}
  const MSSource& source() const {return source_p;}
  const MSSpectralWindow& spectralWindow() const {return spectralWindow_p;}
  const MSState& state() const {return state_p;}
  const MSSysCal& sysCal() const {return sysCal_p;}
  const MSWeather& weather() const {return weather_p;}
  // </group>

  // Initialize the references to the subtables. You need to call
  // this only if you assign new subtables to the table keywords.
  // This also checks for validity of the table and its subtables.
  void initRefs();

  // Create default subtables: fills the required subtable keywords with
  // tables of the correct type, mainly for testing and as an example of
  // how to do this for specific fillers. In practice these tables will
  // often have more things specified, like dimensions of arrays and
  // storage managers for the various columns.
  void createDefaultSubtables(Table::TableOption option=Table::Scratch);

  // Initialize the statics appropriately. This does not need to be
  // called by users, it is called by the implementation class
  // MSTableImpl.
  static void init();

  // Create DATA column from existing FLOAT_DATA column. Noop if DATA already
  // exists or neither exists (returns False in that case).
  Bool makeComplexData();

  // Validate Measure references - check that all Measure columns have their
  // reference value set, report the ones that don't.
  Bool validateMeasureRefs();


private:

  // temporary function to add the CATEGORY keyword to the FLAG_CATEGORY
  // column if it isn't there yet. 2000/08/22
  //  remove this and the calls next MS update
  void addCat();

  // keep references to the subtables
  MSAntenna antenna_p;
  MSDataDescription dataDesc_p;
  MSDoppler doppler_p; //optional
  MSFeed feed_p;
  MSField field_p;
  MSFlagCmd flagCmd_p;
  MSFreqOffset freqOffset_p; //optional
  MSHistory history_p;
  MSObservation observation_p;
  MSPointing pointing_p;
  MSPolarization polarization_p;
  MSProcessor processor_p;
  MSSource source_p;
  MSSpectralWindow spectralWindow_p;
  MSState state_p;
  MSSysCal sysCal_p; //optional
  MSWeather weather_p; //optional

  // required by the need to throw an exception in the destructor
  Bool hasBeenDestroyed_p;

};

#endif
