//# SubMS.h: this defines SubMS which creates a subset of an MS with some
//# transformation
//# Copyright (C) 1997,1998,1999,2000,2001,2003
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
#include <ms/MeasurementSets/MeasurementSet.h>
#include <casa/aips.h>
#include <casa/Arrays/Vector.h>


#ifndef MSVIS_SUBMS_H
namespace casa { //# NAMESPACE CASA - BEGIN

#define MSVIS_SUBMS_H

// <summary>
// SubMS provides functionalities to make a subset of an existing MS
// </summary>

// <visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
// </prerequisite>
//
// <etymology>
// SubMS ...from the SUBset of an MS
// </etymology>
//
// <synopsis>
//
//
//
// </synopsis>

class MSColumns;
class ROMSColumns;

class SubMS
{

 public:

  SubMS(String& theMS);
  
  // construct from an MS
  SubMS(MeasurementSet& ms);

  

  ~SubMS();
  
  // Change or Set the MS this MSSelector refers to.
  void setMS(MeasurementSet& ms);


  // select spw and channels for each spw...
  void selectSpw(Vector<Int> spw, Vector<Int> nchan, Vector<Int> start, 
		 Vector<Int> step, Bool averchan=True);


  //select Time and time averaging or regridding
  //void selectTime();

  // Select source or field
  void selectSource(Vector<Int> fieldid);
  

  // Select Antennas to split out  
  void selectAntenna(Vector<Int>& antennaids, Vector<String>& antennaSel);


  //select time parameters
  void selectTime(Double timeBin=-1.0, String timerng="");

  //void selectSource(Vector<String> sourceid);

  //Method to set if a phase Center rotation is needed
  //void setPhaseCenter(Int fieldid, MDirection& newPhaseCenter);



  //Method to make the subMS

  Bool makeSubMS(String& submsname, String& whichDataCol);

  //Method to make a scratch  subMS and even in memory if posssible
  //Useful if temporary subselection/averaging is necessary
  // It'll be in memory if the basic output ms is less than half of 
  // memory reported by HostInfo unless forced to by user...
  MeasurementSet* makeScratchSubMS(String& whichDataCol, 
				   Bool forceInMemory=False);

  // This setup a default new ms
  // Can be called directly as its not dependent on any private variable
  static MeasurementSet* setupMS(String msname, Int nchan, Int npol, String telescop, Int obstype=0);
  
  
  

 private:
  //method that returns the selected ms
  Bool makeSelection();
  Bool fillAllTables(const String& colname);
  Bool fillDDTables();
  Bool fillFieldTable();
  Bool fillMainTable(const String& which);
  Bool fillAverMainTable(const String& which);
  Bool copyAntenna();
  Bool copyFeed();
  Bool copySource();
  Bool copyObservation();
  Bool copyPointing();
  Bool writeDiffSpwShape(String& columnName);
  Bool writeSimilarSpwShape(String& columnName);
  // return the number of unique antennas selected
  Int numOfBaselines(Vector<Int>& ant1, Vector<Int>& ant2, 
		    Bool includeAutoCorr=False);
  // Number of time bins to average into from selected data
  Int numOfTimeBins(const Double& timeBin);
  Bool fillAverAntTime(Vector<Int>& ant1, Vector<Int>& ant2, 
		       const Double& timeBin, 
		       const Int& numOfTimeBins);
  Bool fillTimeAverData(Vector<Int>& ant1, Vector<Int>& ant2, 
			const Double& timeBin, const Int& numbas, 
			const String& ColumnName);
  void checkSpwShape();

  MSColumns * msc_p;
  MSColumns * mscIn_p;

  MeasurementSet ms_p, mssel_p, msOut_p;
  Vector<Int> spw_p, nchan_p, chanStart_p, chanStep_p, npol_p, inNumChan_p;
  Vector<Int> fieldid_p;
  Bool averageChannel_p;
  Vector<Int> spwRelabel_p, fieldRelabel_p;
  Vector<Int> oldDDSpwMatch_p;
  Bool doChanAver_p;
  Bool antennaSel_p;
  Vector<String> antennaSelStr_p;
  Vector<Int> antennaId_p;
  Double timeBin_p;
  Bool sameShape_p;
  String timeRange_p;
  Vector<Double> newTimeVal_p;
  Vector<Int> timeBinIndex_p;
  Vector<Int> antNewIndex_p;
  Vector<Int> feedNewIndex_p;
};


} //# NAMESPACE CASA - END

#endif

