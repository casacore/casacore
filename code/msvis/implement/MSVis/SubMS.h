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
  
  //void selectSource(Vector<String> sourceid);

  //Method to set if a phase Center rotation is needed
  //void setPhaseCenter(Int fieldid, MDirection& newPhaseCenter);



  //Method to make the subMS

  Bool makeSubMS(String& submsname, String& whichDataCol);

  // This setup a default new ms
  // Can be called directly as its not dependent on any private variable
  MeasurementSet* setupMS(String msname, Int nchan, Int npol, String telescop, Int obstype=0);
  
 
  

 private:

  //method that returns the selected ms
  Bool makeSelection();
  Bool fillDDTables();
  Bool fillFieldTable();
  Bool fillMainTable(const String& which);
  Bool copyAntenna();
  Bool copyFeed();
  Bool copyObservation();
  Bool writeDiffSpwShape(String& columnName);
  Bool writeSimilarSpwShape(String& columnName);
  MSColumns * msc_p;
  MSColumns * mscIn_p;

  MeasurementSet ms_p, mssel_p, msOut_p;
  Vector<Int> spw_p, nchan_p, chanStart_p, chanStep_p, npol_p, inNumChan_p;
  Vector<Int> fieldid_p;
  Bool averageChannel_p;
  Vector<Int> spwRelabel_p, fieldRelabel_p;
  Vector<Int> oldDDSpwMatch_p;
  Bool doChanAver_p;

};


} //# NAMESPACE CASA - END

#endif

