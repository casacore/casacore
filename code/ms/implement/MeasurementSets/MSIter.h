//# MSIter.h: Step through the MeasurementEquation by table
//# Copyright (C) 1996,1999,2000
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

#if !defined(TRIAL_MSITER_H)
#define TRIAL_MSITER_H

#include <aips/aips.h>
#include <aips/Arrays/Matrix.h>
#include <aips/MeasurementSets/MeasurementSet.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MPosition.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Utilities/String.h>
#include <trial/Arrays/SquareMatrix.h>

//# forward decl
class ROMSColumns;
class TableIterator;

// <summary>
// Small helper class to specify an 'interval' comparison
// </summary>
// <synopsis>
// Small helper class to specify an 'interval' comparison for table iteration
// by time interval.
// </synopsis>
class MSInterval {
public:
    static void setOffset(Double offset) {offset_p=offset;}
    static void setInterval(Double interval) {interval_p=interval;}
    static Int compare(const void * obj1, const void * obj2);
private:
    static Double interval_p;
    static Double offset_p;
};

// <summary> 
// An iterator class for MeasurementSets
// </summary>
 
// <use visibility=export>
 
// <prerequisite>
// <ul>
//   <li> <linkto class="MeasurementSet:description">MeasurementSet</linkto> 
// </ul>
// </prerequisite>
//
// <etymology>
// MSIter stands for the MeasurementSet Iterator class.
// </etymology>
//
// <synopsis> 
// An MSIter is a class to traverse a MeasurementSet in various orders.
// It keeps track of some internal state so that changes in e.g. frequency
// or field position can be noticed. It implements iteration by time interval
// for the use of e.g., calibration tasks that want to calculate solution over
// some interval of time. You can iteratate over multiple MeasurementSets
// with this class.
// </synopsis> 
//
// <example>
// <srcblock>
// // use as follows
// MeasurementSet ms("myMS"); 
// Block<int> sort(3);
//        sort[0] = MS::FIELD_ID;
//        sort[2] = MS::DATA_DESCRIPTION_ID;
//        sort[3] = MS::TIME;
// Double timeInterval = 30;
// MSIter msIter(ms,sort,timeInteval);
// for (msIter.origin(); msIter.more(); msIter++) {
// // print out some of the iteration state
//    cout << msIter.fieldId() << endl;
//    cout << msIter.fieldName() << endl;
//    cout << msIter.dataDescriptionId() << endl;
//    cout << msIter.frequency0() << endl;
//    process(msIter.table()); // process the data in the current iteration
// }
// </srcblock>
// </example>
//
// <motivation>
// This class was originally part of the VisibilityIterator class, but that 
// class was getting too large and complicated. By splitting out the toplevel
// iteration into this class the code is much easier to understand.
// </motivation>
//
// <todo>
// multiple observatories in a single MS are not handled correctly (need to
// sort on observation id and check observatory name to set position frame)
// </todo>

class MSIter
{
public:
  enum PolFrame {
    // Circular polarization
    Circular=0,
    // Linear polarization
    Linear=1
  };

  // Default constructor - useful only to assign another iterator later.
  // Use of other member functions on this object is likely to dump core.
  MSIter();

  // Construct from MS and a Block of MS column enums specifying the 
  // iteration order, if none are specified, time, field and
  // dataDescription iteration is implicit. 
  // These will be added (last) if not already specified.
  // An optional timeInterval can be given to iterate through chunks of time.
  // The default interval of 0 groups all times together.
  // Every 'chunk' of data contains all data within a certain time interval
  // and with identical values of the other iteration columns (e.g.
  // DATA_DESCRIPTION_ID and FIELD_ID).
  MSIter(const MeasurementSet& ms, const Block<Int>& sortColumns, 
	 Double timeInterval=0);

  // Same as above with multiple MSs as input.
  MSIter(const Block<MeasurementSet>& mss, const Block<Int>& sortColumns, 
	 Double timeInterval=0);

  // Copy construct. This calls the assigment operator.
  MSIter(const MSIter & other);

  // Destructor
  virtual ~MSIter();
  
  // Assigment. This will reset the iterator to the origin.
  MSIter & operator=(const MSIter &other);

  //# Members
 
  // Set or reset the time interval to use for iteration.
  // You should call origin() to reset the iteration after 
  // calling this.
  void setInterval(Double timeInterval);
 
  // Reset iterator to start of data
  void origin();
 
  // Return False if there is no more data
  Bool more() const;

  // Advance iterator through data
  MSIter & operator++(int);
  MSIter & operator++();

  // Return the current Table iteration
  Table table() const;

  // Return reference to the current MS
  const MS& ms() const;

  // Return reference to the current ROMSColumns
  const ROMSColumns& msColumns() const;

  // Return the current MS Id (according to the order in which 
  // they appeared in the constructor)
  Int msId() const;

  // Return true if msId has changed since last iteration
  Bool newMS() const;

  // Return the current FieldId
  Int fieldId() const;

  // Return the current Field Name
  const String& fieldName() const;

  // Return the current Source Name
  const String& sourceName() const;

  // Return True if FieldId/Source has changed since last iteration
  Bool newField() const;

  // Return current SpectralWindow
  Int spectralWindowId() const;

  // Return True if SpectralWindow has changed since last iteration
  Bool newSpectralWindow() const;

  // Return current DataDescriptionId
  Int dataDescriptionId() const;

  // Return True if DataDescriptionId has changed since last iteration
  Bool newDataDescriptionId() const;

  // Return current PolarizationId
  Int polarizationId() const;

  // Return True if polarization has changed since last iteration
  Bool newPolarizationId() const;

  // Return the current phase center as MDirection
  const MDirection& phaseCenter() const;

  // Return frame for polarization (returns PolFrame enum)
  Int polFrame() const;

  // Return the frequencies corresponding to the DATA matrix.
  const Vector<Double>& frequency() const;

  // Return frequency of first channel with reference frame as a Measure.
  // The reference frame Epoch is that of the first row, reset it as needed
  // for each row.
  // The reference frame Position is the average of the antenna positions.
  const MFrequency& frequency0() const;

  // Return the rest frequency of the specified line as a Measure
  const MFrequency& restFrequency(Int line=0) const;

  // Return the telescope position (if a known telescope) or the 
  // position of the first antenna (if unknown)
  const MPosition& telescopePosition() const;

  // Return the feed configuration/leakage matrix for feed 0 on each antenna
  const Vector<SquareMatrix<Complex,2> >& CJones() const;

  // Return the receptor angle for feed 0 on each antenna.
  // First axis is receptor number, 2nd axis is antennaId.
  const Matrix<Double>& receptorAngle() const;

  // Return the channel number of the first channel in the DATA.
  // (non-zero for reference MS created by VisSet with channel selection)
  Int startChan() const;

protected:
  // handle the construction details
  void construct(const Block<Int>& sortColumns);
  // advance the iteration
  void advance();
  // set the iteration state
  void setState();
  void setMSInfo();
  void setFeedInfo();
  void setDataDescInfo();
  void setFieldInfo();

// Determine if the numbers in r1 are a sorted subset of those in r2
  Bool isSubSet(const class Vector<uInt>& r1, const class Vector<uInt>& r2);

  MSIter* This;
  Block<MeasurementSet> bms_p;
  PtrBlock<TableIterator* > tabIter_p;
  Block<Bool> tabIterAtStart_p;

  Int nMS_p;
  ROMSColumns* msc_p;
  Table curTable_p;
  Int curMS_p, lastMS_p, curSource_p;
  String curFieldName_p, curSourceName_p;
  Int curField_p, lastField_p, curSpectralWindow_p, lastSpectralWindow_p;
  Int curPolarizationId_p, lastPolarizationId_p;
  Int curDataDescId_p, lastDataDescId_p;
  Bool more_p, newMS_p, newField_p, newSpectralWindow_p, 
    newPolarizationId_p, newDataDescId_p, preselected_p,
    timeDepFeed_p, spwDepFeed_p, checkFeed_p;
  Int startChan_p;

  // time selection
  Double interval_p;
  // channel selection
  Block<Int> preselectedChanStart_p,preselectednChan_p;
  
  // columns
  ROScalarColumn<Int> colDataDesc_p, colField_p;

  //cache for access functions
  MDirection phaseCenter_p;
  Matrix<Double> receptorAngle_p;
  Vector<SquareMatrix<Complex,2> > CJones_p;
  PolFrame polFrame_p;
  Bool freqCacheOK_p;
  Vector<Double> frequency_p;
  MFrequency frequency0_p;
  MFrequency restFrequency_p;
  MPosition telescopePosition_p;
};

inline Bool MSIter::more() const { return more_p;}
inline Table MSIter::table() const {return curTable_p;}
inline const MS& MSIter::ms() const {return bms_p[curMS_p];}
inline const ROMSColumns& MSIter::msColumns() const { return *msc_p;}
inline Bool MSIter::newMS() const { return newMS_p;}
inline Bool MSIter::newField() const { return newField_p;}
inline Bool MSIter::newSpectralWindow() const 
{ return newSpectralWindow_p;}
inline Int MSIter::msId() const { return curMS_p;}
inline Int MSIter::fieldId() const { return curField_p;}
inline const String& MSIter::fieldName() const { return curFieldName_p;}
inline const String& MSIter::sourceName() const { return curSourceName_p;}
inline Int MSIter::spectralWindowId() const 
{ return curSpectralWindow_p;}
inline Int MSIter::polarizationId() const {return curPolarizationId_p;}
inline Int MSIter::dataDescriptionId() const {return curDataDescId_p;}
inline Bool MSIter::newPolarizationId() const { return newPolarizationId_p;}
inline Bool MSIter::newDataDescriptionId() const { return newDataDescId_p;}
inline Int MSIter::polFrame() const { return polFrame_p;}
inline const MDirection& MSIter::phaseCenter() const 
{ return phaseCenter_p; }
inline const MPosition& MSIter::telescopePosition() const
{ return telescopePosition_p;}
inline const Vector<SquareMatrix<Complex,2> >& MSIter::CJones() const  
{ return CJones_p;}
inline const Matrix<Double>& MSIter::receptorAngle() const 
{return receptorAngle_p;}
inline Int MSIter::startChan() const {return startChan_p;}
#endif
