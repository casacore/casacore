//# MSIter.h: Step through the MeasurementEquation by table
//# Copyright (C) 1996,1999,2000,2001,2002
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

#ifndef MS_MSITER_H
#define MS_MSITER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/Utilities/Compare.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/scimath/Mathematics/SquareMatrix.h>
#include <casacore/scimath/Mathematics/RigidVector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
class MSInterval : public BaseCompare
{
public:
  explicit MSInterval(Double interval) : interval_p(interval), offset_p(0) {}
    virtual ~MSInterval() {}
    virtual int comp(const void * obj1, const void * obj2) const;
    Double getOffset() {return offset_p;}
    void setOffset(Double offset) {offset_p=offset;}
    Double getInterval() {return interval_p;}
    void setInterval(Double interval) {interval_p=interval;}
private:
    Double interval_p;
    mutable Double offset_p;
};

// <summary> 
// An iterator class for MeasurementSets
// </summary>
 
// <use visibility=export>
 
// <prerequisite>
//   <li> <linkto class="MeasurementSet:description">MeasurementSet</linkto> 
// </prerequisite>
//
// <etymology>
// MSIter stands for the MeasurementSet Iterator class.
// </etymology>
//
// <synopsis> 
// An MSIter is a class to traverse a MeasurementSet in various orders.  It
// automatically adds four predefined sort columns to your selection of sort
// columns (see constructor) so that it can keep track of changes in frequency
// or polarization setup, field position and sub-array.  Note that this can
// cause iterations to occur in a different way from what you would expect, see
// examples below.  MSIter implements iteration by time interval for the use of
// e.g., calibration tasks that want to calculate solutions over some interval
// of time.  You can iterate over multiple MeasurementSets with this class.
// </synopsis> 
//
// <example>
// <srcblock>
// // The following code iterates by by ARRAY_ID, FIELD_ID, DATA_DESC_ID and
// // TIME (all implicitly added columns) and then by baseline (antenna pair),
// // in 3000s intervals.
// MeasurementSet ms("3C273XC1.ms"); 
// Block<int> sort(2);
//        sort[0] = MS::ANTENNA1;
//        sort[1] = MS::ANTENNA2;
// Double timeInterval = 3000;
// MSIter msIter(ms,sort,timeInteval);
// for (msIter.origin(); msIter.more(); msIter++) {
// // print out some of the iteration state
//    cout << msIter.fieldId() << endl;
//    cout << msIter.fieldName() << endl;
//    cout << msIter.dataDescriptionId() << endl;
//    cout << msIter.frequency0() << endl;
//    cout << msIter.table().nrow() << endl;
//    process(msIter.table()); // process the data in the current iteration
// }
// // Output shows only 1 row at a time because the table is sorted on TIME
// // first and ANTENNA1, ANTENNA2 next and each baseline occurs only once per 
// // TIME stamp. The interval has no effect in this case.
// </srcblock>
// </example>

// <example>
// <srcblock>
// // The following code iterates by baseline (antenna pair), TIME, and,
// // implicitly, by ARRAY_ID, FIELD_ID and DATA_DESC_ID in 3000s
// // intervals.
// MeasurementSet ms("3C273XC1.ms"); 
// Block<int> sort(3);
//        sort[0] = MS::ANTENNA1;
//        sort[1] = MS::ANTENNA2;
//        sort[2] = MS::TIME;
// Double timeInterval = 3000;
// MSIter msIter(ms,sort,timeInteval);
// for (msIter.origin(); msIter.more(); msIter++) {
// // print out some of the iteration state
//    cout << msIter.fieldId() << endl;
//    cout << msIter.fieldName() << endl;
//    cout << msIter.dataDescriptionId() << endl;
//    cout << msIter.frequency0() << endl;
//    cout << msIter.table().nrow() << endl;
//    process(msIter.table()); // process the data in the current iteration
// // Now the output shows 7 rows at a time, all with identical ANTENNA1
// // and ANTENNA2 values and TIME values within a 3000s interval.
// }
// </srcblock>
// </example>
//
// <motivation>
// This class was originally part of the VisibilityIterator class, but that 
// class was getting too large and complicated. By splitting out the toplevel
// iteration into this class the code is much easier to understand. It is now
// also available through the ms tool.
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
  // iteration order, if none are specified, ARRAY_ID, FIELD_ID, DATA_DESC_ID,
  // and TIME iteration is implicit (unless addDefaultSortColumns=False)
  // These columns will be added first if they are not specified.
  // An optional timeInterval can be given to iterate through chunks of time.
  // The default interval of 0 groups all times together.
  // Every 'chunk' of data contains all data within a certain time interval
  // and with identical values of the other iteration columns (e.g.
  // DATA_DESCRIPTION_ID and FIELD_ID).
  // See the examples above for the effect of different sort orders.
  MSIter(const MeasurementSet& ms, const Block<Int>& sortColumns, 
	 Double timeInterval=0, Bool addDefaultSortColumns=True);

  // Same as above with multiple MSs as input.
  MSIter(const Block<MeasurementSet>& mss, const Block<Int>& sortColumns, 
	 Double timeInterval=0, Bool addDefaultSortColumns=True);

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

  // Return the current ArrayId
  Int arrayId() const;

  // Return True if ArrayId has changed since last iteration
  Bool newArray() const;

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
  // TODO: CJonesAll can be used instead of this method in all instances
  const Vector<SquareMatrix<Complex,2> >& CJones() const;

  // Return the feed configuration/leakage matrix for all feeds and antennae
  // First axis is antennaId, 2nd axis is feedId. Result of CJones() is
  // a reference to the first column of the matrix returned by this method
  const Matrix<SquareMatrix<Complex,2> >& CJonesAll() const;

  // Return the receptor angle for feed 0 on each antenna.
  // First axis is receptor number, 2nd axis is antennaId.
  // TODO: receptorAngles() can be used instead of this method
  const Matrix<Double>& receptorAngle() const;

  // Return the receptor angles for all feeds and antennae
  // First axis is a receptor number, 2nd axis is antennaId,
  // 3rd axis is feedId. Result of receptorAngle() is just a reference
  // to the first plane of the cube returned by this method
  const Cube<Double>& receptorAngles() const;

  // Return the channel number of the first channel in the DATA.
  // (non-zero for reference MS created by VisSet with channel selection)
  Int startChan() const;

  // Return a string mount identifier for each antenna
  const Vector<String>& antennaMounts() const;

  // Return a cube containing pairs of coordinate offset for each receptor
  // of each feed (values are in radians, coordinate system is fixed with 
  // antenna and is the same as used to define the BEAM_OFFSET parameter 
  // in the feed table). The cube axes are receptor, antenna, feed. 
  const Cube<RigidVector<Double, 2> >& getBeamOffsets() const;

  // True if all elements of the cube returned by getBeamOffsets are zero
  Bool allBeamOffsetsZero() const;

  // Get the spw, start  and nchan for all the ms's is this msiter that 
  // match the frequecy "freqstart-freqStep" and "freqEnd+freqStep" range
  
  void getSpwInFreqRange(Block<Vector<Int> >& spw, 
			 Block<Vector<Int> >& start, 
			 Block<Vector<Int> >& nchan, 
			 Double freqStart, Double freqEnd, 
			 Double freqStep);

  //Get the number of actual ms's associated wth this iterator
  Int numMS() const;

  //Get a reference to the nth ms in the list of ms associated with this 
  // iterator. If larger than the list of ms's current ms is returned
  // So better check wth numMS() before making the call
  const MS& ms(const uInt n) const;

protected:
  // handle the construction details
  void construct(const Block<Int>& sortColumns, Bool addDefaultSortColumns);
  // advance the iteration
  void advance();
  // set the iteration state
  void setState();
  void setMSInfo();
  void setArrayInfo();
  void setFeedInfo();
  void setDataDescInfo();
  void setFieldInfo();

// Determine if the numbers in r1 are a sorted subset of those in r2
  Bool isSubSet(const Vector<uInt>& r1, const Vector<uInt>& r2);

  MSIter* This;
  Block<MeasurementSet> bms_p;
  PtrBlock<TableIterator* > tabIter_p;
  Block<Bool> tabIterAtStart_p;

  Int nMS_p;
  ROMSColumns* msc_p;
  Table curTable_p;
  Int curMS_p, lastMS_p, curArray_p, lastArray_p, curSource_p;
  String curFieldName_p, curSourceName_p;
  Int curField_p, lastField_p, curSpectralWindow_p, lastSpectralWindow_p;
  Int curPolarizationId_p, lastPolarizationId_p;
  Int curDataDescId_p, lastDataDescId_p;
  Bool more_p, newMS_p, newArray_p, newField_p, newSpectralWindow_p, 
    newPolarizationId_p, newDataDescId_p, preselected_p,
    timeDepFeed_p, spwDepFeed_p, checkFeed_p;
  Int startChan_p;

  // time selection
  Double interval_p;
  // channel selection
  Block<Int> preselectedChanStart_p,preselectednChan_p;
  
  // columns
  ScalarColumn<Int> colArray_p, colDataDesc_p, colField_p;

  //cache for access functions
  MDirection phaseCenter_p;
  Matrix<Double> receptorAnglesFeed0_p; // former receptorAngle_p,
                                   // temporary retained for compatibility
                                   // contain actually a reference to the
				   // first plane of receptorAngles_p
  Cube<Double> receptorAngles_p;
  Vector<SquareMatrix<Complex,2> > CJonesFeed0_p; // a temporary reference
                                   // similar to receptorAngle_p
  Matrix<SquareMatrix<Complex,2> > CJones_p;
  Vector<String>  antennaMounts_p; // a string mount identifier for each
                                   // antenna (e.g. EQUATORIAL, ALT-AZ,...)
  Cube<RigidVector<Double, 2> > beamOffsets_p;// angular offsets (two values for
                                   // each element of the cube in radians) 
				   // in the antenna coordinate system. 
				   // Cube axes are: receptor, antenna, feed.
  Bool allBeamOffsetsZero_p;       // True if all elements of beamOffsets_p
                                   // are zero (to speed things up in a 
				   // single beam case)
  PolFrame polFrame_p;
  Bool freqCacheOK_p;
  Vector<Double> frequency_p;
  MFrequency frequency0_p;
  MFrequency restFrequency_p;
  MPosition telescopePosition_p;

  MSInterval *timeComp_p;          // Points to the time comparator.
                                   // 0 if not using a time interval.
};

inline Bool MSIter::more() const { return more_p;}
inline Table MSIter::table() const {return curTable_p;}
inline const MS& MSIter::ms() const {return bms_p[curMS_p];}
inline const ROMSColumns& MSIter::msColumns() const { return *msc_p;}
inline Bool MSIter::newMS() const { return newMS_p;}
inline Bool MSIter::newArray() const {return newArray_p;}
inline Bool MSIter::newField() const { return newField_p;}
inline Bool MSIter::newSpectralWindow() const 
{ return newSpectralWindow_p;}
inline Int MSIter::msId() const { return curMS_p;}
inline Int MSIter::numMS() const { return nMS_p;}
inline Int MSIter::arrayId() const {return curArray_p;}
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
{ return CJonesFeed0_p;}
inline const Matrix<SquareMatrix<Complex,2> >& MSIter::CJonesAll() const
{ return CJones_p;}
inline const Matrix<Double>& MSIter::receptorAngle() const 
{return receptorAnglesFeed0_p;}
inline const Cube<Double>& MSIter::receptorAngles() const
{return receptorAngles_p;}
inline const Vector<String>& MSIter::antennaMounts() const
{return antennaMounts_p;}
inline const Cube<RigidVector<Double, 2> >& MSIter::getBeamOffsets() const
{return beamOffsets_p;}
inline Int MSIter::startChan() const {return startChan_p;}
inline Bool MSIter::allBeamOffsetsZero() const {return allBeamOffsetsZero_p;}

} //# NAMESPACE CASACORE - END

#endif
