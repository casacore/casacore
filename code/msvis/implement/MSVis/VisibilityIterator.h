//# VisibilityIterator.h: Step through the MeasurementEquation by visibility
//# Copyright (C) 1996
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

#if !defined(TRIAL_VISIBILITYITERATOR_H)
#define TRIAL_VISIBILITYITERATOR_H

#include <aips/aips.h>
#include <aips/Measures/Stokes.h>
#include <aips/Measures/MDirection.h>
#include <aips/Utilities/String.h>
#include <aips/MeasurementSets/MeasurementSet.h>
#include <trial/MeasurementEquations/StokesVector.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/TableIter.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Cube.h>

#include <aips/Logging/LogSink.h>

//# forward decl
class VisBuffer;
class ROVisibilityIterator;
class VisibilityIterator;

// Small helper class to specify an 'interval' comparison for table iteration
class Interval {
public:
    static void setOffset(Double offset) {offset_p=offset;}
    static void setInterval(Double interval) {interval_p=interval;}
    static Int compare(const void * obj1, const void * obj2);
private:
    static Double interval_p;
    static Double offset_p;
};

//# To think about: BandPass calibration - should be done BEFORE averaging
//# channels - move averaging to VisEquation::correct()?
class ROVisibilityIterator
{
public:
  enum PolFrame {
    // Circular polarization
    Circular=0,
    // Linear polarization
    Linear=1
  };

  // Default constructor - useful only to assign another iterator later
  ROVisibilityIterator();

  // Construct from MS and a Block of MS column enums specifying the 
  // iteration order, if none are specified, time iteration is implicit.
  // An optional timeInterval can be given to iterate through chunks of time.
  // The default interval of 0 groups all times together.
  // Every 'chunk' of data contains all data within a certain time interval
  // and with identical values of the other iteration columns (e.g.
  // SPECTRAL_WINDOW_ID and FIELD_ID).
  // Using selectChannel(), a number of groups of channels can be requested.
  // At present the channel group iteration will always occur before the 
  // interval iteration.
  ROVisibilityIterator
      (const MeasurementSet& ms, const Block<Int>& sortColumns, 
       Double timeInterval=0);
  // Copy construct. This calls the assigment operator.
  ROVisibilityIterator(const ROVisibilityIterator & other);

  // Assigment. Any attached VisBuffers are lost in the assign.
  ROVisibilityIterator & operator=(const ROVisibilityIterator &other);

  // Destructor

  virtual ~ROVisibilityIterator();
  
  // Members
  
  // Reset iterator to origin/start of data (of current chunk)
  void origin();
  // Reset iterator to true start of data (first chunk)
  void originChunks();
 
  // Set or reset the time interval to use for iteration.
  // You should call originChunks() to reset the iteration after 
  // calling this.
  void setInterval(Double timeInterval);
 
  // Return False if no more data (in current chunk)
  Bool more() const;

  // Advance iterator through data
  ROVisibilityIterator & operator++(int);
  ROVisibilityIterator & operator++();

  // Return False if no more 'Chunks' of data left
  Bool moreChunks() const;

  // Advance to the next Chunk of data
  ROVisibilityIterator& nextChunk();

  // Return antenna1 
  Vector<Int>& antenna1(Vector<Int>& ant1) const;

  // Return antenna2 
  Vector<Int>& antenna2(Vector<Int>& ant2) const;

  // Return channel numbers in selected VisSet spectrum
  // (i.e. disregarding possible selection on the iterator, but
  //  including the selection set when creating the VisSet)
  Vector<Int>& channel(Vector<Int>& chan) const;

  // Return feed configuration matrix for specified antenna
  Vector<SquareMatrix<Complex,2> >& 
  CJones(Vector<SquareMatrix<Complex,2> >& cjones) const;

  // Return feed parallactic angles Vector(nant) (1 feed/ant)
  const Vector<Float>& feed_pa(Double time) const;

  // Return the current FieldId
  Int fieldId() const;

  // Return flag for each channel & row
  Matrix<Bool>& flag(Matrix<Bool>& flags) const;

  // Return row flag
  Vector<Bool>& flagRow(Vector<Bool>& rowflags) const;

  // Return current frequencies
  Vector<Double>& frequency(Vector<Double>& freq) const;

  // Return the current phase center as 2-vector of positions in radians.
  const MDirection& phaseCenter() const;

  // Return frame for polarization (returns PolFrame enum)
  Int polFrame() const;

  // Return sigma
  Vector<Float>& sigma(Vector<Float>& sig) const;

  // Return current SpectralWindow
  Int spectralWindow() const;

  // Return MJD 
  Vector<Double>& time(Vector<Double>& t) const;

  // Return the visibility 4-vector of polarizations for each channel
  Matrix<CStokesVector>& visibility(Matrix<CStokesVector>& vis) const;

  // Return u,v and w (in meters)
  Vector<RigidVector<Double,3> >& 
  uvw(Vector<RigidVector<Double,3> >& uvwvec) const;

  // Return weight
  Vector<Float>& weight(Vector<Float>& wt) const;

  // Return True if FieldId/Source has changed since last iteration
  Bool newFieldId() const;

  // Return True if SpectralWindow has changed since last iteration
  Bool newSpectralWindow() const;

  // Return the index of the first channel of the current channel group 
  // in the total (selected) spectrum.
  Int channelIndex() const;

  // Return the width of the current group of channels, i.e.,
  // the number of channels returned by visibility() and frequency().
  Int channelGroupSize() const;
  
  // Return the number of rows in the current iteration
  Int nRow() const;

  // Channel selection - only the selected channels will be returned by the
  // access functions. The default spectralWindow is the current one (or 0)
  // This allows selection of the input channels, producing
  // nGroup groups of width output channels. Default is to return all channels
  // in a single group.
  ROVisibilityIterator& selectChannel(Int nGroup=1, Int start=0, Int width=0, 
				      Int increment=0, Int spectralWindow=-1);

  // Attach a VisBuffer object.
  // Note that while more than one VisBuffer may be attached, only the
  // last one is actively updated. A Stack is kept internally, so after 
  // a detach, the previous VisBuffer becomes active again.
  void attachVisBuffer(VisBuffer& vb);

  // Detach a VisBuffer object.
  // If the object detached is not the last one attached an exception
  // is thrown.
  void detachVisBuffer(VisBuffer& vb);

protected:
  // advance the iteration
  void advance();
  // set the currently selected table
  void setSelTable();
  // set the iteration state
  void setState();
  // update the DATA slicer
  void updateSlicer();
  // attach the column objects to the currently selected table
  virtual void attachColumns();

  ROVisibilityIterator* This;
  MeasurementSet  ms;
  TableIterator tabIter_p;
  Bool haveIter_p; // do we have an Iterator yet
  Table curTable_p; // as given by table iterator
  Table selTable_p; // currently selected set of rows from curTable
  Int curFieldID_p, lastFieldID_p, curSpectralWindow_p, lastSpectralWindow_p,
      curChanGroup_p, curNumChanGroup_p, channelGroupSize_p, 
      curNumRow_p, curTableNumRow_p, curStartRow_p, curEndRow_p,
      nChan_p, nPol_p;
  Bool more_p, newFieldID_p, newSpectralWindow_p, newChanGroup_p, 
      moreChunks_p, preselected_p;

  // time selection
  Double interval_p;
  // channel selection
  Block<Int> numChanGroup_p, chanStart_p, chanWidth_p, chanInc_p,
    preselectedChanStart_p,preselectednChan_p;
  
  // Stack of VisBuffer objects
  Stack<void*> vbStack_p;


  //cache for access functions
  Slicer slicer_p;
  Bool useSlicer_p;
  Vector<Double> time_p;
  Vector<Double> frequency_p;
  Bool freqCacheOK_p;
  MDirection phaseCenter_p;
  Cube<Bool> flagCube_p;
  Cube<Complex> visCube_p;
  Matrix<Double> uvwMat_p;
  Vector<Float> pa_p;
  Vector<Int> mount_p;
  Vector<Double> receptorAngle_p;
  PolFrame polFrame_p;
  Vector<SquareMatrix<Complex,2> > CJones_p;

  // cache for PA calculations
  Bool first_pa_p;
  Double lastUT_p;
  Int nAnt_p;

  // column access functions
  ROScalarColumn<Int> colSpectralWindow, colAntenna1, colAntenna2, colFieldID;
  ROArrayColumn<Double> colChanFreq;
  ROScalarColumn<Double> colTime;
  ROScalarColumn<Float> colWeight;
  ROArrayColumn<Complex> colVis;
  ROArrayColumn<Float> colSigma;
  ROArrayColumn<Bool> colFlag;
  ROScalarColumn<Bool> colFlagRow;
  ROArrayColumn<Double> colUVW;

  ROArrayColumn<Double> colDirection;
  ROArrayColumn<Double> colAntPos;
  ROScalarColumn<String> colMount;
};

inline Bool ROVisibilityIterator::more() const { return more_p;}
inline Bool ROVisibilityIterator::moreChunks() const { return moreChunks_p;}
inline Bool ROVisibilityIterator::newFieldId() const { return newFieldID_p;}
inline Bool ROVisibilityIterator::newSpectralWindow() const 
{ return newSpectralWindow_p;}
inline Int ROVisibilityIterator::fieldId() const { return curFieldID_p;}
inline Int ROVisibilityIterator::spectralWindow() const 
{ return curSpectralWindow_p;}
inline Int ROVisibilityIterator::polFrame() const { return polFrame_p;}
inline Vector<SquareMatrix<Complex,2> >& 
ROVisibilityIterator::CJones(Vector<SquareMatrix<Complex,2> >& cjones) const 
{ cjones.resize(CJones_p.nelements()); return cjones=CJones_p; }
inline Int ROVisibilityIterator::channelGroupSize() const
{ return chanWidth_p[curSpectralWindow_p]; }
inline Int ROVisibilityIterator::channelIndex() const
{ return chanInc_p[curSpectralWindow_p]*curChanGroup_p; }
inline Int ROVisibilityIterator::nRow() const
{ return curNumRow_p;}


//# The read/write version of the VisibilityIterator
class VisibilityIterator : public ROVisibilityIterator
{
public:

  // Constructors.
  // Note: The VisibilityIterator is not initialized correctly by default, you
  // need to call origin() before using it to iterate.
  VisibilityIterator();
  VisibilityIterator(MeasurementSet & ms, const Block<Int>& sortColumns, 
       Double timeInterval=0);
  VisibilityIterator(const VisibilityIterator & MSI);

  VisibilityIterator & operator=(const VisibilityIterator &MSI);

  // Destructor

  virtual ~VisibilityIterator();
  
  // Members
  
  // Advance iterator through data
  VisibilityIterator & operator++(int);
  VisibilityIterator & operator++();

  // Set/modify the weights in the data
  void setWeight(const Vector<Float>& wt);

  // Set/modify the flags in the data.
  // This will flag all channels in the original data that contributed to
  // the output channel in the case of channel averaging.
  void setFlag(const Matrix<Bool>& flag);

  // Set/modify the visibilities.
  // This is possibly only for a 'reference' MS which has a new DATA column.
  // The length of the vector should equal the selected number of channels
  // in the original MS.
  void setVis(const Matrix<CStokesVector>& vis);

protected:
  virtual void attachColumns();

  // column access functions
  ArrayColumn<Complex> RWcolVis;
  ScalarColumn<Float> RWcolWeight;
  ArrayColumn<Bool> RWcolFlag;

};

#endif

