//# VisibilityIterator.h: Step through the MeasurementEquation by visibility
//# Copyright (C) 1996,1997,1998
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
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Cube.h>
#include <aips/Lattices/Slicer.h>
#include <aips/MeasurementSets/MeasurementSet.h>
#include <aips/Measures/Stokes.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Quanta/MVDoppler.h>
#include <aips/Measures/MCDoppler.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Utilities/String.h>

#include <trial/MeasurementSet/MSDerivedValues.h>
#include <trial/MeasurementEquations/StokesVector.h>
#include <trial/MeasurementSet/MSIter.h>

//# forward decl
class VisBuffer;

// <summary>
// ROVisibilityIterator iterates through one or more readonly MeasurementSets
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="MSIter">MSIter</linkto>
//   <li> <linkto class="MeasurementSet">MeasurementSet</linkto>
//   <li> <linkto class="VisSet">VisSet</linkto>
// </prerequisite>
//
// <etymology>
// The ROVisibilityIterator is a readonly iterator returning visibilities
// </etymology>
//
// <synopsis>
// ROVisibilityIterator provides iteration with various sort orders
// for one or more MSs. It has member functions to retrieve the fields
// commonly needed in synthesis calibration and imaging.
//
// One should use <linkto class="VisBuffer">VisBuffer</linkto>
// to access chunks of data.
// </synopsis>
//
// <example>
// <code>
// //
// </code>
// </example>
//
// <motivation>
// For imaging and calibration you need to access an MS in some consistent
// order (by field, spectralwindow, time interval etc.). This class provides
// that access.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="1997/05/30">
//   <li> cleanup the currently dual interface for visibilities and flags
//   <li> sort out what to do with weights when interpolating
// </todo>

class ROVisibilityIterator
{
public:

  //# the following is a copy of the enum in MSIter
  //# can't think of a way to get one that known to the outside world from here
  enum PolFrame {
    // Circular polarization
    Circular=0,
    // Linear polarization
    Linear=1
  };

  enum DataColumn {
    // Observed data
    Observed=0,
    // Model data
    Model,
    // Corrected data
    Corrected
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
  ROVisibilityIterator(const MeasurementSet& ms, 
		       const Block<Int>& sortColumns, 
		       Double timeInterval=0);

  // Same as previous constructor, but with multiple MSs to iterate over.
  ROVisibilityIterator(const Block<MeasurementSet>& mss,
		       const Block<Int>& sortColumns, 
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
  void setInterval(Double timeInterval)
  { msIter_p.setInterval(timeInterval);}

  // Set the 'blocking' size for returning data.
  // With the default (0) only a single integration is returned at a time, this
  // is what is currently required for the calibration software. With blocking
  // set, up to nRows can be returned in one go. The chunk 
  // size determines the actual maximum.
  void setRowBlocking(Int nRows=0);

  // Return False if no more data (in current chunk)
  Bool more() const;

  // Advance iterator through data
  ROVisibilityIterator & operator++(int);
  ROVisibilityIterator & operator++();

  // Return False if no more 'Chunks' of data left
  Bool moreChunks() const
  { return msIter_p.more();}

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
  Int fieldId() const
  { return msIter_p.fieldId(); }

  // Return flag for each polarization, channel and row
  Cube<Bool>& flag(Cube<Bool>& flags) const;

  // Return flag for each channel & row
  Matrix<Bool>& flag(Matrix<Bool>& flags) const;

  // Return row flag
  Vector<Bool>& flagRow(Vector<Bool>& rowflags) const;

  // Return current frequencies
  Vector<Double>& frequency(Vector<Double>& freq) const;

  // Return frequencies in selected velocity frame,
  // returns the same as frequency() if there is no vel selection active.
  Vector<Double>& lsrFrequency(Vector<Double>& freq) const;

  // Return the current phase center as an MDirection
  const MDirection& phaseCenter() const
  {return msIter_p.phaseCenter(); }

  // Return frame for polarization (returns PolFrame enum)
  Int polFrame() const
  { return msIter_p.polFrame(); }

  // Return the correlation type (returns Stokes enums)
  Vector<Int>& corrType(Vector<Int>& corrTypes) const;

  // Return sigma
  Vector<Float>& sigma(Vector<Float>& sig) const;

  // Return current SpectralWindow
  Int spectralWindow() const
  { return msIter_p.spectralWindowId(); }

  // Return MJD 
  Vector<Double>& time(Vector<Double>& t) const;

  // Return the visibilities as found in the MS, Cube(npol,nchan,nrow).
  Cube<Complex>& visibility(Cube<Complex>& vis,
			    DataColumn whichOne) const;

  // Return the visibility 4-vector of polarizations for each channel.
  // If the MS doesn't contain all polarizations, it is assumed it
  // contains one or two parallel hand polarizations.
  Matrix<CStokesVector>& visibility(Matrix<CStokesVector>& vis, 
				    DataColumn whichOne) const;

  // Return u,v and w (in meters)
  Vector<RigidVector<Double,3> >& 
  uvw(Vector<RigidVector<Double,3> >& uvwvec) const;

  // Return weight
  Vector<Float>& weight(Vector<Float>& wt) const;

  // Return imaging weight (a weight for each channel)
  Matrix<Float>& imagingWeight(Matrix<Float>& wt) const;

  // Return True if FieldId/Source has changed since last iteration
  Bool newFieldId() const
  { return ToBool(curStartRow_p==0 && msIter_p.newField()); }

  // Return True if SpectralWindow has changed since last iteration
  Bool newSpectralWindow() const
  { return ToBool(curStartRow_p==0 && msIter_p.newSpectralWindow()); }

  // Return the index of the first channel of the current channel group 
  // in the total (selected) spectrum.
  Int channelIndex() const;

  // Return the width of the current group of channels, i.e.,
  // the number of channels returned by visibility() and frequency().
  Int channelGroupSize() const;
  
  // Return the number of rows in the current iteration
  Int nRow() const;

  // Velocity selection - specify the output channels in velocity:
  // nChan - number of output channels, vStart - start velocity,
  // vInc - velocity increment. So channel i will have velocity 
  // vStart + i*vInc (i=0,nChan-1).
  // Specify velocities as in e.g., MVRadialVelocity(Quantity(2001.,"km/s")).
  // The reference type and velocity definition are specified separately.
  // Note that no averaging is performed, the visibilities will be interpolated
  // and sampled at the specified velocities, it's up to you to choose a vInc
  // appropriate to the channel width.
  // The REST_FREQUENCY column in the SPECTRAL_WINDOW subtable is used to
  // determine the velocity-frequency conversion.
  // By default calculations are done for a single velocity with offsets 
  // applied for the others (ok for non-rel velocities with RADIO defn), 
  // set precise to True to do a full conversion for each output channel.(NYI)
  ROVisibilityIterator& 
  selectVelocity(Int nChan, 
		 const MVRadialVelocity& vStart, const MVRadialVelocity& vInc,
		 MRadialVelocity::Types rvType = MRadialVelocity::LSR,
		 MDoppler::Types dType = MDoppler::RADIO, Bool precise=False);

  // Select the velocity interpolation scheme.
  // At present the choice is limited to : nearest and linear, linear
  // is the default. 
  // TODO: add cubic, spline and possibly FFT
  ROVisibilityIterator& velInterpolation(const String& type);

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
  // get the TOPO frequencies from the selected velocities and the obs. vel.
  void getTopoFreqs();
  // update the DATA slicer
  void updateSlicer();
  // attach the column objects to the currently selected table
  virtual void attachColumns();
  // get the (velocity selected) interpolated visibilities, flags and weights
  void getInterpolatedVisFlagWeight(DataColumn whichOne) const;

  ROVisibilityIterator* This;
  MSIter msIter_p;
  Table selTable_p; // currently selected set of rows from curTable
  Int curChanGroup_p, curNumChanGroup_p, channelGroupSize_p, 
      curNumRow_p, curTableNumRow_p, curStartRow_p, curEndRow_p,
      nChan_p, nPol_p, nRowBlocking_p;
  Bool more_p, newChanGroup_p, initialized_p, msIterAtOrigin_p, stateOk_p;

  // channel selection
  Block<Int> numChanGroup_p, chanStart_p, chanWidth_p, chanInc_p,
    preselectedChanStart_p,preselectednChan_p;
  
  // Stack of VisBuffer objects
  Stack<void*> vbStack_p;

  //cache for access functions
  Slicer slicer_p;
  Slicer weightSlicer_p;
  Bool useSlicer_p;
  Vector<Double> time_p;
  Vector<Double> frequency_p;
  Bool freqCacheOK_p, flagOK_p, weightSpOK_p;
  Block<Bool> visOK_p;
  Cube<Bool> flagCube_p;
  Cube<Complex> visCube_p;
  Matrix<Double> uvwMat_p;
  Matrix<Float> imagingWeight_p;
  Vector<Float> pa_p;

  // for PA calculations
  MSDerivedValues msd_p;
  Double lastUT_p;
  Int nAnt_p;

  // for velocity selection and conversion
  Bool velSelection_p, vPrecise_p;
  Int nVelChan_p;
  MVRadialVelocity vStart_p;
  MVRadialVelocity vInc_p;
  MDoppler::Convert cFromBETA_p;
  MDoppler::Types vDef_p;
  Vector<Double> selFreq_p;
  Vector<Double> lsrFreq_p;
  String vInterpolation_p;


  // column access functions
  ROScalarColumn<Int> colAntenna1, colAntenna2;
  ROScalarColumn<Double> colTime;
  ROScalarColumn<Float> colWeight;
  ROArrayColumn<Float> colImagingWeight;
  ROArrayColumn<Complex> colVis;
  ROArrayColumn<Complex> colModelVis;
  ROArrayColumn<Complex> colCorrVis;
  PtrBlock<ROArrayColumn<Complex>* > colVisPtr;
  ROArrayColumn<Float> colSigma;
  ROArrayColumn<Bool> colFlag;
  ROScalarColumn<Bool> colFlagRow;
  ROArrayColumn<Double> colUVW;

};

inline Bool ROVisibilityIterator::more() const { return more_p;}
inline Vector<SquareMatrix<Complex,2> >& 
ROVisibilityIterator::CJones(Vector<SquareMatrix<Complex,2> >& cjones) const 
{cjones.resize(msIter_p.CJones().nelements());return cjones=msIter_p.CJones();}
inline Int ROVisibilityIterator::channelGroupSize() const
{ return velSelection_p ? nVelChan_p : chanWidth_p[msIter_p.spectralWindowId()]; }
inline Int ROVisibilityIterator::channelIndex() const
{ return chanInc_p[msIter_p.spectralWindowId()]*curChanGroup_p; }
inline Int ROVisibilityIterator::nRow() const
{ return curNumRow_p;}
inline ROVisibilityIterator& 
ROVisibilityIterator::velInterpolation(const String& type)
{ vInterpolation_p=type; return *this;}

// <summary>
// VisibilityIterator iterates through one or more writable MeasurementSets
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="ROVisibilityIterator">ROVisibilityIterator</linkto>
// </prerequisite>
//
// <etymology>
// The VisibilityIterator is a read/write iterator returning visibilities
// </etymology>
//
// <synopsis>
// VisibilityIterator provides iteration with various sort orders
// for one or more MSs. It has member functions to retrieve the fields
// commonly needed in synthesis calibration and imaging. It is 
// derived from the read-only iterator
// <linkto class="ROVisibilityIterator">ROVisibilityIterator</linkto>.
//
// One should use <linkto class="VisBuffer">VisBuffer</linkto>
// to access chunks of data.
// </synopsis>
//
// <example>
// <code>
// //
// </code>
// </example>
//
// <motivation>
// For imaging and calibration you need to access an MS in some consistent
// order (by field, spectralwindow, time interval etc.). This class provides
// that access.
// </motivation>
//
// #<thrown>
//
// #</thrown>
//
// <todo asof="1997/05/30">
//   <li> cleanup the currently dual interface for visibilities and flags
//   <li> sort out what to do with weights when interpolating
// </todo>

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

  // Set/modify the flags in the data.
  // This will flag all channels in the original data that contributed to
  // the output channel in the case of channel averaging.
  // All polarizations have the same flag value.
  void setFlag(const Matrix<Bool>& flag);

  // Set/modify the flags in the data.
  // This sets the flags as found in the MS, Cube(npol,nchan,nrow),
  // where nrow is the number of rows in the current iteration (given by
  // nRow()).
  void setFlag(const Cube<Bool>& flag);

  // Set/modify the visibilities.
  // This is possibly only for a 'reference' MS which has a new DATA column.
  // The first axis of the matrix should equal the selected number of channels
  // in the original MS.
  // If the MS does not contain all polarizations, only the parallel
  // hand polarizations are used.
  void setVis(const Matrix<CStokesVector>& vis, DataColumn whichOne);

  // Set/modify the visibilities
  // This sets the data as found in the MS, Cube(npol,nchan,nrow).
  void setVis(const Cube<Complex>& vis, DataColumn whichOne);

  // Set the visibility and flags, and interpolate from velocities if needed
  void setVisAndFlag(const Cube<Complex>& vis, const Cube<Bool>& flag,
		     DataColumn whichOne);

  // Set/modify the weights
  void setWeight(const Vector<Float>& wt);

  // Set/modify the imaging weights
  void setImagingWeight(const Matrix<Float>& wt);

protected:
  virtual void attachColumns();
  void setInterpolatedVisFlag(const Cube<Complex>& vis, 
			      const Cube<Bool>& flag);
  void setInterpolatedWeight(const Matrix<Float>& wt); 

  // column access functions
  ArrayColumn<Complex> RWcolVis;
  ArrayColumn<Complex> RWcolModelVis;
  ArrayColumn<Complex> RWcolCorrVis;
  PtrBlock<ArrayColumn<Complex>* > RWcolVisPtr;
  ScalarColumn<Float> RWcolWeight;
  ArrayColumn<Float> RWcolImagingWeight;
  ArrayColumn<Bool> RWcolFlag;

};

#endif

