//# VisBuffer.h: buffer for iterating through MS in large blocks
//# Copyright (C) 1996,1997,1998,1999,2000,2002,2003
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

#ifndef MSVIS_VISBUFFER_H
#define MSVIS_VISBUFFER_H

#include <casa/aips.h>
#include <casa/Arrays/Cube.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/BasicSL/Complex.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MEpoch.h>
#include <msvis/MSVis/StokesVector.h>
#include <msvis/MSVis/VisibilityIterator.h>
#include <msvis/MSVis/MSCalEnums.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//#forward

//<summary>VisBuffers encapulsate one chunk of visibility data for processing.</summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
//   <li> <linkto class="VisSet">VisSet</linkto>
//   <li> <linkto class="VisibilityIterator">VisibilityIterator</linkto>
// </prerequisite>
//
// <etymology>
// VisBuffer is a buffer for visibility data
// </etymology>
//
//<synopsis>
// This class contains 'one iteration' of the 
// <linkto class="VisibilityIterator">VisibilityIterator</linkto>
// It is a modifiable
// buffer of values to which calibration and averaging can be applied.
// This allows processing of the data in larger blocks, avoiding some
// overheads for processing per visibility point or spectrum.
//
// See <linkto module="MeasurementEquations">MeasurementEquations</linkto>
// for more details on how the VisBuffer is to be used.
//</synopsis>

//<todo>
// <li> reconcile vis/visCube usage: visCube, flagCube and weightMatrix
// are currently only correct when this VisBuffer got them from a
// VisIter, operations like -=, freqAverage() are only done for
// visibility() and flag().
//</todo>
class VisBuffer
{
public:
  // Create empty VisBuffer you can assign to or attach.
  VisBuffer();
  // Construct VisBuffer for a particular VisibilityIterator
  // The buffer will remain synchronized with the iterator.
  VisBuffer(ROVisibilityIterator & iter);

  // Copy construct, looses synchronization with iterator: only use buffer for
  // current iteration (or reattach).
  VisBuffer(const VisBuffer& vb);

  // Destructor (detaches from VisIter)
  ~VisBuffer(); 

  // Assignment, looses synchronization with iterator: only use buffer for 
  // current iteration (or reattach)
  VisBuffer& operator=(const VisBuffer& vb);

  // Assignment, optionally without copying the data across; with copy=True
  // this is identical to normal assignment operator
  VisBuffer& assign(const VisBuffer& vb, Bool copy=True);

  // subtraction: return the difference of the visibilities, flags of
  // this and other are or-ed. An exception is thrown if the number of
  // rows or channels differs, but no further checks are done.
  VisBuffer& operator-=(const VisBuffer& vb);

  // Attach to a VisIter. Detaches itself first if already attached 
  // to a VisIter. Will remain synchronized with iterator.
  void attachToVisIter(ROVisibilityIterator & iter);

  // Invalidate the cache
  void invalidate();

  // <group>
  // Access functions
  //
  Int& nChannel() { return nChannelOK_p ? nChannel_p : fillnChannel();}
  Int nChannel() const { return This->nChannel();}

  Vector<Int>& channel() { return channelOK_p ? channel_p : fillChannel();}
  const Vector<Int>& channel() const { return This->channel();}

  Int& nRow() { return nRowOK_p ? nRow_p : fillnRow();}
  Int nRow() const { return This->nRow();}

  Vector<Int>& antenna1() {return ant1OK_p ? antenna1_p : fillAnt1();}
  const Vector<Int>& antenna1() const {return This->antenna1();}

  Vector<Int>& antenna2() {return ant2OK_p ? antenna2_p : fillAnt2();}
  const Vector<Int>& antenna2() const {return This->antenna2();}

  Vector<Int>& feed1() {return feed1OK_p ? feed1_p : fillFeed1();}
  const Vector<Int>& feed1() const {return This->feed1();}

  Vector<Int>& feed2() {return feed2OK_p ? feed2_p : fillFeed2();}
  const Vector<Int>& feed2() const {return This->feed2();}

  // feed1_pa() and feed2_pa() return an array of parallactic angles
  // (each corresponds to the first receptor of the feed) one for each
  // row in the current buffer. In contrast, feed_pa() calculates
  // the angles for each antenna. These methods are implemented for
  // VisBuffer only to benefit from caching of the feed and antenna IDs.
  Vector<Float>& feed1_pa() {return feed1_paOK_p ? feed1_pa_p : fillFeed1_pa();}
  const Vector<Float>& feed1_pa() const {return This->feed1_pa();}

  Vector<Float>& feed2_pa() {return feed2_paOK_p ? feed2_pa_p : fillFeed2_pa();}
  const Vector<Float>& feed2_pa() const {return This->feed2_pa();}
  
  Vector<SquareMatrix<Complex,2> >& CJones()
  { return cjonesOK_p ? cjones_p : fillCjones();}
  const Vector<SquareMatrix<Complex,2> >& CJones() const 
  {return This->CJones();} 

  // Note that feed_pa is a function instead of a cached value
  const Vector<Float>& feed_pa(Double time) const; 

  // direction1() and direction2() return arrays of directions where
  // the first and the second antenna/feed are pointed to. One value for
  // each row in the current buffer.
  Vector<MDirection>& direction1() {return direction1OK_p ? direction1_p :
	    fillDirection1();}
  const Vector<MDirection>& direction1()  const {return This->direction1();}

  Vector<MDirection>& direction2() {return direction2OK_p ? direction2_p :
	    fillDirection2();}
  const Vector<MDirection>& direction2()  const {return This->direction2();}

  // Note that azel is a function instead of a cached value
  const Vector<MDirection>& azel(Double time) const; 

  Int fieldId() const {return fieldIdOK_p ? fieldId_p : This->fillFieldId();}

  Int arrayId() const {return arrayIdOK_p ? arrayId_p : This->fillArrayId();}

  Matrix<Bool>& flag() { return flagOK_p ? flag_p : fillFlag();}
  const Matrix<Bool>& flag() const { return This->flag();}

  Cube<Bool>& flagCube() { return flagCubeOK_p ? flagCube_p : fillFlagCube();}
  const Cube<Bool>& flagCube() const { return This->flagCube();}

  Vector<Bool>& flagRow() {return flagRowOK_p ? flagRow_p : fillFlagRow();}
  const Vector<Bool>& flagRow() const { return This->flagRow();}

  Vector<Int>& scan() {return scanOK_p ? scan_p : fillScan();}
  const Vector<Int>& scan() const { return This->scan();}

  // scalar version for convenience, when scan known constant for
  // entire iteration/buffer.
  Int scan0() { return scan()(0);  }

  Vector<Double>& frequency() {return freqOK_p ? frequency_p : fillFreq();}
  const Vector<Double>& frequency() const {return This->frequency();}

  Vector<Double>& lsrFrequency() 
  {return lsrFreqOK_p ? lsrFrequency_p : fillLSRFreq();}
  const Vector<Double>& lsrFrequency() const {return This->lsrFrequency();}


  //the following method is to convert the observed frequencies
  // This conversion may not be accurate for some frame 
  // conversion like topo to lsr except if the spw is in the actual buffer

  void lsrFrequency(const Int& spw, Vector<Double>& freq, Bool convert=False)
    { visIter_p->lsrFrequency(spw, freq, convert);}

  void lsrFrequency(const Int& spw, Vector<Double>& freq, 
		    Bool convert=False) const
    { visIter_p->lsrFrequency(spw, freq, convert);}
  MDirection& phaseCenter() 
  { return phaseCenterOK_p ? phaseCenter_p : fillPhaseCenter();}
  const MDirection& phaseCenter() const {return This->phaseCenter();}

  Int polFrame() const {return polFrameOK_p ? polFrame_p : This->fillPolFrame();}

  Vector<Int>& corrType() { return corrTypeOK_p ? corrType_p : fillCorrType();}
  const Vector<Int>& corrType() const { return This->corrType();}

  Vector<Float>& sigma() {return sigmaOK_p ? sigma_p : fillSigma();}
  const Vector<Float>& sigma() const {return This->sigma();}

  Matrix<Float>& sigmaMat() {return sigmaMatOK_p ? sigmaMat_p : fillSigmaMat();}
  const Matrix<Float>& sigmaMat() const {return This->sigmaMat();}

  Int& spectralWindow() {return spwOK_p ? spectralWindow_p : This->fillSpW();}
  Int spectralWindow() const {return spwOK_p ? spectralWindow_p : This->fillSpW();}
  Int dataDescriptionId() const {return visIter_p->dataDescriptionId();}
  Vector<Double>& time() {return timeOK_p ? time_p : fillTime();}
  const Vector<Double>& time() const {return This->time();}

  Vector<Double>& timeInterval() {
    return timeIntervalOK_p ? timeInterval_p : fillTimeInterval();}
  const Vector<Double>& timeInterval() const {return This->timeInterval();}

  Vector<RigidVector<Double,3> >& uvw() {return uvwOK_p ? uvw_p : filluvw();}
  const Vector<RigidVector<Double,3> >& uvw() const {return This->uvw();}

  Matrix<CStokesVector>& visibility() 
  { return visOK_p ? visibility_p : fillVis(VisibilityIterator::Observed);}
  const Matrix<CStokesVector>& visibility() const {return This->visibility();}

  Matrix<CStokesVector>& modelVisibility() 
  { 
    return modelVisOK_p ? modelVisibility_p : 
      fillVis(VisibilityIterator::Model);
  }
  const Matrix<CStokesVector>& modelVisibility() const {return This->modelVisibility();}

  Matrix<CStokesVector>& correctedVisibility() 
  { 
    return correctedVisOK_p ? correctedVisibility_p : 
      fillVis(VisibilityIterator::Corrected);
  }
  const Matrix<CStokesVector>& correctedVisibility() const {return This->correctedVisibility();}

  Cube<Complex>& visCube() 
  { return visCubeOK_p ? visCube_p : fillVisCube(VisibilityIterator::Observed);}
  const Cube<Complex>& visCube() const {return This->visCube();}

  Cube<Complex>& modelVisCube() {
    return modelVisCubeOK_p ? modelVisCube_p : 
      fillVisCube(VisibilityIterator::Model);
  }

  Cube<Complex>& modelVisCube(const Bool& matchVisCubeShape) {
    // Avoids call to fillVisCube(VisIter::Model)
    modelVisCubeOK_p=True;
    if (matchVisCubeShape) {
      // shape it in memory like data
      modelVisCube_p.resize(visCube().shape());
      modelVisCube_p=Complex(0.0);
    }
    // ... and return it in the usual way
    return modelVisCube();
  }

  const Cube<Complex>& modelVisCube() const {return This->modelVisCube();}

  Cube<Complex>& correctedVisCube() 
  { 
    return correctedVisCubeOK_p ? correctedVisCube_p : 
      fillVisCube(VisibilityIterator::Corrected);
  }
  const Cube<Complex>& correctedVisCube() const {return This->correctedVisCube();}


  Vector<Float>& weight() {return weightOK_p ? weight_p : fillWeight();}
  const Vector<Float>& weight() const {return This->weight();}

  Matrix<Float>& weightMat() {return weightMatOK_p ? weightMat_p : fillWeightMat();}
  const Matrix<Float>& weightMat() const {return This->weightMat();}

  Cube<Float>& weightSpectrum()
    {return weightSpectrumOK_p ? weightSpectrum_p : fillWeightSpectrum();}
  const Cube<Float>& weightSpectrum() const {return This->weightSpectrum();}

  Matrix<Float>& imagingWeight() 
  {return imagingWeightOK_p ? imagingWeight_p : fillImagingWeight();}
  const Matrix<Float>& imagingWeight() const {return This->imagingWeight();}
 
  Cube<Float>& weightCube() {return weightCube_p;}
  //</group>

  //<group>
  // Utility functions to provide coordinate or column ranges of the
  // data in the VisBuffer. Flagging is applied before computing the ranges.
  //
  // Generic accessor to column ranges of integer type, as specified by
  // enumerations defined in class MSCalEnums. Throws an exception
  // if the enum is not for a recognized integer column.
  Vector<Int> vecIntRange(const MSCalEnums::colDef& calEnum) const;

  // Antenna id. range (includes both ANTENNA1 and ANTENNA2 columns)
  Vector<Int> antIdRange() const;

  // Time range 
  Bool timeRange(MEpoch& rTime, MVEpoch& rTimeEP, MVEpoch& rInterval) const;


  // Return the row Ids from the original ms. If the ms used is a subset of 
  // another ms then rowIds() return the row ids of the original ms.
  
  Vector<uInt>& rowIds();

  const Vector<uInt>& rowIds()const {return This->rowIds();};


  //</group>

  // Frequency average the buffer (visibility() column only)
  void freqAverage();

  // Frequency average the buffer (visCube and [if present] modelVisCube)
  void freqAveCubes();

  // Sort/unsort the correlations, if necessary
  //  (Rudimentary handling of non-canonically sorted correlations--use with care!)
  void sortCorr();
  void unSortCorr();
    
  // Normalize the visCube by the modelVisCube 
  //   (and optionally also divide visCube_p by its normalized amp)
  void normalize(const Bool& phaseOnly=False);

  // Fill weightMat according to sigma column
  void resetWeightMat();

  // Update coordinate info - useful for copied VisBuffers that need
  // to retain some state for later reference.
  // Presently this fills antenna, array, field and spectralWindow ids, time,
  // frequency and number of rows. Add more as needed.
  void updateCoordInfo();

  // Set the visibility to a constant, note that this only changes the buffer,
  // no values are written back to tables from here.
  void setVisCube(Complex c);
  void setModelVisCube(Complex c);
  void setCorrectedVisCube(Complex c);

  // Set the visibility, note that this only changes the buffer,
  // no values are written back to tables from here.
  void setVisCube(const Cube<Complex>& vis);
  void setModelVisCube(const Cube<Complex>& vis);
  void setCorrectedVisCube(const Cube<Complex>& vis);

  // Reference external model visibilities
  void refModelVis(const Matrix<CStokesVector>& mvis);

  // Remove scratch cols data from vb
  void removeScratchCols(); 

  // Access the current ROMSColumns object via VisIter
  const ROMSColumns& msColumns() const
    { return visIter_p->msColumns();}

  // Get all selected spectral windows not just the one in the actual buffer
  void allSelectedSpectralWindows(Vector<Int>& spws, Vector<Int>& nvischan){
    visIter_p->allSelectedSpectralWindows(spws, nvischan);
  }

  void allSelectedSpectralWindows(Vector<Int>& spws, Vector<Int>& nvischan) const { This->allSelectedSpectralWindows(spws, nvischan);}

  // Return the actual msid, useful if using multiple ms to monitor which 
  // ms in the  list is being dealt with
  Int msId() const
    { This->checkMSId(); return oldMSId_p; };

  //checked if the ms has changed since the last chunk processed
  Bool newMS() const 
    { This->checkMSId(); return newMS_p;};

  //

private:

  // validate the cache
  void validate();

  // functions to fill cache from iterator
  Int & fillnChannel();
  Vector<Int>& fillChannel();
  Int & fillnRow();
  Vector<Int>& fillAnt1();
  Vector<Int>& fillAnt2();
  Vector<Int>& fillFeed1();
  Vector<Int>& fillFeed2();
  // calling fillFeed1_pa or fillFeed2_pa will fill antenna, feed
  // and time caches automatically
  Vector<Float>& fillFeed1_pa();
  Vector<Float>& fillFeed2_pa();

  // calling direction1 or direction2 will fill antenna,feed, time and pa 
  // caches automatically
  Vector<MDirection>& fillDirection1();
  Vector<MDirection>& fillDirection2();
  
  Vector<SquareMatrix<Complex,2> >& fillCjones();
  Int& fillFieldId();
  Int& fillArrayId();
  Matrix<Bool>& fillFlag();
  Cube<Bool>& fillFlagCube();
  Vector<Bool> & fillFlagRow();
  Vector<Int> & fillScan();
  Vector<Double>& fillFreq();
  Vector<Double>& fillLSRFreq();
  MDirection& fillPhaseCenter();
  Int& fillPolFrame();
  Vector<Int>& fillCorrType();
  Vector<Float>& fillSigma();
  Matrix<Float>& fillSigmaMat();
  Int& fillSpW();
  Vector<Double>& fillTime();
  Vector<Double>& fillTimeInterval();
  Vector<RigidVector<Double,3> >& filluvw();
  Matrix<CStokesVector>& fillVis(VisibilityIterator::DataColumn whichOne);
  Cube<Complex>& fillVisCube(VisibilityIterator::DataColumn whichOne);
  Vector<Float>& fillWeight();
  Matrix<Float>& fillWeightMat();
  Cube<Float>& fillWeightSpectrum();
  Matrix<Float>& fillImagingWeight();
  Bool checkMSId();

  // Filter index arrays for unique elements
  Vector<Int> unique(const Vector<Int>& indices) const;

  ROVisibilityIterator* visIter_p;
  Bool twoWayConnection_p;

  // Are correlations in non-canonical order?
  Bool nonCanonCorr();

  // Have correlations been sorted by sortCorr?
  Bool corrSorted_p;

  VisBuffer* This;
  // variables to track validity of cache
  Bool nChannelOK_p, channelOK_p, nRowOK_p, ant1OK_p, ant2OK_p,
    feed1OK_p, feed2OK_p, cjonesOK_p,
    fieldIdOK_p, arrayIdOK_p, flagOK_p, flagRowOK_p, scanOK_p, freqOK_p,
    lsrFreqOK_p, phaseCenterOK_p, polFrameOK_p, sigmaOK_p, sigmaMatOK_p,spwOK_p,
    timeOK_p, timeIntervalOK_p, uvwOK_p, visOK_p, weightOK_p, weightMatOK_p,
    weightSpectrumOK_p;
  Bool corrTypeOK_p, flagCubeOK_p, visCubeOK_p, imagingWeightOK_p,
    modelVisOK_p, correctedVisOK_p, modelVisCubeOK_p, correctedVisCubeOK_p;
  Bool msOK_p, newMS_p;
  Bool feed1_paOK_p,feed2_paOK_p,direction1OK_p,direction2OK_p;
  Bool rowIdsOK_p;

  // cached variables
  Int nChannel_p, nRow_p;
  Vector<Int> channel_p, antenna1_p, antenna2_p, feed1_p, feed2_p;
  Vector<Float> feed1_pa_p, feed2_pa_p; 
  Vector<SquareMatrix<Complex,2> > cjones_p;
  Vector<MDirection> direction1_p; //where the first antenna/feed is pointed to
  Vector<MDirection> direction2_p; //where the second antenna/feed is pointed to
  Int fieldId_p;
  Int arrayId_p;
  Matrix<Bool> flag_p;
  Vector<Bool> flagRow_p;
  Vector<Int> scan_p;
  Vector<Double> frequency_p, lsrFrequency_p;
  MDirection phaseCenter_p;
  Int polFrame_p;
  Vector<Int> corrType_p;
  Vector<Float> sigma_p;
  Matrix<Float> sigmaMat_p;
  Int spectralWindow_p;
  Vector<Double> time_p;
  Vector<Double> timeInterval_p;
  Vector<RigidVector<Double,3> > uvw_p;
  Matrix<CStokesVector> visibility_p, modelVisibility_p, correctedVisibility_p;
  Vector<Float> weight_p;
  Matrix<Float> weightMat_p;
  Cube<Float> weightSpectrum_p;
  Cube<Bool> flagCube_p;
  Cube<Complex> visCube_p, modelVisCube_p, correctedVisCube_p;
  Vector<uInt> rowIds_p;
  Matrix<Float> imagingWeight_p;
  Int oldMSId_p;

  Cube<Float> weightCube_p;

};


} //# NAMESPACE CASA - END

#endif

