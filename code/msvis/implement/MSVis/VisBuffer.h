//# VisBuffer.h: buffer for iterating through MS in large blocks
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

#if !defined(TRIAL_VISBUFFER_H)
#define TRIAL_VISBUFFER_H

#include <aips/aips.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Measures/MDirection.h>
#include <trial/MeasurementEquations/StokesVector.h>
#include <trial/MeasurementEquations/VisibilityIterator.h>

//#forward

//<summary>
// VisBuffers encapulsate one chunk of visibility data for processing.
//</summary>
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
// See <linkto class="MeasurementEquations">MeasurementEquations</linkto>
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

  Vector<SquareMatrix<Complex,2> >& CJones() 
  { return cjonesOK_p ? cjones_p : fillCjones();}
  const Vector<SquareMatrix<Complex,2> >& CJones() const 
  {return This->CJones();} 

  // Note that feed_pa is a function instead of a cached value
  const Vector<Float>& feed_pa(Double time) const; 

  Int fieldId() const {return fieldIdOK_p ? fieldId_p : This->fillFieldId();}

  Matrix<Bool>& flag() { return flagOK_p ? flag_p : fillFlag();}
  const Matrix<Bool>& flag() const { return This->flag();}

  Cube<Bool>& flagCube() { return flagCubeOK_p ? flagCube_p : fillFlagCube();}
  const Cube<Bool>& flagCube() const { return This->flagCube();}

  Vector<Bool>& flagRow() {return flagRowOK_p ? flagRow_p : fillFlagRow();}
  const Vector<Bool>& flagRow() const { return This->flagRow();}

  Vector<Double>& frequency() {return freqOK_p ? frequency_p : fillFreq();}
  const Vector<Double>& frequency() const {return This->frequency();}

  Vector<Double>& lsrFrequency() 
  {return lsrFreqOK_p ? lsrFrequency_p : fillLSRFreq();}
  const Vector<Double>& lsrFrequency() const {return This->lsrFrequency();}

  MDirection& phaseCenter() 
  { return phaseCenterOK_p ? phaseCenter_p : fillPhaseCenter();}
  const MDirection& phaseCenter() const {return This->phaseCenter();}

  Int polFrame() const {return polFrameOK_p ? polFrame_p : This->fillPolFrame();}

  Vector<Int>& corrType() { return corrTypeOK_p ? corrType_p : fillCorrType();}
  const Vector<Int>& corrType() const { return This->corrType();}

  Vector<Float>& sigma() {return sigmaOK_p ? sigma_p : fillSigma();}
  const Vector<Float>& sigma() const {return This->sigma();}

  Int spectralWindow() const {return spwOK_p ? spectralWindow_p : This->fillSpW();}

  Vector<Double>& time() {return timeOK_p ? time_p : fillTime();}
  const Vector<Double>& time() const {return This->time();}

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

  Cube<Complex>& modelVisCube() 
  { 
    return modelVisCubeOK_p ? modelVisCube_p : 
      fillVisCube(VisibilityIterator::Model);
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

  Matrix<Float>& imagingWeight() 
  {return weightMatOK_p ? weightMat_p : fillImagingWeight();}
  const Matrix<Float>& imagingWeight() const {return This->imagingWeight();}
  //</group>

  // Frequency average the buffer
  void freqAverage();

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

private:

  // validate the cache
  void validate();

  // functions to fill cache from iterator
  Int & fillnChannel();
  Vector<Int>& fillChannel();
  Int & fillnRow();
  Vector<Int>& fillAnt1();
  Vector<Int>& fillAnt2();
  Vector<SquareMatrix<Complex,2> >& fillCjones();
  Int& fillFieldId();
  Matrix<Bool>& fillFlag();
  Cube<Bool>& fillFlagCube();
  Vector<Bool> & fillFlagRow();
  Vector<Double>& fillFreq();
  Vector<Double>& fillLSRFreq();
  MDirection& fillPhaseCenter();
  Int& fillPolFrame();
  Vector<Int>& fillCorrType();
  Vector<Float>& fillSigma();
  Int& fillSpW();
  Vector<Double>& fillTime();
  Vector<RigidVector<Double,3> >& filluvw();
  Matrix<CStokesVector>& fillVis(VisibilityIterator::DataColumn whichOne);
  Cube<Complex>& fillVisCube(VisibilityIterator::DataColumn whichOne);
  Vector<Float>& fillWeight();
  Matrix<Float>& fillImagingWeight();

  ROVisibilityIterator* visIter_p;
  Bool twoWayConnection_p;

  VisBuffer* This;
  // variables to track validity of cache
  Bool nChannelOK_p, channelOK_p, nRowOK_p, ant1OK_p, ant2OK_p, cjonesOK_p,
    fieldIdOK_p, flagOK_p, flagRowOK_p, freqOK_p, lsrFreqOK_p,
    phaseCenterOK_p, polFrameOK_p,
    sigmaOK_p, spwOK_p, timeOK_p, uvwOK_p, visOK_p, weightOK_p;
  Bool corrTypeOK_p, flagCubeOK_p, visCubeOK_p, weightMatOK_p,
    modelVisOK_p, correctedVisOK_p, modelVisCubeOK_p, correctedVisCubeOK_p;

  // cached variables
  Int nChannel_p, nRow_p;
  Vector<Int> channel_p, antenna1_p, antenna2_p;
  Vector<SquareMatrix<Complex,2> > cjones_p;
  Int fieldId_p;
  Matrix<Bool> flag_p;
  Vector<Bool> flagRow_p;
  Vector<Double> frequency_p, lsrFrequency_p;
  MDirection phaseCenter_p;
  Int polFrame_p;
  Vector<Int> corrType_p;
  Vector<Float> sigma_p;
  Int spectralWindow_p;
  Vector<Double> time_p;
  Vector<RigidVector<Double,3> > uvw_p;
  Matrix<CStokesVector> visibility_p, modelVisibility_p, correctedVisibility_p;
  Vector<Float> weight_p;
  Cube<Bool> flagCube_p;
  Cube<Complex> visCube_p, modelVisCube_p, correctedVisCube_p;
  Matrix<Float> weightMat_p;

};

#endif

