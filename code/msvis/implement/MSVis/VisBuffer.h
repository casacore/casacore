//# VisBuffer.h: buffer for iterating through MS in large blocks
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

#if !defined(TRIAL_VISBUFFER_H)
#define TRIAL_VISBUFFER_H

#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Measures/MDirection.h>
#include <trial/MeasurementEquations/StokesVector.h>

//#forward
class ROVisibilityIterator;

//<synopsis>
// This class contains 'one iteration' of the VisIter. It is a modifiable
// buffer of values to which calibration and averaging can be applied.
// This allows processing of the data in larger blocks, avoiding some
// overheads for processing per visibility point or spectrum.
//</synopsis>

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
  // this and other are or'ed. An exception is thrown if the number of
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
  Vector<Bool>& flagRow() {return flagRowOK_p ? flagRow_p : fillFlagRow();}
  const Vector<Bool>& flagRow() const { return This->flagRow();}
  Vector<Double>& frequency() {return freqOK_p ? frequency_p : fillFreq();}
  const Vector<Double>& frequency() const {return This->frequency();}
  MDirection& phaseCenter() 
  { return phaseCenterOK_p ? phaseCenter_p : fillPhaseCenter();}
  const MDirection& phaseCenter() const {return This->phaseCenter_p;}
  Int polFrame() const {return polFrameOK_p ? polFrame_p : This->fillPolFrame();}
  //#Vector<Int>& polTypes() { return polTypesOK_p ? polTypes_p : fillPolTypes();}
  Vector<Float>& sigma() {return sigmaOK_p ? sigma_p : fillSigma();}
  const Vector<Float>& sigma() const {return This->sigma();}
  Int spectralWindow() const {return spwOK_p ? spectralWindow_p : This->fillSpW();}
  Vector<Double>& time() {return timeOK_p ? time_p : fillTime();}
  const Vector<Double>& time() const {return This->time();}
  Vector<RigidVector<Double,3> >& uvw() {return uvwOK_p ? uvw_p : filluvw();}
  const Vector<RigidVector<Double,3> >& uvw() const {return This->uvw();}
  Matrix<CStokesVector>& visibility() 
  { return visOK_p ? visibility_p : fillVis();}
  const Matrix<CStokesVector>& visibility() const {return This->visibility();}
  Vector<Float>& weight() {return weightOK_p ? weight_p : fillWeight();}
  const Vector<Float>& weight() const {return This->weight();}
  //</group>

  // Frequency average the buffer
  void freqAverage();

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
  Vector<Bool> & fillFlagRow();
  Vector<Double>& fillFreq();
  MDirection& fillPhaseCenter();
  Int& fillPolFrame();
  //Vector<Int>& fillPolTypes();
  Vector<Float>& fillSigma();
  Int& fillSpW();
  Vector<Double>& fillTime();
  Vector<RigidVector<Double,3> >& filluvw();
  Matrix<CStokesVector>& fillVis();
  Vector<Float>& fillWeight();

  ROVisibilityIterator* visIter_p;
  Bool twoWayConnection_p;

  VisBuffer* This;
  // variables to track validity of cache
  Bool nChannelOK_p, channelOK_p, nRowOK_p, ant1OK_p, ant2OK_p, cjonesOK_p,
    fieldIdOK_p, flagOK_p, flagRowOK_p, freqOK_p, phaseCenterOK_p, polFrameOK_p,
    sigmaOK_p, spwOK_p, timeOK_p, uvwOK_p, visOK_p, weightOK_p;

  // cached variables
  Int nChannel_p, nRow_p;
  Vector<Int> channel_p, antenna1_p, antenna2_p;
  Vector<SquareMatrix<Complex,2> > cjones_p;
  Int fieldId_p;
  Matrix<Bool> flag_p;
  Vector<Bool> flagRow_p;
  Vector<Double> frequency_p;
  MDirection phaseCenter_p;
  Int polFrame_p;
  Vector<Int> polTypes_p;
  Vector<Float> sigma_p;
  Int spectralWindow_p;
  Vector<Double> time_p;
  Vector<RigidVector<Double,3> > uvw_p;
  Matrix<CStokesVector> visibility_p;
  Vector<Float> weight_p;
};

#endif

