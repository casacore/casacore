//# VisBuffer.cc: buffer for iterating through MS in large blocks
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

#include <trial/MeasurementEquations/VisibilityIterator.h>
#include <trial/MeasurementEquations/VisBuffer.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Utilities/Assert.h>

VisBuffer::VisBuffer():visIter_p((ROVisibilityIterator*)0),
twoWayConnection_p(False),This(this),nChannel_p(0),nRow_p(0)
{validate();}

VisBuffer::VisBuffer(ROVisibilityIterator& iter):visIter_p(&iter),This(this)
{ 
  iter.attachVisBuffer(*this); 
  twoWayConnection_p=True;
}

VisBuffer::VisBuffer(const VisBuffer& vb):visIter_p((ROVisibilityIterator*)0),
This(this)
{
  operator=(vb);
}

VisBuffer& VisBuffer::operator=(const VisBuffer& other)
{
  if (this!=&other) {
    if (visIter_p!=(ROVisibilityIterator*)0 && twoWayConnection_p) 
      visIter_p->detachVisBuffer(*this);
    visIter_p=other.visIter_p;
    twoWayConnection_p=False;
    if (visIter_p == (ROVisibilityIterator*)0) {
      validate();
    } else {
      nChannelOK_p=other.nChannelOK_p;
      nRowOK_p=other.nRowOK_p;
      channelOK_p=other.channelOK_p;
      ant1OK_p=other.ant1OK_p;
      ant2OK_p=other.ant2OK_p;
      corrTypeOK_p=other.corrTypeOK_p;
      cjonesOK_p=other.cjonesOK_p;
      fieldIdOK_p=other.fieldIdOK_p;
      flagOK_p=other.flagOK_p;
      flagCubeOK_p=other.flagCubeOK_p;
      flagRowOK_p=other.flagRowOK_p;
      freqOK_p=other.freqOK_p;
      lsrFreqOK_p=other.lsrFreqOK_p;
      phaseCenterOK_p=other.phaseCenterOK_p;
      polFrameOK_p=other.polFrameOK_p;
      sigmaOK_p=other.sigmaOK_p;
      spwOK_p=other.spwOK_p;
      timeOK_p=other.timeOK_p;
      uvwOK_p=other.uvwOK_p;
      visOK_p=other.visOK_p;
      modelVisOK_p=other.modelVisOK_p;
      correctedVisOK_p=other.correctedVisOK_p;
      visCubeOK_p=other.visCubeOK_p;
      modelVisCubeOK_p=other.modelVisCubeOK_p;
      correctedVisCubeOK_p=other.correctedVisCubeOK_p;
      weightOK_p=other.weightOK_p;
      weightMatOK_p=other.weightMatOK_p;
    }
    if (nChannelOK_p) nChannel_p=other.nChannel_p;
    if (nRowOK_p) nRow_p=other.nRow_p;
    if (channelOK_p) {
      channel_p.resize(other.channel_p.nelements()); 
      channel_p=other.channel_p;
    }
    if (ant1OK_p) {
      antenna1_p.resize(other.antenna1_p.nelements()); 
      antenna1_p=other.antenna1_p;
    }
    if (ant2OK_p) {
      antenna2_p.resize(other.antenna2_p.nelements()); 
      antenna2_p=other.antenna2_p;
    }
    if (corrTypeOK_p) {
      corrType_p.resize(other.corrType_p.nelements()); 
      corrType_p=other.corrType_p;
    }
    if (cjonesOK_p) {
      cjones_p.resize(other.cjones_p.nelements()); 
      cjones_p=other.cjones_p;
    }
    if (fieldIdOK_p) fieldId_p=other.fieldId_p;
    if (flagOK_p) {
      flag_p.resize(other.flag_p.shape()); 
      flag_p=other.flag_p;
    }
    if (flagCubeOK_p) {
      flagCube_p.resize(other.flagCube_p.shape()); 
      flagCube_p=other.flagCube_p;
    }
    if (flagRowOK_p) {
      flagRow_p.resize(other.flagRow_p.nelements());
      flagRow_p=other.flagRow_p;
    }
    if (freqOK_p) {
      frequency_p.resize(other.frequency_p.nelements()); 
      frequency_p=other.frequency_p;
    }
    if (lsrFreqOK_p) {
      lsrFrequency_p.resize(other.lsrFrequency_p.nelements()); 
      lsrFrequency_p=other.lsrFrequency_p;
    }
    if (phaseCenterOK_p) phaseCenter_p=other.phaseCenter_p;
    if (polFrameOK_p) polFrame_p=other.polFrame_p;
    if (sigmaOK_p) {
      sigma_p.resize(other.sigma_p.shape()); 
      sigma_p=other.sigma_p;
    }
    if (spwOK_p) spectralWindow_p=other.spectralWindow_p;
    if (timeOK_p) {
      time_p.resize(other.time_p.nelements()); 
      time_p=other.time_p;
    }
    if (uvwOK_p) {
      uvw_p.resize(other.uvw_p.nelements()); 
      uvw_p=other.uvw_p;
    }
    if (visOK_p) {
      visibility_p.resize(other.visibility_p.shape());
      visibility_p=other.visibility_p;
    }
    if (modelVisOK_p) {
      modelVisibility_p.resize(other.modelVisibility_p.shape());
      modelVisibility_p=other.modelVisibility_p;
    }
    if (correctedVisOK_p) {
      correctedVisibility_p.resize(other.correctedVisibility_p.shape());
      correctedVisibility_p=other.correctedVisibility_p;
    }
    if (visCubeOK_p) {
      visCube_p.resize(other.visCube_p.shape());
      visCube_p=other.visCube_p;
    }
    if (modelVisCubeOK_p) {
      modelVisCube_p.resize(other.modelVisCube_p.shape());
      modelVisCube_p=other.modelVisCube_p;
    }
    if (correctedVisCubeOK_p) {
      correctedVisCube_p.resize(other.correctedVisCube_p.shape());
      correctedVisCube_p=other.correctedVisCube_p;
    }
    if (weightOK_p) {
      weight_p.resize(other.weight_p.nelements()); 
      weight_p=other.weight_p;
    }
    if (weightMatOK_p) {
      weightMat_p.resize(other.weightMat_p.shape()); 
      weightMat_p=other.weightMat_p;
    }
  }
  return *this;
}
  
VisBuffer::~VisBuffer()
{ 
  if (visIter_p!=(ROVisibilityIterator*)0 && twoWayConnection_p) 
    visIter_p->detachVisBuffer(*this);
}

VisBuffer& VisBuffer::operator-=(const VisBuffer& vb)
{
  // check the shapes
  AlwaysAssert(nRow_p==vb.nRow(),AipsError);
  AlwaysAssert(nChannel_p==vb.nChannel(),AipsError);
  // make sure flag and flagRow are current
  flag(); flagRow();

  // do the subtraction, or'ing the flags
  for (Int row=0; row<nRow_p; row++) {
    if (vb.flagRow()(row)) flagRow_p(row)=True;
    if (!flagRow_p(row)) {
      for (Int chn=0; chn<nChannel_p; chn++) {
	if (vb.flag()(chn,row)) flag_p(chn,row)=True;
	if (!flag_p(chn,row)) {
	  visibility_p(chn,row)-=vb.visibility()(chn,row);
	}
      }
    }
  }
  return *this;
}

void VisBuffer::attachToVisIter(ROVisibilityIterator& iter)
{ 
  if (visIter_p!=(ROVisibilityIterator*)0 && twoWayConnection_p) 
    visIter_p->detachVisBuffer(*this);
  visIter_p=&iter; 
  iter.attachVisBuffer(*this);
  twoWayConnection_p=True;
}

void VisBuffer::invalidate()
{
  nChannelOK_p=channelOK_p=nRowOK_p=ant1OK_p=ant2OK_p=cjonesOK_p=
    fieldIdOK_p=flagOK_p=flagRowOK_p=freqOK_p=lsrFreqOK_p=phaseCenterOK_p=polFrameOK_p=
    sigmaOK_p=spwOK_p=timeOK_p=uvwOK_p=visOK_p=weightOK_p=corrTypeOK_p= False;
  flagCubeOK_p=visCubeOK_p=weightMatOK_p=False;
  modelVisOK_p=correctedVisOK_p=modelVisCubeOK_p=correctedVisCubeOK_p=False;
}

void VisBuffer::validate()
{
  nChannelOK_p=channelOK_p=nRowOK_p=ant1OK_p=ant2OK_p=cjonesOK_p=
    fieldIdOK_p=flagOK_p=flagRowOK_p=freqOK_p=lsrFreqOK_p=phaseCenterOK_p=polFrameOK_p=
    sigmaOK_p=spwOK_p=timeOK_p=uvwOK_p=visOK_p=weightOK_p = corrTypeOK_p=True;
  flagCubeOK_p=visCubeOK_p=weightMatOK_p=True;  
  modelVisOK_p=correctedVisOK_p=modelVisCubeOK_p=correctedVisCubeOK_p=True;
}

void VisBuffer::freqAverage() 
{
  Matrix<CStokesVector> newVisibility(1,nRow()); 
  Matrix<Bool> newFlag(1,nRow()); newFlag=True;
  Double newFrequency; newFrequency=0;
  Int nfreq=0;
  Int nChan=nChannel();
  for (Int row=0; row<nRow(); row++) {
    if (!flagRow()(row)) {
      Int n=0;
      for (Int chn=0; chn<nChan; chn++) {
	if (!flag()(chn,row)) {
	  newVisibility(0,row)+=visibility()(chn,row);
	  newFlag(0,row)=False;
	  newFrequency+=frequency()(chn);
	  n++; nfreq++;
	}
      }
      if (n==0) flagRow()(row)=True;
      if (n>0) newVisibility(0,row)*=1.0f/n;
    }
  }
  // Average frequency for this buffer (should really be row based)
  if (nfreq>0) newFrequency/=Double(nfreq);
  nChannel_p=1;
  flag_p.reference(newFlag);
  visibility_p.reference(newVisibility);
  frequency_p.resize(1); frequency_p(0)=newFrequency;
}

void VisBuffer::setVisCube(Complex c)
{
  visCube_p.resize(visIter_p->visibilityShape());
  visCube_p.set(c);
  visCubeOK_p=True; 
}
void VisBuffer::setModelVisCube(Complex c)
{
  modelVisCube_p.resize(visIter_p->visibilityShape());
  modelVisCube_p.set(c);
  modelVisCubeOK_p=True;
}
void VisBuffer::setCorrectedVisCube(Complex c)
{
  correctedVisCube_p.resize(visIter_p->visibilityShape());
  correctedVisCube_p.set(c);
  correctedVisCubeOK_p=True;
}
void VisBuffer::setVisCube(const Cube<Complex>& vis)
{
  visCube_p.resize(vis.shape());
  visCube_p=vis;
  visCubeOK_p=True;
}
void VisBuffer::setModelVisCube(const Cube<Complex>& vis)
{
  modelVisCube_p.resize(vis.shape());
  modelVisCube_p=vis;
  modelVisCubeOK_p=True;
}
void VisBuffer::setCorrectedVisCube(const Cube<Complex>& vis)
{
  correctedVisCube_p.resize(vis.shape());
  correctedVisCube_p=vis;
  correctedVisCubeOK_p=True;
}

Int & VisBuffer::fillnChannel() 
{ 
  nChannelOK_p=True; 
  nChannel_p=visIter_p->channelGroupSize();
  return nChannel_p;
 }

Vector<Int> & VisBuffer::fillChannel()
{ channelOK_p=True; return visIter_p->channel(channel_p);}

Int & VisBuffer::fillnRow()
{ 
  nRowOK_p=True; 
  nRow_p=visIter_p->nRow(); 
  return nRow_p; 
}

Vector<Int>& VisBuffer::fillAnt1()
{ ant1OK_p=True; return visIter_p->antenna1(antenna1_p);}
Vector<Int>& VisBuffer::fillAnt2()
{ ant2OK_p=True; return visIter_p->antenna2(antenna2_p);}
Vector<SquareMatrix<Complex,2> >& VisBuffer::fillCjones()
{ cjonesOK_p=True; return visIter_p->CJones(cjones_p); }
Vector<Int>& VisBuffer::fillCorrType()
{ corrTypeOK_p=True; return visIter_p->corrType(corrType_p); }

Int& VisBuffer::fillFieldId()
{ 
  fieldIdOK_p=True; 
  fieldId_p=visIter_p->fieldId(); 
  return fieldId_p; 
}

Matrix<Bool>& VisBuffer::fillFlag()
{ flagOK_p=True; return visIter_p->flag(flag_p); }
Cube<Bool>& VisBuffer::fillFlagCube()
{ flagCubeOK_p=True; return visIter_p->flag(flagCube_p); }
Vector<Bool>& VisBuffer::fillFlagRow()
{ flagRowOK_p=True; return visIter_p->flagRow(flagRow_p);}
Vector<Double>& VisBuffer::fillFreq()
{ freqOK_p=True; return visIter_p->frequency(frequency_p); }
Vector<Double>& VisBuffer::fillLSRFreq()
{ lsrFreqOK_p=True; return visIter_p->lsrFrequency(lsrFrequency_p); }
MDirection& VisBuffer::fillPhaseCenter()
{ phaseCenterOK_p=True; return phaseCenter_p=visIter_p->phaseCenter();}

Int& VisBuffer::fillPolFrame()
{ 
  polFrameOK_p=True; 
  polFrame_p=visIter_p->polFrame();
  return polFrame_p;
}
Vector<Float>& VisBuffer::fillSigma()
{ sigmaOK_p=True; return visIter_p->sigma(sigma_p);}

Int& VisBuffer::fillSpW()
{ 
  spwOK_p=True; 
  spectralWindow_p=visIter_p->spectralWindow();
  return spectralWindow_p;
}

Vector<Double>& VisBuffer::fillTime()
{ timeOK_p=True; return visIter_p->time(time_p);}
Vector<RigidVector<Double,3> >& VisBuffer::filluvw()
{ uvwOK_p=True; return visIter_p->uvw(uvw_p);}
Matrix<CStokesVector>& 
VisBuffer::fillVis(VisibilityIterator::DataColumn whichOne)
{
  switch (whichOne) {
  case VisibilityIterator::Model:
    modelVisOK_p=True;
    return visIter_p->visibility(modelVisibility_p,whichOne);
    break;
  case VisibilityIterator::Corrected:
    correctedVisOK_p=True;
    return visIter_p->visibility(correctedVisibility_p,whichOne);
    break;    
  case VisibilityIterator::Observed:
  default:
    visOK_p=True; 
    return visIter_p->visibility(visibility_p,whichOne);
    break;
  }
}

Cube<Complex>& VisBuffer::fillVisCube(VisibilityIterator::DataColumn whichOne)
{ 
  switch(whichOne) {
  case VisibilityIterator::Model:
    modelVisCubeOK_p=True; 
    return visIter_p->visibility(modelVisCube_p,whichOne);
    break;
  case VisibilityIterator::Corrected:
    correctedVisCubeOK_p=True; 
    return visIter_p->visibility(correctedVisCube_p,whichOne);
    break;
  case VisibilityIterator::Observed:
  default:
    visCubeOK_p=True; 
    return visIter_p->visibility(visCube_p,whichOne);
    break;
  }
}

Vector<Float>& VisBuffer::fillWeight()
{ weightOK_p=True; return visIter_p->weight(weight_p);}
Matrix<Float>& VisBuffer::fillImagingWeight()
{ weightMatOK_p=True; return visIter_p->imagingWeight(weightMat_p);}

const Vector<Float>& VisBuffer::feed_pa(Double time) const
{return visIter_p->feed_pa(time);}


