//# VisBuffer.cc: buffer for iterating through MS in large blocks
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

#include <trial/MeasurementEquations/VisibilityIterator.h>
#include <trial/MeasurementEquations/VisBuffer.h>

VisBuffer::VisBuffer():visIter_p((ROVisibilityIterator*)0),This(this),
  nChannel_p(0),nRow_p(0),twoWayConnection_p(False)
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
      cjonesOK_p=other.cjonesOK_p;
      fieldIdOK_p=other.fieldIdOK_p;
      flagOK_p=other.flagOK_p;
      flagRowOK_p=other.flagRowOK_p;
      freqOK_p=other.freqOK_p;
      phaseCenterOK_p=other.phaseCenterOK_p;
      polFrameOK_p=other.polFrameOK_p;
      sigmaOK_p=other.sigmaOK_p;
      spwOK_p=other.spwOK_p;
      timeOK_p=other.timeOK_p;
      uvwOK_p=other.uvwOK_p;
      visOK_p=other.visOK_p;
      weightOK_p=other.weightOK_p;
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
    if (cjonesOK_p) {
      cjones_p.resize(other.cjones_p.nelements()); 
      cjones_p=other.cjones_p;
    }
    if (fieldIdOK_p) fieldId_p=other.fieldId_p;
    if (flagOK_p) {
      flag_p.resize(other.flag_p.shape()); 
      flag_p=other.flag_p;
    }
    if (flagRowOK_p) {
      flagRow_p.resize(other.flagRow_p.nelements());
      flagRow_p=other.flagRow_p;
    }
    if (freqOK_p) {
      frequency_p.resize(other.frequency_p.nelements()); 
      frequency_p=other.frequency_p;
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
    if (weightOK_p) {
      weight_p.resize(other.weight_p.nelements()); 
      weight_p=other.weight_p;
    }
  }
  return *this;
}
  
VisBuffer::~VisBuffer()
{ 
  if (visIter_p!=(ROVisibilityIterator*)0 && twoWayConnection_p) 
    visIter_p->detachVisBuffer(*this);
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
    fieldIdOK_p=flagOK_p=flagRowOK_p=freqOK_p=phaseCenterOK_p=polFrameOK_p=
    sigmaOK_p=spwOK_p=timeOK_p=uvwOK_p=visOK_p=weightOK_p = False;
}

void VisBuffer::validate()
{
  nChannelOK_p=channelOK_p=nRowOK_p=ant1OK_p=ant2OK_p=cjonesOK_p=
    fieldIdOK_p=flagOK_p=flagRowOK_p=freqOK_p=phaseCenterOK_p=polFrameOK_p=
    sigmaOK_p=spwOK_p=timeOK_p=uvwOK_p=visOK_p=weightOK_p = True;
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

Int& VisBuffer::fillFieldId()
{ 
  fieldIdOK_p=True; 
  fieldId_p=visIter_p->fieldId(); 
  return fieldId_p; 
}

Matrix<Bool>& VisBuffer::fillFlag()
{ flagOK_p=True; return visIter_p->flag(flag_p); }
Vector<Bool>& VisBuffer::fillFlagRow()
{ flagRowOK_p=True; return visIter_p->flagRow(flagRow_p);}
Vector<Double>& VisBuffer::fillFreq()
{ freqOK_p=True; return visIter_p->frequency(frequency_p); }
MDirection& VisBuffer::fillPhaseCenter()
{ phaseCenterOK_p=True; return phaseCenter_p=visIter_p->phaseCenter();}

Int& VisBuffer::fillPolFrame()
{ 
  polFrameOK_p=True; 
  polFrame_p=visIter_p->polFrame();
  return polFrame_p;
}
//Vector<Int>& VisBuffer::fillPolTypes()
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
Matrix<CStokesVector>& VisBuffer::fillVis()
{ visOK_p=True; return visIter_p->visibility(visibility_p);}
Vector<Float>& VisBuffer::fillWeight()
{ weightOK_p=True; return visIter_p->weight(weight_p);}

const Vector<Float>& VisBuffer::feed_pa(Double time) const
{return visIter_p->feed_pa(time);}










