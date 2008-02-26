//# VisTimeAverager.cc: Implementation of VisTimeAverager.h
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
//----------------------------------------------------------------------------

#include <msvis/MSVis/VisBuffAccumulator.h>
#include <casa/Exceptions/Error.h>

#define PRTLEV_VBA 0

namespace casa { //# NAMESPACE CASA - BEGIN

//----------------------------------------------------------------------------
  
VisBuffAccumulator::VisBuffAccumulator(const Int& nAnt, const Double& interval,
				       const Bool& prenorm) 
  : nAnt_p(nAnt), 
    interval_p(interval), 
    prenorm_p(prenorm),
    prtlev_(PRTLEV_VBA)
{
// Construct from the number of antennas and the averaging interval
// Input:
//    nAnt                 const Int&       No. of antennas
//    interval             const Double&    Time interval (in seconds).
//    prenorm              const Bool&      Pre-normalization flag
// Output to private data:
//    nAnt_p               Int              No. of antennas
//    interval_p           Double           Time interval (in seconds).
//    prenorm_p            Bool             Pre-normalization flag
//

  if (prtlev()>2) cout << "VBA::VBA()" << endl;

  // Interval MUST be strictly greater than zero
  if (interval_p < DBL_EPSILON)
    interval_p=0.1;   // TBD: is this reasonable? 

  // Reset the averager
  reset();
};

//----------------------------------------------------------------------------

VisBuffAccumulator::~VisBuffAccumulator()
{
// Null default destructor
//
  if (prtlev()>2) cout << "VBA::~VBA()" << endl;
};

//----------------------------------------------------------------------------

void VisBuffAccumulator::reset()
{
// Reset the averager
// Output to private data:
//    tStart_p         Double        Start time of current accumulation
//    firstInterval_p  Bool          Is this the first interval ?
//    nChan_p          Int           No. of channels in the averaging buffer
//    avrow_p          Int           Starting row of current accumulation
//    avBuf_p          VisBuffer     Averaging buffer
//
  tStart_p = 0.0;
  firstInterval_p = True;
  nCorr_p = 0;
  nChan_p = 0;
  avrow_p = 0;
  aveTime_p = 0.0;
  aveTimeWt_p = 0.0;

  if (prtlev()>2) cout << " VBA::reset()" << endl;

};

//----------------------------------------------------------------------------

void VisBuffAccumulator::accumulate (const VisBuffer& vb)
{
// Accumulate a VisBuffer
// Input:
//    vb               const VisBuffer&     VisBuffer to accumulate
// Output to private data:
//    tStart_p         Double               Start time of current accumulation
//    nChan_p          Int                  No. of channels in the avg. buffer
//    avrow_p          Int                  Start row of current accumulation
//    avBuf_p          VisBuffer            Averaging buffer
//

  if (prtlev()>2) cout << " VBA::accumulate()" << endl;

  // Check if avBuf_p initialization required; if so,
  // assign to vb to establish a two-way connection to
  // the underlying VisIter. 
  if (firstInterval_p) {
    // Initialize the averaging buffer
    //  TBD: Avoid this spurious data copy!!
    avBuf_p = vb;
    avBuf_p.nRow();
    avBuf_p.nRow() = 0;
  };

  // Iterate through the current VisBuffer
  Int row = 0;
  while (row < vb.nRow()) {
    // Find the next unflagged time
    while (row < vb.nRow() && vb.flagRow()(row)) {
      row++;
    };
    if (row < vb.nRow()) {
      Double thisTime = vb.time()(row);

      // Check for the first accumulation interval
      if (firstInterval_p) {
	tStart_p = thisTime;
	firstInterval_p = False;
	nChan_p = vb.nChannel();
        nCorr_p = vb.corrType().nelements();
	// Initialize the first accumulation interval
	initialize();
      };

      // Check for end of the current accumulation interval

      if ((vb.time()(row) - tStart_p) > (interval_p-DBL_EPSILON)) {

	// Normalize
	normalize();
	// Advance indices to the next accumulation interval
	tStart_p = thisTime;
	avrow_p += hashFunction(nAnt_p-1, nAnt_p-1) + 1;
	// Initialize the next accumulation interval
	initialize();
      };

      // Add the VisBuffer row to the current accumulation
      // 
      // Only accumulate VisBuffers with the same number of channels
      Int nCorr=vb.corrType().nelements();
      if (vb.nChannel() != nChan_p || nCorr != nCorr_p) {
	throw(AipsError("VisBuffAccumulator: data shape does not conform"));
      };


      Int ant1 = vb.antenna1()(row);
      Int ant2 = vb.antenna2()(row);

      // Calculate row from antenna numbers with the hash function.
      Int outrow = avrow_p + hashFunction (ant1, ant2);
      Float wt = vb.weight()(row);
      Vector<Float> wtM(vb.weightMat().column(row));

      // (Prenormalization removed!)

      // Accumulate the visCube itself
      //  TBD: Handle weights of channel-dep accumulation
      //       better (i.e., if some channels are sometimes
      //       flagged, they should accumulate with different
      //       total weights, even if we will maintain only 
      //       a channel-indep weightMat.  So, either use
      //       weightSpectrum, or at least accumulate per
      //       channel correctly!)

      Int goodChan(0);
      for (Int chn=0; chn<vb.nChannel(); chn++) {
	if (!vb.flag()(chn,row)) {
	  goodChan++;
	  avBuf_p.flag()(chn,outrow) = False;
	  for (Int cor=0;cor<nCorr;cor++) {
	    avBuf_p.visCube()(cor,chn,outrow) += 
	      (wtM(cor)*vb.visCube()(cor,chn,row));
	    avBuf_p.modelVisCube()(cor,chn,outrow) += 
	      (wtM(cor)*vb.modelVisCube()(cor,chn,row));
	  }
	}
      };

      // Only if there is any good channels this row
      if (goodChan > 0) {
	avBuf_p.flagRow()(outrow) = False;
	avBuf_p.weight()(outrow) += wt;
	for (Int cor=0;cor<nCorr;cor++) 
	  avBuf_p.weightMat()(cor,outrow) += wtM(cor);

	// UVW (vector average, is this right?)
	// gcc-3.2 needs the multiplication on a separate line; gcc-3.4 can do
	// it as: avBuf_p.uvw()(outrow) += vb.uvw()(row) * Double(wt);
	RigidVector<Double,3> wtuvw = vb.uvw()(row) * Double(wt);
	avBuf_p.uvw()(outrow) += wtuvw;
	
	// Accumulate global timestamp average for this interval:
	//   (subtract offset from time here to avoid roundoff problems)
	aveTime_p += (vb.time()(row)-tStart_p) * wt;
	aveTimeWt_p += wt;
      }

      // Increment the row number
      row++;
    }; // if (row < vb.nRow())
  }; // while (row < vb.nRow())
};

//----------------------------------------------------------------------------

void VisBuffAccumulator::finalizeAverage ()
{
// Finalize the average, and return the result
// Output:
//    avBuf         VisBuffer&       Averaged buffer
//

  if (prtlev()>2) cout << " VBA::finalizeAverage()" << endl;

  // Normalize the current (final) accumulation interval
  normalize();

};

//----------------------------------------------------------------------------

void VisBuffAccumulator::initialize()
{
// Initialize the next accumulation interval
// Output to private data:
//    avBuf_p          VisBuffer       Averaging buffer
//    

  if (prtlev()>2) cout << "  VBA::initialize()" << endl;

  Int nRowAdd = hashFunction (nAnt_p-1, nAnt_p-1) + 1;
  avBuf_p.nRow() += nRowAdd;
  avBuf_p.nChannel() = nChan_p;

  // Resize and initialize the VisBuffer columns used here
  Int nRow = avBuf_p.nRow();
  avBuf_p.antenna1().resize(nRow, True);
  avBuf_p.antenna2().resize(nRow, True);

  avBuf_p.time().resize(nRow, True); 
  avBuf_p.uvw().resize(nRow, True); 

  avBuf_p.visCube().resize(nCorr_p,nChan_p, nRow,True);
  avBuf_p.modelVisCube().resize(nCorr_p,nChan_p, nRow,True);

  avBuf_p.weight().resize(nRow, True); 
  avBuf_p.weightMat().resize(nCorr_p,nRow, True); 

  avBuf_p.flagRow().resize(nRow, True); 

  avBuf_p.flag().resize(nChan_p, nRow,True);


  // Fill in the antenna numbers for all rows
  Int row = avrow_p;
  for (Int ant1=0; ant1 < nAnt_p; ant1++) {
    for (Int ant2 = ant1; ant2 < nAnt_p; ant2++) {
      avBuf_p.antenna1()(row) = ant1;
      avBuf_p.antenna2()(row) = ant2;
      row++;
    }
  }

  // Initialize everything else
  for (row = avrow_p; row < nRow; row++) {
    avBuf_p.time()(row) = 0.0;
    avBuf_p.uvw()(row) = 0.0;
    for (Int chn = 0; chn < nChan_p; chn++) {
      avBuf_p.flag()(chn,row) = True;
      for (Int cor=0; cor < nCorr_p; cor++) {
	avBuf_p.visCube()(cor,chn,row) = Complex(0.0);
	avBuf_p.modelVisCube()(cor,chn,row) = Complex(0.0);
      }
    };
    avBuf_p.weight()(row) = 0.0f;
    avBuf_p.weightMat().column(row) = 0.0f;
    avBuf_p.flagRow()(row) = True;
  };

  // Init global timestamp
  aveTime_p = 0.0;
  aveTimeWt_p = 0.0;

};

//----------------------------------------------------------------------------

void VisBuffAccumulator::normalize()
{
// Normalize the current accumulation interval
// Output to private data:
//    avBuf_p         VisBuffer&       Averaged buffer
//  

  if (prtlev()>2) cout  << "  VBA::normalize()" << endl;

  // Only if there will be a valid timestamp
  if (aveTimeWt_p > 0.0 ) {

    // Global timestamp for this interval
    aveTime_p/=aveTimeWt_p;
    aveTime_p+=tStart_p;

    // Divide by the weights
    for (Int row=avrow_p; row<avBuf_p.nRow(); row++) {
      Float wt=avBuf_p.weight()(row);
      Vector<Float> wtM(avBuf_p.weightMat().column(row));
      if (wt==0.0f) avBuf_p.flagRow()(row)=True;
      if (!avBuf_p.flagRow()(row)) {
	avBuf_p.time()(row)=aveTime_p;
	avBuf_p.uvw()(row)*=(1.0f/wt);
	for (Int chn=0; chn<avBuf_p.nChannel(); chn++) {
	  if (!avBuf_p.flag()(chn,row)) {
	    for (Int cor=0;cor<nCorr_p;cor++) {
	      if (wtM(cor)>0.0f) {
		avBuf_p.visCube()(cor,chn,row)*=1.0f/wtM(cor);
		avBuf_p.modelVisCube()(cor,chn,row)*=1.0f/wtM(cor);
	      }
	      else {
		avBuf_p.visCube()(cor,chn,row)=0.0;
		avBuf_p.modelVisCube()(cor,chn,row)=0.0;
	      }
	    }
	  }
	}
      }
    }
  }
  else {
    // Ensure this interval is entirely flagged
    for (Int row=avrow_p; row<avBuf_p.nRow(); row++) {
      avBuf_p.flagRow()(row)=True;
      avBuf_p.weight()(row)=0.0f;
      avBuf_p.weightMat().column(row)=0.0f;
    }
  }
};

//----------------------------------------------------------------------------

Int VisBuffAccumulator::hashFunction (const Int& ant1, const Int& ant2)
{
// Compute row index in an accumulation interval for an
// interferometer index (ant1, ant2).
// Input:
//    ant1            const Int&      Antenna 1
//    ant2            const Int&      Antenna 2
// Output:
//    hashFunction    Int             Row offset in current accumulation
//
  Int index;
  index = nAnt_p * ant1 - (ant1 * (ant1 - 1)) / 2 + ant2 - ant1;
  return index;
};

//----------------------------------------------------------------------------

  



} //# NAMESPACE CASA - END

