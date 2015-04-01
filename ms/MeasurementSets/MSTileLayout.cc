//# MSTileLayout.cc: Implementation of MSTileLayout
//# Copyright (C) 2002
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

#include <casacore/ms/MeasurementSets/MSTileLayout.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

IPosition MSTileLayout::tileShape(const IPosition& dataShape,
				  Int observationType, Int nIfr, Int nInt)
{
  //Try to bypass most stupid choices
  if(nIfr<1) 
    nIfr=100;
  if(nInt<1)
    nInt=1;
  const Int ioBlockSize = 131072; // 131072 * sizeOf(Complex) = 1 MB
  IPosition tileShape(3,0,0,0);
  if (dataShape.nelements()==2 && dataShape(0)>0 && dataShape(1)>0) {
    // Always read all polarizations, since we'll often want to do conversion
    Int corrSize = dataShape(0);     
    // Read all channels, in order to minimize the overhead of the
    // i/o layer
    //Read all the channels for fast mosaic
    Int chanSize = dataShape(1);
    Int rowSize=max(1, ioBlockSize/corrSize/chanSize);
    if(observationType==0){
      if(chanSize <100){
	chanSize=max(1, ioBlockSize/corrSize/nIfr);
      }
      else if(chanSize < 10000) {
	chanSize=Int(floor(sqrt(Float(chanSize)/99.9)))*10;
      }
      else{
	chanSize=100;
      }
      Int elIOBlkSize=ioBlockSize;
      while(((ioBlockSize/corrSize/chanSize) > 10*nIfr*nInt) && chanSize < dataShape(1)){
	chanSize+=2;
      } 
      if((ioBlockSize/corrSize/chanSize) > 10*nIfr*nInt){
	//we are still having too many blocks in one tile
	elIOBlkSize=10*nIfr*nInt*corrSize*chanSize;
      }
      chanSize=(chanSize >=   dataShape(1)) ? dataShape(1) : chanSize; 
      rowSize= max(1,elIOBlkSize/corrSize/chanSize);
      
    }
    else{
      //fast mosaic mode: no need to make people load several pointings
      rowSize=nIfr*nInt;
      chanSize=max(1, ioBlockSize/corrSize/rowSize);
      chanSize=(chanSize >   dataShape(1)) ? dataShape(1) : chanSize; 
    }

    tileShape(0)=corrSize; 
    tileShape(1)=chanSize;
    tileShape(2)=rowSize;
  }
  return tileShape;
}

IPosition MSTileLayout::tileShape(const IPosition& dataShape,
					 Int observationType, 
					 const String& array)
{
  Int nIfr=200;
  if (array=="ATCA") nIfr=15;
  if (array=="VLA") nIfr=351;
  if (array=="WSRT") nIfr=91;
  if (array=="BIMA") nIfr=36;
  if (array=="DRAO") nIfr=21;
  if (array=="SMA") nIfr=28;
  return tileShape(dataShape,observationType,nIfr);
}

} //# NAMESPACE CASACORE - END

