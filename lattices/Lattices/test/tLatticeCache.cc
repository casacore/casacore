//# tLattice.cc:  test the Lattice class
//# Copyright (C) 1994,1995,1997,1999,2000,2001,2002
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or(at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/LatticeCache.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/lattices/Lattices/PagedArray.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/COWPtr.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicMath/Random.h>

#include <casacore/casa/stdlib.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
void a() {
    int32_t arraySize=2048;
    cout<<"Array Size?   "; cin>>arraySize; 
    IPosition map2shape(2, arraySize, arraySize);
    int32_t tileSize=16;
    int32_t cacheSize=351*tileSize*tileSize;
    int32_t trials=1000;
    float tileOverlap=0.5;
    int32_t imageTileSize=16;
    cout<<"Image Tile Size?    "; cin>>imageTileSize; 
    //    cout<<"Tile Overlap? "; cin>>tileOverlap; 
    //    cout<<"Cache Size?   "; cin>>cacheSize; 
    cout<<"Trials?       "; cin>>trials; 
    IPosition tileShape(2,tileSize,tileSize);
    IPosition imageTileShape(2,imageTileSize,imageTileSize);
    Vector<float> tileOverlapVec(2);
    tileOverlapVec=tileOverlap;
    PagedArray<Complex> pi2(TiledShape(map2shape, imageTileShape));
    pi2.setCacheSizeInTiles(1);
    LatticeCache<Complex> itc(pi2, cacheSize, tileShape, tileOverlapVec,
			       (tileOverlap>0.));      
    MLCG rng(835, 05401);
    DiscreteUniform randomPos(&rng, tileSize, arraySize-tileSize-1);
    Uniform randomChoice(&rng, 0.0, 1.0);
    Timer timer;
    timer.mark();
    pi2.set(0.0);
    cout<<"Time to initialize array = "<<1000.0*timer.real()<<" ms"<<endl;
    timer.mark();
    IPosition tilePos(2,0);
    if(trials<0) {
      IPosition myPos=IPosition(2,0);
      for (int32_t j=0;j<arraySize;j++) {
	for (int32_t i=0;i<arraySize;i++) {
	  myPos=IPosition(2,i,j);
	  Array<Complex>& myTile=itc.tile(tilePos,myPos,false);
	  cout<<"Filling tile at "<<myPos<<" -> "<<tilePos<<endl;
	  myTile(myPos-tilePos)+=1.0;
	}
      }
    }
    else {
      int32_t i=randomPos.asInt();
      int32_t j=randomPos.asInt();
      IPosition myPos=IPosition(2,i,j);
      double missFraction=0.0;
      cout<<"MissFraction ? ";cin>>missFraction;
      for (int32_t trial=0;trial<trials;trial++) {
	if(randomChoice()<missFraction) {
	  i=randomPos.asInt();
	  j=randomPos.asInt();
	  myPos=IPosition(2,i,j);
	  //	  cout<<"New tile on trial "<<trial<<" at "<<myPos<<endl;
	}
	Array<Complex>& myTile=itc.tile(tilePos,myPos,false);
	myTile(myPos-tilePos)+=1.0;
      }
    }
    itc.flush();
    pi2.showCacheStatistics(cout);
    itc.showCacheStatistics(cout);
    cout<<"Time per tile = "<<1000.0*timer.real()/trials<<" ms"<<endl;
    //    pi2.table().flush();
}

void b() {
    int32_t arraySize=128;
    cout<<"Array Size?   "; cin>>arraySize; 
    int32_t nChannels=128;
    int32_t nChanTile=128;
    int32_t nPol=1;
    int32_t nPolTile=1;
    int32_t tileSize=16;
    int32_t cacheSize=351*tileSize*tileSize*nChanTile*nPolTile;
    int32_t trials=100;
    int32_t imageTileSize=16;
    cout<<"Image Tile Size?    "; cin>>imageTileSize; 
    float tileOverlap=0.5;
    //    cout<<"Tile Size?    "; cin>>tileSize; 
    //    cout<<"Tile Overlap? "; cin>>tileOverlap; 
    //    cout<<"Cache Size?   "; cin>>cacheSize; 
    cout<<"Trials?       "; cin>>trials; 
    Vector<float> tileOverlapVec(4);
    tileOverlapVec=0.0;
    tileOverlapVec(0)=tileOverlap;
    tileOverlapVec(1)=tileOverlap;
    IPosition tileShape(4,tileSize,tileSize,nPolTile,nChanTile);
    IPosition map4shape(4, arraySize, arraySize, nPol, nChannels);
    IPosition imageTileShape(4,imageTileSize,imageTileSize,1,imageTileSize);
    PagedArray<float> pi4(TiledShape(map4shape, imageTileShape));
    pi4.setCacheSizeInTiles(0);
    LatticeCache<float> itc(pi4, cacheSize, tileShape, tileOverlapVec,
			       (tileOverlap>0.0));      
    MLCG rng(835, 05401);
    DiscreteUniform randomPos(&rng, tileSize, arraySize-tileSize-1);
    DiscreteUniform randomChan(&rng, 0, 31);
    DiscreteUniform randomPol(&rng, 0, 3);
    Uniform randomChoice(&rng, 0.0, 1.0);
    Timer timer;
    timer.mark();
    pi4.set(0.0);
    cout<<"Time to initialize array = "<<1000.0*timer.real()<<" ms"<<endl;
    timer.mark();
    int32_t i=randomPos.asInt();
    int32_t j=randomPos.asInt();
    int32_t pol=randomPol.asInt();
    int32_t chan=randomChan.asInt();
    IPosition myPos=IPosition(4,i,j,pol,chan);
    double missFraction=0.0;
    cout<<"MissFraction ? ";cin>>missFraction;
    for (int32_t trial=0;trial<trials;trial++) {
      if(randomChoice()<missFraction) {
	i=randomPos.asInt();
	j=randomPos.asInt();
// 	pol=randomPol.asInt();
// 	chan=randomChan.asInt();
	pol=0;
	chan=0;
        myPos=IPosition(4,i,j,pol,chan);
        cout<<"New tile on trial "<<trial<<" at "<<myPos<<endl;
      }
      IPosition tilePos(4, 0);
      Array<float>& myTile=itc.tile(tilePos,myPos,false);
      IPosition offPos=myPos-tilePos;
      myTile(offPos)+=1.0;
    }
    itc.flush();
    cout<<"Time per tile = "<<1000.0*timer.real()/trials<<" ms"<<endl;
    pi4.showCacheStatistics(cout);
    itc.showCacheStatistics(cout);
    //    pi4.table().flush();
}

int main()
{
  try {
    cout<<">>>"<<endl;
    int32_t type=1;
    cout<<"Enter 0 for 2D, 1 for 4D, 2 for both ";
    cin>>type;
    switch(type) {
    case 0:
      a();
      break;
    case 1:
      b();
      break;
    default:
      a();
      b();
    }

    cout<<"<<<"<<endl;
    cout<< "OK"<< endl;
  } catch (std::exception& x) {
    cerr << "Exception caught: " << x.what() << endl;
  } 

  return 0;
}
