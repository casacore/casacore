//# RFCubeLattice.cc: this defines RFCubeLattice
//# Copyright (C) 2000,2001
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
#include <lattices/Lattices/LatticeStepper.h>
#include <flagging/Flagging/RFCubeLattice.h>

namespace casa { //# NAMESPACE CASA - BEGIN

template<class T> RFCubeLatticeIterator<T>::RFCubeLatticeIterator ()
{
  curs=NULL;
}

template<class T> RFCubeLatticeIterator<T>::RFCubeLatticeIterator ( TempLattice<T> &lat,const IPosition &iter_shape )
  : LatticeIterator<T>(lat,LatticeStepper(lat.shape(),iter_shape,IPosition(2,0,1),IPosition()))
{
  iter_pos=0;
  read=False;
  write=True;
  curs=NULL;
}

template<class T> RFCubeLatticeIterator<T>::~RFCubeLatticeIterator ()
{
}

template<class T> Matrix<T> * RFCubeLatticeIterator<T>::setupCursor ()
{
  // fetch RO,WO or RW cursor
  if( read )
    return curs = ( write ? &rwMatrixCursor() : (Matrix<T>*) &matrixCursor() );
  return curs = &woMatrixCursor();
}

template<class T> Matrix<T> * RFCubeLatticeIterator<T>::reset ( Bool r,Bool w )
{
  LatticeIterator<T>::reset();
  read=r;
  write=w;
  iter_pos=0;
  return setupCursor();
}

template<class T> Matrix<T> * RFCubeLatticeIterator<T>::advance ( Int t1 )
{
  // advance the iterator
  iter_pos = LatticeIterator<T>::position()(2); 
  if( iter_pos>t1 )
  {
    LatticeIterator<T>::reset();
    iter_pos=0;
  }
  for( ; iter_pos<t1; iter_pos++,(*this)++ );
  return setupCursor();
}

template<class T> RFCubeLattice<T>::RFCubeLattice ()
{
}
template<class T> RFCubeLattice<T>::RFCubeLattice ( uInt nchan,uInt nifr,uInt ntime,Int maxmem )
{
  init(nchan,nifr,ntime,maxmem);
}
template<class T> RFCubeLattice<T>::RFCubeLattice ( uInt nchan,uInt nifr,uInt ntime,const T &init_val,Int maxmem )
{
  init(nchan,nifr,ntime,init_val,maxmem);
}
template<class T> RFCubeLattice<T>::~RFCubeLattice ()
{
  cleanup();
}

template<class T> void RFCubeLattice<T>::init ( uInt nchan,uInt nifr,uInt ntime,Int maxmem,Int tile_mb )
{
  lat_shape = IPosition(3,nchan,nifr,ntime);
// itertaor is one plane of lattice
  iter_shape = IPosition(3,nchan,nifr,1);
// select a tile size
  uInt tilesize = tile_mb*1024*1024, 
      planesize = iter_shape.product()*sizeof(T),
      ntile = (uInt)(tilesize/(Float)planesize+.2);
  tile_shape = IPosition(3,nchan,nifr,ntile);
//  cerr<<"Using "<<ntile<<" planes ("<<tile_shape.product()*sizeof(T)/(1024*1024.)<<"MB) tile\n";
  lat = TempLattice<T>( TiledShape(lat_shape,iter_shape),maxmem );
  iter = RFCubeLatticeIterator<T>( lat,iter_shape );
}

template<class T> RFCubeLatticeIterator<T>  RFCubeLattice<T>::newIter()
{
  return RFCubeLatticeIterator<T>( lat,iter_shape );
}

template<class T> void RFCubeLattice<T>::init ( uInt nchan,uInt nifr,uInt ntime,const T &init_val,Int maxmem,Int tile_mb )
{
  init(nchan,nifr,ntime,maxmem,tile_mb);
  lat.set(init_val);
  iter.setRead();
}

template<class T> void RFCubeLattice<T>::cleanup ()
{
  iter = RFCubeLatticeIterator<T>();
  lat = TempLattice<T>();
  lat_shape.resize(0);
}

template<class T> Matrix<T> * RFCubeLattice<T>::reset ( Bool r,Bool w )
{
  return iter.reset(r,w);
}


} //# NAMESPACE CASA - END

