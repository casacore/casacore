//# LatticeApply.h: optimally iterate through Lattcies and apply provided function
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
//
#if !defined(AIPS_LATTICEAPPLY_H)
#define AIPS_LATTICEAPPLY_H
 
#if defined(_AIX)
#pragma implementation ("LatticeApply.cc")
#endif

// <summary> Classes to asists computation with optimal lattice iteration </summary>
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> Lattice
//   <li> LatticeIterator
//   <li> LatticeStepper
//   <li> TiledStepper
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// Optimally iterate though a Lattice and apply the supplied function
// to the Lattice data.
// </synopsis>
//
// <example>
// <srcBlock>
// </srcBlock>
// </example>
// 
// <note role=caution>
// </note>
//      
// <note role=tip>
// </note>
//      
// <motivation>
// </motivation>
//      
// <todo asof="1997/08/01">   
//   <li> 
// </todo>
//
 
#include <aips/aips.h>
#include <aips/Containers/Block.h>
template <class T> class VectorCollapser;
template <class T> class Lattice;
class IPosition;

// Provided by the experts to push the data through the algorithm as
// efficiently as possible

template <class T> class LatticeApply
{
public:
  static void vectorApply(Lattice<T>& latticeOut, 
                          Lattice<T>& latticeIn,
                          VectorCollapser<T>& operation,
                          const Int profileAxis,
                          const IPosition blc,
                          const IPosition trc,
                          const Bool dropAxis,
                          const Bool showProgress,
                          const String progressTitle);

  static void vectorMultiApply(PtrBlock<Lattice<T> *> &latticeOut, 
                               Lattice<T>& latticeIn,
                               VectorCollapser<T>& collapser,
                               const Int VectorAxis,
                               const IPosition blc,
                               const IPosition trc,
                               const Bool dropAxis,
                               const Bool showProgress,
                               const String progressTitle);

private:

   static void prepare(IPosition& ioMap,
                       IPosition& inBlc,
                       IPosition& inTrc,
                       Lattice<T>& latticeIn,
                       Lattice<T>& latticeOut,
                       const Int VectorAxis,
                       const Bool dropAxis);

};

#endif
