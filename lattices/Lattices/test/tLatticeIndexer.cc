// tLatticeIndexer.cc:  mechanical test of LatticeIndexer class
//# Copyright (C) 1995,1996,1997,1999,2000,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
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
//#
//# $Id$

#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/LatticeIndexer.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main ()
{
  try {
    {
      LatticeIndexer l;
      AlwaysAssert(l.nelements() == 1, AipsError);
      AlwaysAssert(l.ndim() == 1, AipsError);
      AlwaysAssert(l.increment().isEqual(IPosition(1,1)), AipsError);
      AlwaysAssert(l.offset().isEqual(IPosition(1,0)), AipsError);
      
      l.resize(IPosition(3,10,20,30));
      AlwaysAssert(l.nelements() == 10*20*30, AipsError);
      AlwaysAssert(l.ndim() == 3, AipsError);
      AlwaysAssert(l.shape().isEqual(IPosition(3,10,20,30)), AipsError);
      AlwaysAssert(l.increment().isEqual(IPosition(3,1)), AipsError);
      AlwaysAssert(l.offset().isEqual(IPosition(3,0)), AipsError);
      
      l.subSection(IPosition(3,1,2,3), IPosition(3,9,19,29),
		   IPosition(3,2,3,4));
      AlwaysAssert(l.nelements() == 5*6*7, AipsError);
      AlwaysAssert(l.ndim() == 3, AipsError);
      AlwaysAssert(l.shape(0) == 5, AipsError);
      AlwaysAssert(l.shape(1) == 6, AipsError);
      AlwaysAssert(l.shape(2) == 7, AipsError);
      AlwaysAssert(l.increment(0) == 2, AipsError);
      AlwaysAssert(l.increment(1) == 3, AipsError);
      AlwaysAssert(l.increment(2) == 4, AipsError);
      AlwaysAssert(l.offset(0) == 1, AipsError);
      AlwaysAssert(l.offset(1) == 2, AipsError);
      AlwaysAssert(l.offset(2) == 3, AipsError);
      AlwaysAssert(l.fullShape(0) == 10, AipsError);
      AlwaysAssert(l.fullShape(1) == 20, AipsError);
      AlwaysAssert(l.fullShape(2) == 30, AipsError);
      
      l.subSection(IPosition(3,2,2,1), IPosition(3,4,5,6), IPosition(3,2,3,6));
      AlwaysAssert(l.nelements() == 2*2*1, AipsError);
      AlwaysAssert(l.ndim() == 3, AipsError);
      AlwaysAssert(l.shape().isEqual(IPosition(3,2,2,1)), AipsError);
      AlwaysAssert(l.increment().isEqual(IPosition(3,4,9,24)), AipsError);
      AlwaysAssert(l.offset().isEqual(IPosition(3,5,8,7)), AipsError);
      AlwaysAssert(l.fullShape().isEqual(IPosition(3,10,20,30)), AipsError);
   
      AlwaysAssert(l.absolutePosition(IPosition(3,0))
		   .isEqual(IPosition(3,5,8,7)), AipsError);
      AlwaysAssert(l.absolutePosition(IPosition(3,1))
		   .isEqual(IPosition(3,9,17,31)), AipsError);
   
      l.fullSize();
      AlwaysAssert(l.ndim() == 3, AipsError);
      AlwaysAssert(l.shape().isEqual(IPosition(3,10,20,30)), AipsError);
      AlwaysAssert(l.increment().isEqual(IPosition(3,1)), AipsError);
      AlwaysAssert(l.offset().isEqual(IPosition(3,0)), AipsError);
    }
    {
      LatticeIndexer l(IPosition(3,5,6,7));
      LatticeIndexer lc(l), la;
      la = l;
      l.subSection(IPosition(3,2,2,1), IPosition(3,4,5,6));
      AlwaysAssert(lc.shape().isEqual(IPosition(3,5,6,7)), AipsError);
      AlwaysAssert(lc.increment().isEqual(IPosition(3,1)), AipsError);
      AlwaysAssert(lc.offset().isEqual(IPosition(3,0)), AipsError);
      AlwaysAssert(la.shape().isEqual(IPosition(3,5,6,7)), AipsError);
      AlwaysAssert(la.increment().isEqual(IPosition(3,1)), AipsError);
      AlwaysAssert(la.offset().isEqual(IPosition(3,0)), AipsError);
    }
    {
      LatticeIndexer l(IPosition(3,5,6,7));
      IPosition point(3,0);
      IPosition shape(3,3);
      IPosition heading(3,0,2,1); // move along the x-axis then z-axis
      // Move forward through all the locations
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,3,0,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,0,0,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,3,0,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,0,0,6), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,3,0,6), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,0,3,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,3,3,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,0,3,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,3,3,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,0,3,6), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,3,3,6), AipsError);
      // Should not be able to move any further
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading)==False, 
		   AipsError);
      // Now move backwards one step and check we ended up where we were
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,0,3,6), AipsError);
      // set the position to the last element and move backwards from there
      point = l.shape() - 1;
      heading = IPosition(3,2,1,0);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,4,5,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,4,5,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,4,2,6), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,4,2,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,4,2,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,4,-1,6), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,4,-1,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,4,-1,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,1,5,6), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,1,5,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,1,5,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,1,2,6), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,1,2,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,1,2,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,1,-1,6), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,1,-1,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,1,-1,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,-2,5,6), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,-2,5,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,-2,5,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,-2,2,6), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,-2,2,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,-2,2,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,-2,-1,6), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,-2,-1,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),AipsError);
      AlwaysAssert(point == IPosition(3,-2,-1,0), AipsError);
      // Should not be able to move back any further
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading)==False, 
		   AipsError);
    }
    {
      // test tiledCursorMove with degenerate axes.
      LatticeIndexer l(IPosition(3,7,9,1), IPosition(3,1,1,0), 
		       IPosition(3,6,8,0), IPosition(3,2,2,1));
      IPosition point(3,0);
      IPosition shape(3,2,2,1);
      IPosition heading(3,0,1,2); // move along the x-axis then the y-axis
      // Move forward through all the locations
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,2,0,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,0,2,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,2,2,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading)==False, 
		   AipsError);
      // Move backward through all the locations. 
      point = l.shape() - 1;
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,0,3,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,2,1,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,0,1,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,2,-1,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading), AipsError);
      AlwaysAssert(point == IPosition(3,0,-1,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading)==False, 
		   AipsError);
    }
    {
      LatticeIndexer l(IPosition(3,7,9,1), IPosition(3,1,1,0), 
		       IPosition(3,6,8,0), IPosition(3,2,2,1));
      AlwaysAssert(l.isInside(IPosition(3,2,3,0))==True, AipsError);
      AlwaysAssert(l.isInside(IPosition(3,3,3,0))==False, AipsError);
      AlwaysAssert(l.isInside(IPosition(3,-1,0,0))==False, AipsError);
      
    }
    {
      LatticeIndexer l(IPosition(2,8,5));
      IPosition shape(2,3);
      IPosition point(2,0);
      IPosition heading(2,0,1);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading),
		   AipsError);
      AlwaysAssert(point == IPosition(2,3,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading),
		   AipsError);
      AlwaysAssert(point == IPosition(2,6,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading)
		   , AipsError);
      AlwaysAssert(point == IPosition(2,0,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading)
		   , AipsError);
      AlwaysAssert(point == IPosition(2,3,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading)
		   , AipsError);
      AlwaysAssert(point == IPosition(2,6,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading) == False
		   , AipsError);
      //
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,3,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,0,3), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,6,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,3,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,0,0), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading)
 		   == False, AipsError);

      point = IPosition(2,1,1);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,4,1), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,7,1), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,-2,4), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,1,4), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,4,4), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,7,4), AipsError);
      AlwaysAssert(l.tiledCursorMove(True, point, shape, heading)
		   == False, AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),
		   AipsError);
      AlwaysAssert(point == IPosition(2,4,4), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,1,4), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,-2,4), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,7,1), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,4,1), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,1,1), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,-2,1), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,7,-2), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,4,-2), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,1,-2), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading),
 		   AipsError);
      AlwaysAssert(point == IPosition(2,-2,-2), AipsError);
      AlwaysAssert(l.tiledCursorMove(False, point, shape, heading)
 		   == False, AipsError);
    }
    cout << "OK" << endl;
    return 0;
  } catch   (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  
}

