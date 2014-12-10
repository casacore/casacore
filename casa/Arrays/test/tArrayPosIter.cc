//# tArrayPosIter.cc: This program tests the class ArrayPosIter
//# Copyright (C) 2004
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

//# Includes

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayPosIter.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>

using namespace casacore;

int main()
{
  {   
    cout << "\nBEGIN.  Testing ArrayPositionIterator.  0 dim. ......\n";

    IPosition shape(2);
    shape(0) = shape(1) = 5;

    ArrayPositionIterator ai (shape, 0);
    AlwaysAssertExit (ai.atStart());
    AlwaysAssertExit (!ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 2);
    IPosition index (2);
    uInt iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        cout << iloop << " [ " << index (0) << " " << index (1) << " ] \n";

    }
    AlwaysAssertExit (iloop == 25);
    AlwaysAssertExit (!ai.atStart());
    AlwaysAssertExit (ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 2);
    ai.origin();
    AlwaysAssertExit (ai.atStart());
    AlwaysAssertExit (!ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 2);

    cout << "END.  Testing ArrayPositionIterator.  0 dim. ......\n";
  }

  {   
    cout << "\nBEGIN.  Testing ArrayPositionIterator.  1 dim. ......\n";

    IPosition shape(2);
    shape(0) = shape(1) = 5;

    ArrayPositionIterator ai (shape, 1);
    AlwaysAssertExit (ai.atStart());
    AlwaysAssertExit (!ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 2);
    IPosition index (2);
    uInt iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        cout << iloop << " [ " << index (0) << " " << index (1) << " ] "
             << "\n";
    }
    AlwaysAssertExit (iloop == 5);
    AlwaysAssertExit (!ai.atStart());
    AlwaysAssertExit (ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 2);
    ai.origin();
    AlwaysAssertExit (ai.atStart());
    AlwaysAssertExit (!ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 2);

    cout << "END.  Testing ArrayPositionIterator.  1 dim. ......\n";
  }

  {   
    cout << "\nBEGIN.  Testing ArrayPositionIterator.  2 dim. ......\n";

    IPosition shape(2);
    shape(0) = shape(1) = 5;

    ArrayPositionIterator ai (shape, 2);
    AlwaysAssertExit (ai.atStart());
    AlwaysAssertExit (!ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 2);
    IPosition index (2);
    uInt iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        cout << iloop << " [ " << index (0) << " " << index (1) << " ] "
             << "\n";
    }
    AlwaysAssertExit (iloop == 1);
    AlwaysAssertExit (!ai.atStart());
    AlwaysAssertExit (ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 2);
    ai.origin();
    AlwaysAssertExit (ai.atStart());
    AlwaysAssertExit (!ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 2);

    cout << "END.  Testing ArrayPositionIterator.  2 dim. ......\n";
  }

  {   
    cout << "\nBEGIN.  Testing ArrayPositionIterator.  dim 2 ......\n";

    IPosition shape(3);
    shape(0) = 5;
    shape(1) = 3;
    shape(2) = 7;

    ArrayPositionIterator ai (shape, 2);
    AlwaysAssertExit (ai.atStart());
    AlwaysAssertExit (!ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 3);
    IPosition index (3);
    uInt iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        cout << iloop << " [ " << index (0) << " " << index (1)
	     << " " << index(2) << " ] "
             << "\n";
    }
    AlwaysAssertExit (iloop == 7);
    AlwaysAssertExit (!ai.atStart());
    AlwaysAssertExit (ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 3);
    ai.origin();
    AlwaysAssertExit (ai.atStart());
    AlwaysAssertExit (!ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 3);

    cout << "END.  Testing ArrayPositionIterator.  dim 2 ......\n";
  }

  {   
    cout << "\nBEGIN.  Testing ArrayPositionIterator.  dim 0,1 ......\n";

    IPosition shape(3);
    shape(0) = 5;
    shape(1) = 3;
    shape(2) = 7;

    ArrayPositionIterator ai (shape, IPosition(1,2));
    AlwaysAssertExit (ai.atStart());
    AlwaysAssertExit (!ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 3);
    IPosition index (3);
    uInt iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        cout << iloop << " [ " << index (0) << " " << index (1)
	     << " " << index(2) << " ] "
             << "\n";
    }
    AlwaysAssertExit (iloop == 15);
    AlwaysAssertExit (!ai.atStart());
    AlwaysAssertExit (ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 3);
    ai.origin();
    AlwaysAssertExit (ai.atStart());
    AlwaysAssertExit (!ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 3);

    cout << "END.  Testing ArrayPositionIterator.  dim 0,1 ......\n";
  }

  {   
    cout << "\nBEGIN.  Testing ArrayPositionIterator.  dim 2,0 ......\n";

    IPosition shape(3);
    shape(0) = 5;
    shape(1) = 3;
    shape(2) = 7;

    ArrayPositionIterator ai (shape, IPosition(2,2,0), False);
    AlwaysAssertExit (ai.atStart());
    AlwaysAssertExit (!ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 3);
    IPosition index (3);
    uInt iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        cout << iloop << " [ " << index (0) << " " << index (1)
	     << " " << index(2) << " ] "
             << "\n";
    }
    AlwaysAssertExit (iloop == 35);
    AlwaysAssertExit (!ai.atStart());
    AlwaysAssertExit (ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 3);
    ai.origin();
    AlwaysAssertExit (ai.atStart());
    AlwaysAssertExit (!ai.pastEnd());
    AlwaysAssertExit (ai.ndim() == 3);

    cout << "END.  Testing ArrayPositionIterator.  dim 2,0 ......\n";
  }

  return 0;
}
