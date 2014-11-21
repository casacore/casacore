//# tCompositeNumber.cc:  this tests CompositeNumber
//# Copyright (C) 1996,1997,1999,2000,2001
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

#include <casacore/casa/iostream.h>
#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/CompositeNumber.h>


#include <casacore/casa/namespace.h>
int main()
{

  {
    CompositeNumber cn;
    uInt n;
    n= cn.nextLarger(41);
    cout << "Next larger composite number of 41 is " << n << endl;
    n= cn.nextLargerEven(41);
    cout << "Next larger even composite number of 41 is " << n << endl;
    n = cn.nextSmaller(41);
    cout << "Next smaller composite number of 41 is " << n << endl;
    n = cn.nextSmallerEven(41);
    cout << "Next smaller even composite number of 41 is " << n << endl;
    n = cn.nearest(41);
    cout << "The nearest composite number to 41 is " << n << endl;
    n = cn.nearestEven(41);
    cout << "The nearest even composite number to 41 is " << n << endl;
  
    n = cn.nextLarger(397);
    cout << "Next larger composite number of 397 is " << n << endl;
    n = cn.nextLargerEven(397);
    cout << "Next larger even composite number of 397 is " << n << endl;
    n = cn.nextSmallerEven(397);
    cout << "Next smaller even composite number of 397 is " << n << endl;
    n = cn.nextSmaller(397);
    cout << "Next smaller composite number of 397 is " << n << endl;
    n = cn.nearest(397);
    cout << "The nearest composite number to 397 is " << n << endl; 
    n = cn.nearestEven(397);
    cout << "The nearest even composite number to 397 is " << n << endl; 
  
    n = cn.nextLargerEven(9362);
    cout << "Next larger even composite number of 9362 is " << n << endl;
    n = cn.nextLarger(9362);
    cout << "Next larger composite number of 9362 is " << n << endl;
    n = cn.nextSmaller(9362);
    cout << "Next smaller composite number of 9362 is " << n << endl;
    n = cn.nearest(9362);
    cout << "The nearest composite number to 9362 is " << n << endl; 
    n = cn.nearestEven(9362);
    cout << "The nearest even composite number to 9362 is " << n << endl; 

    cout << "Is 40 composite? " << cn.isComposite(40) << endl;
    cout << "Is 41 composite? " << cn.isComposite(41) << endl;
    cout << "Is 128 composite? " << cn.isComposite(128) << endl;
    cout << "Is 129 composite? " << cn.isComposite(129) << endl;
    cout << "Is 1024 composite? " << cn.isComposite(1024) << endl;
    cout << "Is 1025 composite? " << cn.isComposite(1025) << endl;
    cout << "Is 1026 composite? " << cn.isComposite(1026) << endl;
    cout << "Is 1027 composite? " << cn.isComposite(1027) << endl;
  }

  {
    CompositeNumber cn(100);
    uInt n;
    n= cn.nextLarger(41);
    cout << "Next larger composite number of 41 is " << n << endl;
    n = cn.nextSmaller(41);
    cout << "Next smaller composite number of 41 is " << n << endl;
    n = cn.nearest(41);
    cout << "The nearest composite number to 41 is " << n << endl;
  
    n = cn.nextLarger(397);
    cout << "Next larger composite number of 397 is " << n << endl;
    n = cn.nextSmaller(397);
    cout << "Next smaller composite number of 397 is " << n << endl;
    n = cn.nearest(397);
    cout << "The nearest composite number to 397 is " << n << endl; 

    cout << "Is 40 composite? " << cn.isComposite(40) << endl;
    cout << "Is 41 composite? " << cn.isComposite(41) << endl;
    cout << "Is 128 composite? " << cn.isComposite(128) << endl;
    cout << "Is 129 composite? " << cn.isComposite(129) << endl;
    cout << "Is 1024 composite? " << cn.isComposite(1024) << endl;
    cout << "Is 1025 composite? " << cn.isComposite(1025) << endl;
    cout << "Is 1026 composite? " << cn.isComposite(1026) << endl;
    cout << "Is 1027 composite? " << cn.isComposite(1027) << endl;
  }
}
