//# tArrayAccessor.cc: Test program for the ArrayAccessor class
//# Copyright (C) 2002
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
#include <casacore/casa/Arrays/ArrayAccessor.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main() {
  // Size test cube
  const uInt N0=10;
  const uInt N1=20;

  const uInt N2=30;
  // Test cube
  Cube<Int> cub(N0,N1,N2);
  indgen(cub);
  try {
    cout << "--------- Test ArrayAccessor ---------------------" << endl;
    Int inx(0);
    for (uInt i=0; i<N2; ++i) {
      for (uInt j=0; j<N1; ++j) {
	for (uInt k=0; k<N0; ++k) AlwaysAssertExit(cub(k,j,i) == inx++);
      }
    }
    cout << "Cube of dimensions (" << N0 << "," << N1 << "," << N2 <<
      ") properly filled" << endl;
    cout << "--------------------------------------------------" << endl;

    cout << "Testing Axis<> constructors --- ";
    inx = 0;
    for (ArrayAccessor<Int, Axis<2> > i(cub); i!=i.end(); ++i) {
      for (ArrayAccessor<Int, Axis<1> > j(i); j!=j.end(); ++j) {
	for (ArrayAccessor<Int, Axis<0> > k(j); k!=k.end(); ++k) {
	  AlwaysAssertExit(*k == inx++);
	}
      }
    }
    cout << "ok" << endl <<
      "--------------------------------------------------" << endl;

    cout << "Testing AxisN constructors --- ";
    inx = 0;
    for (ArrayAccessor<Int, AxisN> i(cub, AxisN(2)); i!=i.end(); ++i) {
      for (ArrayAccessor<Int, AxisN> j(i, AxisN(1)); j!=j.end(); ++j) {
	for (ArrayAccessor<Int, AxisN> k(j, AxisN(0)); k!=k.end(); ++k) {
	  AlwaysAssertExit(*k == inx++);
	}
      }
    }
    cout << "ok" << endl <<
      "--------------------------------------------------" << endl;

    cout << "Testing Mixed Axis<> and AxisN constructors --- ";
    inx = 0;
    for (ArrayAccessor<Int, Axis<2> > i(cub); i!=i.end(); ++i) {
      for (ArrayAccessor<Int, AxisN> j(i, AxisN(1)); j!=j.end(); ++j) {
	for (ArrayAccessor<Int, Axis<0> > k(j); k!=k.end(); ++k) {
	  AlwaysAssertExit(*k == inx++);
	}
      }
    }
    inx = 0;
    for (ArrayAccessor<Int, AxisN> i(cub, AxisN(2)); i!=i.end(); ++i) {
      for (ArrayAccessor<Int, Axis<1> > j(i); j!=j.end(); ++j) {
	for (ArrayAccessor<Int, AxisN> k(j, AxisN(0)); k!=k.end(); ++k) {
	  AlwaysAssertExit(*k == inx++);
	}
      }
    }
    cout << "ok" << endl <<
      "--------------------------------------------------" << endl;

    cout << "Testing Axis<> assignments --- ";
      inx = 0;
    {
      ArrayAccessor<Int, Axis<2> > i(cub);
      ArrayAccessor<Int, Axis<1> > j;
      ArrayAccessor<Int, Axis<0> > k;
      ArrayAccessor<Int, Axis<0> > l;
      for (; i!=i.end(); ++i) {
	for (j = i; j!=j.end(); ++j) {
	  for (k =j; k!=k.end(); ++k) {
	    AlwaysAssertExit(*k == inx);
	    l = k;
	    AlwaysAssertExit(*l == inx++);
	  }
	}
      }
    }
    cout << "ok" << endl <<
      "--------------------------------------------------" << endl;

    cout << "Testing AxisN assignments --- ";
    inx = 0;
    {
      ArrayAccessor<Int, AxisN> i(cub, AxisN(2));
      ArrayAccessor<Int, AxisN> j(AxisN(1));
      ArrayAccessor<Int, AxisN> k(AxisN(0));
      ArrayAccessor<Int, AxisN> l(AxisN(0));
      for (; i!=i.end(); ++i) {
	for (j = i; j!=j.end(); ++j) {
	  for (k =j; k!=k.end(); ++k) {
	    AlwaysAssertExit(*k == inx);
	    l = k;
	    AlwaysAssertExit(*l == inx++);
	  }
	}
      }
    }
    cout << "ok" << endl <<
      "--------------------------------------------------" << endl;

    cout << "Testing Mixed Axis<> and AxisN assignments --- ";
    inx = 0;
    {
      ArrayAccessor<Int, Axis<2> > i(cub);
      ArrayAccessor<Int, AxisN> j(AxisN(1));
      ArrayAccessor<Int, Axis<0> > k;
      ArrayAccessor<Int, AxisN> l(AxisN(0));
      for (; i!=i.end(); ++i) {
	for (j = i; j!=j.end(); ++j) {
	  for (k =j; k!=k.end(); ++k) {
	    AlwaysAssertExit(*k == inx);
	    l = k;
	    AlwaysAssertExit(*l == inx++);
	  }
	}
      }
    }
    inx = 0;
    {
      ArrayAccessor<Int, AxisN> i(cub, AxisN(2));
      ArrayAccessor<Int, Axis<1> > j;
      ArrayAccessor<Int, AxisN> k(AxisN(0));
      ArrayAccessor<Int, Axis<0> > l;
      for (; i!=i.end(); ++i) {
	for (j = i; j!=j.end(); ++j) {
	  for (k =j; k!=k.end(); ++k) {
	    AlwaysAssertExit(*k == inx);
	    l = k;
	    AlwaysAssertExit(*l == inx++);
	  }
	}
      }
    }
    cout << "ok" << endl <<
      "--------------------------------------------------" << endl;

    cout << "Testing Axis<> init --- ";
    inx = 0;
    {
      ArrayAccessor<Int, Axis<2> > i(cub);
      for (; i!=i.end(); ++i) {
	for (ArrayAccessor<Int, Axis<1> > j(i); j!=j.end(); ++j) {
	  for (ArrayAccessor<Int, Axis<0> > k(j); k!=k.end(); ++k) {
	    AlwaysAssertExit(*k == inx++);
	  }
	}
      }
      inx = 0;
      for (i.init(cub); i!=i.end(); ++i) {
	for (ArrayAccessor<Int, Axis<1> > j(i); j!=j.end(); ++j) {
	  for (ArrayAccessor<Int, Axis<0> > k(j); k!=k.end(); ++k) {
	    AlwaysAssertExit(*k == inx++);
	  }
	}
      }
    }
    cout << "ok" << endl <<
      "--------------------------------------------------" << endl;

    cout << "Testing AxisN init --- ";
    inx = 0;
    {
      ArrayAccessor<Int, AxisN> i(cub, AxisN(2));
      ArrayAccessor<Int, AxisN> j(AxisN(1));
      ArrayAccessor<Int, AxisN> k(AxisN(0));
      for (; i!=i.end(); ++i) {
	for (j = i; j!=j.end(); ++j) {
	  for (k = j; k!=k.end(); ++k) {
	    AlwaysAssertExit(*k == inx++);
	  }
	}
      }
      inx = 0;
      j.init(AxisN(0));
      k.init(AxisN(1));
      for (i.init(cub, AxisN(2)); i!=i.end(); ++i) {
	for (k = i; k!=k.end(); ++k) {
	  for (j = k; j!=j.end(); ++j) {
	    AlwaysAssertExit(*j == inx++);
	  }
	}
      }
    }
    cout << "ok" << endl <<
      "--------------------------------------------------" << endl;

    cout << "Testing Axis<> prev, next, index --- ";
    inx = 0;
    for (ArrayAccessor<Int, Axis<2> > i(cub); i!=i.end(); ++i) {
      for (ArrayAccessor<Int, Axis<1> > j(i); j!=j.end(); ++j) {
	AlwaysAssertExit(*j == inx);
	AlwaysAssertExit(j.next<Axis<0> >() == inx+1);
	AlwaysAssertExit(j.index<Axis<0> >(2) == inx+2);
	AlwaysAssertExit(j.next(AxisN(0)) == inx+1);
	AlwaysAssertExit(j.index(2, AxisN(0)) == inx+2);
	if (j == j.end()-1) {
	  AlwaysAssertExit(j.prev<Axis<0> >() == inx-1);
	  AlwaysAssertExit(j.prev(AxisN(0)) == inx-1);
	}
	inx += N0;
      }
    }
    cout << "ok" << endl <<
      "--------------------------------------------------" << endl;

    cout << "Testing += -= -- ++ --- ";
    inx = 0;
    Int ln = N0*N1;
    for (ArrayAccessor<Int, Axis<2> > i(cub); i!=i.end(-4); ++i) {
      AlwaysAssertExit(*i == inx);
      ++i; AlwaysAssertExit(*i == inx+1*ln);
      i++; AlwaysAssertExit(*i == inx+2*ln);
      --i; AlwaysAssertExit(*i == inx+1*ln);
      i--; AlwaysAssertExit(*i == inx);
      i += 3; AlwaysAssertExit(*i == inx+3*ln);
      i -= 1; AlwaysAssertExit(*i == inx+2*ln);
      i -= 2;
      inx += ln;
    }
    cout << "ok" << endl <<
      "--------------------------------------------------" << endl;

    cout << "Testing dereferencing --- ";
    inx = 0;
    uInt cnt = 0;
    for (ArrayAccessor<Int, Axis<2> > i(cub); i!=i.end(-2); ++i) {
      AlwaysAssertExit(*i == inx);
      AlwaysAssertExit(i.data() == &cub(0,0,cnt));
      AlwaysAssertExit(&(i.baseArray()) == &cub);
      AlwaysAssertExit(i[2] == inx+2*ln);
      AlwaysAssertExit(i.step() == uInt(ln));
      inx += ln;
      ++cnt;
    }
    cout << "ok" << endl <<
      "--------------------------------------------------" << endl;

    cout << "Testing end(), begin(), rbegin(), rend() --- ";
    inx = 0;
    for (ArrayAccessor<Int, Axis<2> > i(cub); i!=i.end(); ++i) {
      AlwaysAssertExit(*i == inx);
      AlwaysAssertExit(i.begin() == &cub(0,0,0));
      AlwaysAssertExit(i.end() == i.begin()+N2*ln);
      AlwaysAssertExit(i.rbegin() == i.end()-ln);
      AlwaysAssertExit(i.rend() == i.begin()-ln);
      AlwaysAssertExit(i.begin(-2) == i.begin()-2*ln);
      AlwaysAssertExit(i.end(-1) == i.begin()+N2*ln-ln);
      AlwaysAssertExit(i.rbegin(+5) == i.end()-ln+5*ln);
      AlwaysAssertExit(i.rend(1) == i.begin());
      inx += ln;
    }
    cout << "ok" << endl <<
      "--------------------------------------------------" << endl;

    cout << "Testing Axis<> and AxisN reset() --- ";
    inx = 0;
    {
      ArrayAccessor<Int, Axis<2> > i(cub);
      for (; i!=i.end(); ++i) {
	AlwaysAssertExit(*i == inx);
	inx += ln;
      }
      inx = 0;
      for (i.reset(); i!=i.end(); ++i) {
	AlwaysAssertExit(*i == inx);
	inx += ln;
      }
      inx = 2*ln;
      for (i.reset(i.begin(2)); i!=i.end(); ++i) {
	AlwaysAssertExit(*i == inx);
	inx += ln;
      }
    }
    {
      inx = 0;
      ArrayAccessor<Int, AxisN> i(cub, AxisN(2));
      for (; i!=i.end(); ++i) {
	AlwaysAssertExit(*i == inx);
	inx += ln;
      }
      inx = 0;
      for (i.reset(); i!=i.end(); ++i) {
	AlwaysAssertExit(*i == inx);
	inx += ln;
      }
      inx = 2*ln;
      for (i.reset(i.begin(2)); i!=i.end(); ++i) {
	AlwaysAssertExit(*i == inx);
	inx += ln;
      }
    }
    cout << "ok" << endl <<
      "--------------------------------------------------" << endl;

    cout << "Testing Axis<> and AxisN split loops --- ";
    inx = 0;
    {
      ArrayAccessor<Int, Axis<2> > i(cub);
      ArrayAccessor<Int, Axis<2> > j;
      for (; i!=i.end(-2); ++i) {
	AlwaysAssertExit(*i == inx);
	inx += ln;
      }
      for (j = i; j!=j.end(); ++j) {
	AlwaysAssertExit(*j == inx);
	inx += ln;
      }
    }
    inx = 0;
    {
      ArrayAccessor<Int, AxisN> i(cub, AxisN(2));
      ArrayAccessor<Int, AxisN> j(AxisN(2));
      for (; i!=i.end(-2); ++i) {
	AlwaysAssertExit(*i == inx);
	inx += ln;
      }
      for (j = i; j!=j.end(); ++j) {
	AlwaysAssertExit(*j == inx);
	inx += ln;
      }
    }
    cout << "ok" << endl <<
      "--------------------------------------------------" << endl;
 
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
    return 1;
  }

  return 0;
}
