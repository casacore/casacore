//# IComplex.h: Integer complex number
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


#ifndef CASA_ICOMPLEX_H
#define CASA_ICOMPLEX_H

//# Includes

#include <casacore/casa/aips.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Integer complex numbers.
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <synopsis>
// The class <src>IComplex</src> is only a container for FITS usage.
// </synopsis>

class IComplex {
public:
  //# Constructors
  // From one or two ints (note for gnu use)
  // <group>
  IComplex() : re(0), im(0) {;};
  IComplex(Int r) : re(r), im(0) {;};
  IComplex(Int r, Int i) : re(r), im(i) {;};
  // </group>

  //# Member functions
  // For use in FITS classes only
  // <group>
  Int real() const { return re; };
  Int imag() const { return im; };
  // </group>

private:
  Int re;
  Int im;
};


// Show on ostream.
ostream &operator<< (ostream &os, const IComplex&);



} //# NAMESPACE CASACORE - END

#endif
