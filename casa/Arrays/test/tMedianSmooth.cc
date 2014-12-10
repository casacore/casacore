//# ArrayMath.cc: Arithmetic functions defined on Arrays
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2001,2003
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

#include <iostream>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayPartMath.h>

int main()
{
  const size_t specsize=50;
  const int width=1;
  casacore::IPosition box(1,width);
  casacore::Vector<float> input(specsize);
  for(size_t i=0;i<specsize;i++) input[i]=i%5+(i/5)*0.01;
  casacore::Vector<float> medians = slidingArrayMath(input, box, casacore::MedianFunc<float>(false,true,false));
  std::cout << "Input = ";
  for(size_t i=0;i<specsize;i++) std::cout << input[i]<<" ";
  std::cout << "\nMedians = ";
  for(size_t i=0;i<specsize;i++) std::cout << medians[i]<<" ";
  std::cout << "\n";

}
