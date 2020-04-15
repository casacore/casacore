//# Array_tmpl.cc: Explicit Array template instantiations
//# Copyright (C) 2015
//# Associated Universities, Inc. Washington DC, USA,
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
//# $Id: Array.h 21545 2015-01-22 19:36:35Z gervandiepen $

//# Includes
#include "Array.h"
#include "ArrayMath.h"
#include "MaskedArray.h"

//# Instantiate extern templates for often used types.
namespace casacore {
  template class Array<bool, std::allocator<bool>>;
  template class Array<char, std::allocator<char>>;
  template class Array<unsigned char, std::allocator<unsigned char>>;
  template class Array<short, std::allocator<short>>;
  template class Array<unsigned short, std::allocator<unsigned short>>;
  template class Array<int, std::allocator<int>>;
  template class Array<unsigned int, std::allocator<unsigned int>>;
  template class Array<long long, std::allocator<long long>>;
  template class Array<float, std::allocator<float>>;
  template class Array<double, std::allocator<double>>;
}
