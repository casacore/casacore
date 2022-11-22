//# ArrColDesc_tmpl.cc: Explicit ArrayColumnDesc template instantiations
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

//# Includes
#include <casacore/tables/Tables/ArrColDesc.h>

//# Instantiate extern templates for often used types.
namespace casacore {
  template class ArrayColumnDesc<Bool>;
  template class ArrayColumnDesc<Char>;
  template class ArrayColumnDesc<Short>;
  template class ArrayColumnDesc<uShort>;
  template class ArrayColumnDesc<Int>;
  template class ArrayColumnDesc<uInt>;
  template class ArrayColumnDesc<Int64>;
  template class ArrayColumnDesc<Float>;
  template class ArrayColumnDesc<Double>;
  template class ArrayColumnDesc<Complex>;
  template class ArrayColumnDesc<DComplex>;
  template class ArrayColumnDesc<String>;
}
