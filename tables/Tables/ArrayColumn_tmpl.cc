//# ArrayColumn_tmpl.cc: Explicit ArrayColumn template instantiations
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
//# $Id: Vector.h 21545 2015-01-22 19:36:35Z gervandiepen $

//# Includes
#include <casacore/tables/Tables/ArrayColumn.h>

//# Instantiate extern templates for often used types.
namespace casacore {
  template class ArrayColumn<Bool>;
  template class ArrayColumn<Char>;
  template class ArrayColumn<Short>;
  template class ArrayColumn<uShort>;
  template class ArrayColumn<Int>;
  template class ArrayColumn<uInt>;
  template class ArrayColumn<Float>;
  template class ArrayColumn<Double>;
  template class ArrayColumn<Complex>;
  template class ArrayColumn<DComplex>;
  template class ArrayColumn<String>;
}
