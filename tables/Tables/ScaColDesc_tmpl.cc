//# ScaColDesc_tmpl.cc: Explicit ScalarColumnDesc template instantiations
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
#include <casacore/tables/Tables/ScaColDesc.h>

//# Instantiate extern templates for often used types.
namespace casacore {
  template class ScalarColumnDesc<Bool>;
  template class ScalarColumnDesc<Char>;
  template class ScalarColumnDesc<Short>;
  template class ScalarColumnDesc<uShort>;
  template class ScalarColumnDesc<Int>;
  template class ScalarColumnDesc<uInt>;
  template class ScalarColumnDesc<Float>;
  template class ScalarColumnDesc<Double>;
  template class ScalarColumnDesc<Complex>;
  template class ScalarColumnDesc<DComplex>;
  template class ScalarColumnDesc<String>;
}
