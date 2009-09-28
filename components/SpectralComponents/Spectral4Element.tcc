//# Spectral4Element.cc: Member templates for SpectralElement
//# Copyright (C) 2001,2002,2004
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

//# Includes
#include <components/SpectralComponents/SpectralElement.h>

#include <casa/Exceptions/Error.h>
#include <casa/BasicSL/String.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Member templates
template <class MT>
void SpectralElement::set(SpectralElement::Types tp,
			  const Vector<MT> &param) {
  tp_p = tp;
  n_p = 0;
  uInt n = 0;
  if (tp_p == GAUSSIAN) {
    if (param.nelements() != 3) {
      throw(AipsError("SpectralElement: GAUSSIAN must have "
		      "3 parameters"));
    };
    n = 3;
  } else if (tp_p == POLYNOMIAL) {
    if (param.nelements() == 0) {
      throw(AipsError("SpectralElement: POLYNOMIAL must have "
		      "at least 1 parameter"));
    };
    n_p = param.nelements()-1;
    n = n_p+1;
  } else if (tp_p == COMPILED) {
    n = param.nelements();
  };
  par_p.resize(n);
  err_p.resize(n);
  fix_p.resize(n);
  for (uInt i=0; i<par_p.nelements(); i++) par_p(i) = param(i);
  err_p = 0;
  fix_p = False;
  check();
}

template <class MT>
void SpectralElement::set(const Vector<MT> &param) {
    if (param.nelements() != par_p.nelements()) {
      throw(AipsError("SpectralElement: setting incorrect number of "
		      "parameters in the element"));
    };
    for (uInt i=0; i<par_p.nelements(); i++) par_p(i) = param(i);
}

} //# NAMESPACE CASA - END

