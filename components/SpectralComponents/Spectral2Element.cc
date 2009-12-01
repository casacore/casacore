//# Spectral2Element.cc: Record conversion for SpectralElement
//# Copyright (C) 2001,2004
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

#include <casa/Arrays/ArrayMath.h>
#include <casa/Containers/RecordInterface.h>
#include <casa/Containers/RecordFieldId.h>
#include <casa/Containers/Record.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicSL/String.h>
#include <casa/Utilities/DataType.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Member functions 
Bool SpectralElement::toRecord(String &, RecordInterface &out) const {
  out.define(RecordFieldId("type"), fromType(tp_p));

  Vector<Double> ptmp(par_p.copy());
  Vector<Double> etmp(err_p.copy());

  if (tp_p == GAUSSIAN) {
     ptmp(2) = sigmaToFWHM(par_p(2));
     etmp(2) = sigmaToFWHM(err_p(2));
  };

  out.define(RecordFieldId("parameters"), ptmp);  
  out.define(RecordFieldId("errors"), etmp);
  if (tp_p == COMPILED) {
    out.define(RecordFieldId("compiled"), str_p);
  };

  return True;
}

Bool SpectralElement::fromRecord(String &error, const RecordInterface &in) {
  if (in.isDefined("type") && in.isDefined("parameters") &&
      in.type(in.idToNumber(RecordFieldId("type"))) == TpString) {
    String stp;
    SpectralElement::Types tp;
    in.get(RecordFieldId("type"), stp);
    if (!SpectralElement::toType(tp, stp)) {
      error += String("Unknown spectral type in SpectralElement::fromRecord\n");
      return False;
    }

    // Accomodate possibility of record coming from Glish in any old type

    Vector<Double> errs;

    // Get the errors if defined in record

    if (in.isDefined("errors")) {
      DataType dataType = in.dataType("errors");
      if (dataType == TpArrayDouble) {
	in.get("errors", errs);
      } else if (dataType == TpArrayFloat) {
	Vector<Float> v;
	in.get("errors", v);
	errs.resize(v.nelements());
	convertArray(errs, v);
      } else if (dataType == TpArrayInt) {
	Vector<Int> v;
	in.get("errors", v);
	errs.resize(v.nelements());
	convertArray(errs, v);
      } else {
	error += String("SpectralElement::fromRecord: errors field "
			"must be double, float or int\n");
	return False;
      };
    };

    Vector<Double> param;
    DataType dataType = in.dataType("parameters");
    if (dataType == TpArrayDouble) {
       in.get("parameters", param);
    } else if (dataType == TpArrayFloat) {
       Vector<Float> v;
       in.get("parameters", v);
       param.resize(v.nelements());
       convertArray(param, v);
    } else if (dataType == TpArrayInt) {
       Vector<Int> v;
       in.get("parameters", v);
       param.resize(v.nelements());
       convertArray(param, v);
    } else {
      error += String("SpectralElement::fromRecord: parameters field "
		      "must be double, float or int\n");
      return False;
    };

    // Make sizes of errors and params equal
    if (errs.nelements() == 0) {
      errs.resize(param.nelements());
      errs = 0.0;
    };
    if (errs.nelements() != param.nelements()) {
      error += String("SpectralElement::fromRecord must have equal lengths "
		      "for parameters and errors fields\n");
    };
    if (tp == GAUSSIAN) {
      if (param.nelements() != 3) {
	error += String("Illegal number of parameters for Gaussian element\n");
	return False;
      };
      if (param(2) <= 0.0) {
	error += String("The width of a Gaussian element must be positive\n");
	return False;
      };
      param(2) = sigmaFromFWHM (param(2));
      errs(2) = sigmaFromFWHM (errs(2));
      par_p.resize(3);
      err_p.resize(3);
    } else if (tp == POLYNOMIAL) {
      if (param.nelements() == 0) {
	error += String("Polynomial spectral element must have order "
			"of at least zero\n");
	return False;
      };
      n_p = param.nelements()-1;
      par_p.resize(n_p+1);
      err_p.resize(n_p+1);
    } else if (tp == COMPILED) {
      if (in.isDefined("compiled") && 
	  in.type(in.idToNumber(RecordFieldId("compiled"))) == TpString) {
	in.get(RecordFieldId("compiled"), str_p);
	uInt n = param.nelements();
	par_p.resize(n);
	err_p.resize(n);
      } else {
	error += String("No compiled string in SpectralElement::fromRecord\n");
      };
    };
    par_p = param;
    err_p = errs;
    tp_p = tp;

   return True;
  };
  error += String("Illegal spectral element record in "
		  "SpectralElement::fromRecord\n");
  return False;
  }

Bool SpectralElement::fromString(String &error, const String &in) {
  SpectralElement::Types tp;
  if (!SpectralElement::toType(tp, in)) {
    error += String("Unknown spectral type in SpectralElement::fromString\n");
    return False;
  };
  if (tp == GAUSSIAN) *this = SpectralElement();
  else if (tp_p == POLYNOMIAL)  *this = SpectralElement(0);
  else if (tp_p == COMPILED)  *this = SpectralElement(String("0"),
						      Vector<Double>());
  return True;
}

} //# NAMESPACE CASA - END

