//# LSQMatrix2.cc: Support class for the LSQ package -- Record from/to
//# Copyright (C) 2005
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
#include <scimath/Fitting/LSQMatrix.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Arrays/Vector.h>
#include <casa/Containers/RecordInterface.h>
#include <casa/Containers/RecordFieldId.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  Bool LSQMatrix::fromRecord(String &error, const RecordInterface &in) {
    set(0);
    Int vlen;
    if (in.isDefined(String("tmatsiz")) &&
	in.type(in.idToNumber(RecordFieldId("tmatsiz"))) == TpInt) {
      in.get(RecordFieldId("tmatsiz"), vlen);
    } else {
      error += String("No triangular matrix length present");
      return False;
    };
    set(vlen);
    if (vlen && in.isDefined(String("tmatdat")) &&
	in.type(in.idToNumber(RecordFieldId("tmatdat"))) == TpArrayDouble) {
      Vector<Double> vt;
      in.get(RecordFieldId("tmatdat"), vt);
      if (len_p != vt.nelements()) {
	error += String("Inconsistency between lengths in "
			"triangular matrix record");
	set(0);
	return False;
      };
      std::copy(vt.data(), vt.data()+len_p, trian_p); 
    } else {
      error += String("No triangular data present in non-empty matrix");
      return False;
    };
    return True;
  }
  
  Bool LSQMatrix::toRecord(String &error, RecordInterface &out) const {
    out.define(RecordFieldId("tmatsiz"), static_cast<Int>(n_p));
    if (n_p) {
      if (trian_p) {
	const IPosition vlen(1, len_p);
	Vector<Double> vt(vlen, trian_p, COPY);
	out.define(RecordFieldId("tmatdat"), vt);
      } else {
	error += String("No triangular fitting data for non-empty matrix");
	return False;
      };
    };
    return True;
  }
  
  const String &LSQMatrix::ident() const {
    static String myid = "tmat";
    return myid;
  }

} //# NAMESPACE CASA - END

