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

//# Includes
#include <casacore/scimath/Fitting/LSQMatrix.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/casa/Containers/RecordFieldId.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // Constants
  const String LSQMatrix::tmatsiz = String("tmatsiz");
  const String LSQMatrix::tmatdat = String("tmatdat");

  bool LSQMatrix::fromRecord(String &error, const RecordInterface &in) {
    set(0);
    int32_t vlen;
    if (in.isDefined(tmatsiz) &&
	in.type(in.idToNumber(RecordFieldId(tmatsiz))) == TpInt) {
      in.get(RecordFieldId(tmatsiz), vlen);
    } else {
      error += String("No triangular matrix length present");
      return false;
    }
    set(vlen);
    return getCArray(error, in, tmatdat, len_p, trian_p);
  }
  
  bool LSQMatrix::toRecord(String &error, RecordInterface &out) const {
    out.define(RecordFieldId(tmatsiz), static_cast<int32_t>(n_p));
    if (n_p) return putCArray(error, out, tmatdat, len_p, trian_p);
    return true;
  }
  
  const String &LSQMatrix::ident() const {
    static String myid = "tmat";
    return myid;
  }

  bool LSQMatrix::putCArray(String &error, RecordInterface &out,
			 const String &fname,
			 uint32_t len, const double * const in) {
    if (len) {
      if (in) {
	Vector<double> vt(len);
	std::copy(in, in+len, vt.data()); 
	out.define(RecordFieldId(fname), vt);
      } else {
	error += String("No data for non-empty ") + fname + "vector";
	return false;
      }
    }
    return true;
  }

  bool LSQMatrix::getCArray(String &error, const RecordInterface &in,
			 const String &fname,
			 uint32_t len, double *&out) {
    if (in.isDefined(fname) &&
	in.type(in.idToNumber(RecordFieldId(fname))) == TpArrayDouble) {
      Vector<double> vt;
      in.get(RecordFieldId(fname), vt);
      uint32_t vlen = vt.nelements();
      if (!out) out = new double[vlen];
      if (len && vlen != len) {
	error += String("Inconsistency between lengths in " + fname +
			"field in record");
	return false;
      }
      std::copy(vt.data(), vt.data()+len, out); 
    }
    return true;
  }

  bool LSQMatrix::putCArray(String &error, RecordInterface &out,
			 const String &fname,
			 uint32_t len, const uint32_t * const in) {
    if (len) {
      if (in) {
	Vector<int32_t> vt(len);
	std::copy(in, in+len, vt.data()); 
	out.define(RecordFieldId(fname), vt);
      } else {
	error += String("No data for non-empty ") + fname + "vector";
	return false;
      }
    }
    return true;
  }

  bool LSQMatrix::getCArray(String &error, const RecordInterface &in,
			 const String &fname,
			 uint32_t len, uint32_t *&out) {
    if (in.isDefined(fname) &&
	in.type(in.idToNumber(RecordFieldId(fname))) == TpArrayInt) {
      Vector<int32_t> vt;
      in.get(RecordFieldId(fname), vt);
      uint32_t vlen = vt.nelements();
      if (!out) out = new uint32_t[vlen];
      if (len && vlen != len) {
	error += String("Inconsistency between lengths in " + fname +
			"field in record");
	return false;
      }
      std::copy(vt.data(), vt.data()+len, out); 
    }
    return true;
  }


  void LSQMatrix::toAipsIO (AipsIO& out) const
  {
    out << n_p;
    if (n_p) putCArray(out, len_p, trian_p);
  }

  void LSQMatrix::fromAipsIO (AipsIO& in)
  {
    set(0);
    uint32_t n;
    in >> n;
    set(n);
    if (n > 0) getCArray (in, len_p, trian_p);
  }

  void LSQMatrix::putCArray (AipsIO& out, uint32_t len, const double* const in)
  {
    if (in) {
      out << true;
      out.put (len, in);
    } else {
      out << false;
    }
  }

  void LSQMatrix::getCArray (AipsIO& in, uint32_t len, double*& out)
  {
    bool flag;
    in >> flag;
    if (flag) {
      uint32_t vlen;
      in >> vlen;
      if (vlen > 0) {
	if (!out) out = new double[vlen];
	AlwaysAssert (vlen == len, AipsError);
	in.get (len, out);
      }
    }
  }

  void LSQMatrix::putCArray (AipsIO& out, uint32_t len, const uint32_t* const in)
  {
    if (in) {
      out << true;
      out.put (len, in);
    } else {
      out << false;
    }
  }

  void LSQMatrix::getCArray (AipsIO& in, uint32_t len, uint32_t*& out)
  {
    bool flag;
    in >> flag;
    if (flag) {
      uint32_t vlen;
      in >> vlen;
      if (vlen > 0) {
	if (!out) out = new uint32_t[vlen];
	AlwaysAssert (vlen == len, AipsError);
	in.get (len, out);
      }
    }
  }

} //# NAMESPACE CASACORE - END

