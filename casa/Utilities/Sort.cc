//# Sort.cc: Sort on one or more keys, ascending and/or descending
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2000,2001,2003
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

#include <casacore/casa/Utilities/Sort.tcc>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/SortError.h>
#include <casacore/casa/Arrays/Vector.h>
///#include <casacore/casa/Containers/BlockIO.h>

#include <casacore/casa/stdlib.h>                 // for rand
#ifdef _OPENMP
# include <omp.h>
#endif


namespace casacore { //# NAMESPACE CASACORE - BEGIN

SortKey::SortKey (const void* dat, const CountedPtr<BaseCompare>& cmpobj,
                  uInt inc, int opt)
: order_p   (opt),
  data_p    (dat),
  incr_p    (inc),
  ccmpObj_p (cmpobj),
  cmpObj_p  (cmpobj.operator->())
{
    if (order_p != Sort::Descending) {
	order_p = Sort::Ascending;        // make sure order has correct value
    }
}

SortKey::SortKey (const SortKey& that)
: order_p   (that.order_p),
  data_p    (that.data_p),
  incr_p    (that.incr_p),
  ccmpObj_p (that.ccmpObj_p),
  cmpObj_p  (that.cmpObj_p)
{}

SortKey::~SortKey()
{}

SortKey& SortKey::operator= (const SortKey& that)
{
    if (this != &that) {
	order_p   = that.order_p;
	data_p    = that.data_p;
	incr_p    = that.incr_p;
        ccmpObj_p = that.ccmpObj_p;
	cmpObj_p  = that.cmpObj_p;
    }
    return *this;
}

uInt SortKey::tryGenSort (Vector<uInt>& indexVector, uInt nrrec, int opt) const
{
    Sort::Order ord = (order_p < 0  ?  Sort::Ascending : Sort::Descending);
    DataType dtype = cmpObj_p->dataType();
    if (dtype == TpDouble) {
	if (incr_p == sizeof(Double)) {
	    return GenSortIndirect<Double>::sort (indexVector, (Double*)data_p,
						  nrrec, ord, opt);
	}
    } else if (dtype == TpFloat) {
	if (incr_p == sizeof(Float)) {
	    return GenSortIndirect<Float>::sort (indexVector, (Float*)data_p,
						 nrrec, ord, opt);
	}
    } else if (dtype == TpUInt) {
	if (incr_p == sizeof(uInt)) {
	    return GenSortIndirect<uInt>::sort (indexVector, (uInt*)data_p,
						nrrec, ord, opt);
	}
    } else if (dtype == TpInt) {
	if (incr_p == sizeof(Int)) {
	    return GenSortIndirect<Int>::sort (indexVector, (Int*)data_p,
					       nrrec, ord, opt);
	}
    } else if (dtype == TpInt64) {
	if (incr_p == sizeof(Int64)) {
	    return GenSortIndirect<Int64>::sort (indexVector, (Int64*)data_p,
                                                 nrrec, ord, opt);
	}
    } else if (dtype == TpString) {
	if (incr_p == sizeof(String)) {
	    return GenSortIndirect<String>::sort (indexVector, (String*)data_p,
						  nrrec, ord, opt);
	}
    }
    return 0;
}

rownr_t SortKey::tryGenSort (Vector<rownr_t>& indexVector, rownr_t nrrec, int opt) const
{
  /*
    Sort::Order ord = (order_p < 0  ?  Sort::Ascending : Sort::Descending);
    DataType dtype = cmpObj_p->dataType();
    if (dtype == TpDouble) {
	if (incr_p == sizeof(Double)) {
	    return GenSortIndirect<Double>::sort (indexVector, (Double*)data_p,
						  nrrec, ord, opt);
	}
    } else if (dtype == TpFloat) {
	if (incr_p == sizeof(Float)) {
	    return GenSortIndirect<Float>::sort (indexVector, (Float*)data_p,
						 nrrec, ord, opt);
	}
    } else if (dtype == TpUInt) {
	if (incr_p == sizeof(uInt)) {
	    return GenSortIndirect<uInt>::sort (indexVector, (uInt*)data_p,
						nrrec, ord, opt);
	}
    } else if (dtype == TpInt) {
	if (incr_p == sizeof(Int)) {
	    return GenSortIndirect<Int>::sort (indexVector, (Int*)data_p,
					       nrrec, ord, opt);
	}
    } else if (dtype == TpInt64) {
	if (incr_p == sizeof(Int64)) {
	    return GenSortIndirect<Int64>::sort (indexVector, (Int64*)data_p,
                                                 nrrec, ord, opt);
	}
    } else if (dtype == TpString) {
	if (incr_p == sizeof(String)) {
	    return GenSortIndirect<String>::sort (indexVector, (String*)data_p,
						  nrrec, ord, opt);
	}
    }
  */
    return 0;
}




Sort::Sort()
: nrkey_p (0),
  data_p  (0),
  size_p  (0),
  order_p (0)
{}

Sort::Sort (const void* dat, uInt sz)
: nrkey_p (0),
  data_p  (dat),
  size_p  (sz),
  order_p (0)
{}

Sort::Sort (const Sort& that)
: nrkey_p (0),
  data_p  (0),
  size_p  (0),
  order_p (0)
{
    copy (that);
}

Sort::~Sort()
{
    for (uInt i=0; i<nrkey_p; i++) {
	delete keys_p[i];
    }
}

Sort& Sort::operator= (const Sort& that)
{
    if (this != &that) {
        copy (that);
    }
    return *this;
}

void Sort::copy (const Sort& that)
{
    for (uInt i=0; i<nrkey_p; i++) {
	delete keys_p[i];
    }
    nrkey_p = that.nrkey_p;
    keys_p.resize (nrkey_p);
    for (uInt i=0; i<nrkey_p; i++) {
	keys_p = new SortKey (*(that.keys_p[i]));
    }
    data_p  = that.data_p;
    size_p  = that.size_p;
    order_p = that.order_p;
}

void Sort::sortKey (const void* dat, DataType dt, uInt inc, Order ord)
{
    addKey (dat, dt, inc, ord);
}
void Sort::sortKey (const void* dat, const CountedPtr<BaseCompare>& cmp,
                    uInt inc, Order ord)
{
    addKey (new SortKey(dat, cmp, inc, ord));
}
void Sort::sortKey (uInt off, DataType dt, Order ord)
{
    if (data_p == 0) {
	throw SortNoData();
    }
    addKey ((char*)data_p+off, dt, size_p, ord);
}
void Sort::sortKey (uInt off, const CountedPtr<BaseCompare>& cmp, Order ord)
{
    if (data_p == 0) {
	throw SortNoData();
    }
    addKey (new SortKey ((char*)data_p+off, cmp, size_p, ord));
}


void Sort::addKey (const void* dat, DataType dt, uInt inc, int ord)
{
    uInt sz = ValType::getTypeSize (dt);
    if (inc != 0) {
	if (sz > inc) {
	    throw SortInvIncr();
	}
	sz = inc;
    }
    addKey (new SortKey (dat, ValType::getCmpObj(dt), sz, ord));
}


void Sort::addKey (SortKey* key)
{
    if (nrkey_p == 0) {
        order_p = key->order();
    } else if (order_p != key->order()) {
      order_p = 0;    // mixed order
    }
    if (nrkey_p >= keys_p.nelements()) {
	keys_p.resize (keys_p.nelements() + 32);
    }
    keys_p[nrkey_p++] = key;
}


uInt Sort::sort (Vector<uInt>& indexVector, uInt nrrec,
                 int options, Bool tryGenSort) const
  { return doSort (indexVector, nrrec, options, tryGenSort); }

uInt64 Sort::sort (Vector<uInt64>& indexVector, uInt64 nrrec,
                   int options, Bool tryGenSort) const
  { return doSort (indexVector, nrrec, options, tryGenSort); }

uInt Sort::unique (Vector<uInt>& uniqueVector, uInt nrrec) const
  { return doUnique (uniqueVector, nrrec); }

uInt Sort::unique (Vector<uInt>& uniqueVector,
                   const Vector<uInt>& indexVector) const
  { return doUnique (uniqueVector, indexVector); }

uInt64 Sort::unique (Vector<uInt64>& uniqueVector, uInt64 nrrec) const
  { return doUnique (uniqueVector, nrrec); }

uInt64 Sort::unique (Vector<uInt64>& uniqueVector,
                     const Vector<uInt64>& indexVector) const
  { return doUnique (uniqueVector, indexVector); }
    // </group>

} //# NAMESPACE CASACORE - END

