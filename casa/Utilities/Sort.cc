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

#include <casa/Utilities/Sort.h>
#include <casa/Utilities/GenSort.h>
#include <casa/Utilities/ValType.h>
#include <casa/Utilities/Copy.h>
#include <casa/BasicSL/String.h>
#include <casa/Utilities/SortError.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayMath.h>

#include <casa/stdlib.h>                 // for rand



namespace casa { //# NAMESPACE CASA - BEGIN

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




Sort::Sort()
: nrkey_p (0),
  data_p  (0),
  size_p  (0)
{}

Sort::Sort (const void* dat, uInt sz)
: nrkey_p (0),
  data_p  (dat),
  size_p  (sz)
{}

Sort::Sort (const Sort& that)
: nrkey_p (0),
  data_p  (0),
  size_p  (0)
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
    uInt i;
    for (i=0; i<nrkey_p; i++) {
	delete keys_p[i];
    }
    nrkey_p = that.nrkey_p;
    keys_p.resize (nrkey_p);
    for (i=0; i<nrkey_p; i++) {
	keys_p = new SortKey (*(that.keys_p[i]));
    }
    data_p = that.data_p;
    size_p = that.size_p;
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
    if (nrkey_p >= keys_p.nelements()) {
	keys_p.resize (keys_p.nelements() + 32);
    }
    keys_p[nrkey_p++] = key;
}


uInt Sort::unique (Vector<uInt>& uniqueVector, uInt nrrec) const
{
    Vector<uInt> indexVector(nrrec);
    indgen (indexVector);
    return unique (uniqueVector, indexVector);
}

uInt Sort::unique (Vector<uInt>& uniqueVector,
		   const Vector<uInt>& indexVector) const
{
    uInt nrrec = indexVector.nelements();
    uniqueVector.resize (nrrec);
    if (nrrec == 0) {
        return 0;
    }
    // Pass the sort function a C-array of indices, because indexing
    // in there is (much) faster than in a vector.
    Bool delInx, delUniq;
    const uInt* inx = indexVector.getStorage (delInx);
    uInt* uniq = uniqueVector.getStorage (delUniq);
    uniq[0] = 0;
    uInt nruniq = 1;
    for (uInt i=1; i<nrrec; i++) {
        Int cmp = compare (inx[i-1], inx[i]);
	if (cmp != 1  &&  cmp != -1) {
	    uniq[nruniq++] = i;
	}
    }
    indexVector.freeStorage (inx, delInx);
    uniqueVector.putStorage (uniq, delUniq);
    if (nruniq < nrrec) {
        uniqueVector.resize (nruniq, True);
    }
    return nruniq;
}


uInt Sort::sort (Vector<uInt>& indexVector, uInt nrrec, int opt) const
{
    //# Try if we can use the faster GenSort when we have one key only.
    if (nrkey_p == 1) {
	uInt n = keys_p[0]->tryGenSort (indexVector, nrrec, opt);
	if (n > 0) {
	    return n;
	}
    }
    indexVector.resize (nrrec);
    indgen (indexVector);
    // Pass the sort function a C-array of indices, because indexing
    // in there is (much) faster than in a vector.
    Bool del;
    uInt* inx = indexVector.getStorage (del);
    // Choose the sort required.
    int nodup = opt & NoDuplicates;
    int type  = opt - nodup;
    uInt n = 0;
    switch (type) {
    case QuickSort:
	if (nodup) {
	    n = quickSortNoDup (nrrec, inx);
	}else{
	    n = quickSort (nrrec, inx);
	}
	break;
    case HeapSort:
	if (nodup) {
	    n = heapSortNoDup (nrrec, inx);
	}else{
	    n = heapSort (nrrec,inx);
	}
	break;
    case InsSort:
	if (nodup) {
	    n = insSortNoDup (nrrec, inx);
	}else{
	    n = insSort (nrrec, inx);
	}
	break;
    default:
	throw SortInvOpt();
    }
    indexVector.putStorage (inx, del);
    // If n < nrrec, some duplicates have been deleted.
    // This means we have to resize the Vector.
    if (n < nrrec) {
	indexVector.resize (n, True);
    }
    return n;
}


uInt Sort::insSort (uInt nrrec, uInt* inx) const
{
    Int  j;
    uInt cur;
    for (uInt i=1; i<nrrec; i++) {
	j   = i;
	cur = inx[i];
	while (--j>=0  &&  compare(inx[j], cur) <= 0) {
	    inx[j+1] = inx[j];
	}
	inx[j+1] = cur;
    }
    return nrrec;
}

uInt Sort::insSortNoDup (uInt nrrec, uInt* inx) const
{
    if (nrrec < 2) {
	return nrrec;                             // nothing to sort
    }
    Int  j, k;
    uInt cur;
    uInt nr = 1;
    int  cmp = 0;
    for (uInt i=1; i<nrrec; i++) {
	j   = nr;
	cur = inx[i];
	// Continue as long as key is out of order.
	while (--j>=0  &&  (cmp = compare (inx[j], cur)) == 0) {
	}
	if (j<0 || cmp==2) {                    // no equal key
	    for (k=nr-1; k>j; k--) {
		inx[k+1] = inx[k];              // now shift to right
	    }
	    inx[j+1] = cur;                     // insert in right place
	    nr++;
	}
    }
    return nr;
}


uInt Sort::quickSort (uInt nrrec, uInt* inx) const
{
    // Use the quicksort algorithm and improvements as described
    // in "Algorithms in C" by R. Sedgewick.
    // Small subsets are not sorted with qksort anymore, but
    // thereafter with insertion sort.
    qkSort (nrrec, inx);
    return insSort (nrrec, inx);
}

uInt Sort::quickSortNoDup (uInt nrrec, uInt* inx) const
{
    qkSort (nrrec, inx);
    return insSortNoDup (nrrec, inx);
}


void Sort::qkSort (Int nr, uInt* inx) const
{
    // If the nr of elements to be sorted is less than N, it is
    // better not to use quicksort anymore (according to Sedgewick).
    // Take N=15, because that seems to work best after testing
    // N=5, 10, 15 and 20.
    if (nr <= 15) {
	return;
    }
    // According to Sedgewick it is best to use a random partition element
    // to avoid degenerated cases (if the data is already in order for example)
    // rand is not a particularly good random number generator, but good
    // enough for this purpose.
    // Put this element at the beginning of the array.
    Int p = rand() % nr;
    swap (0, p, inx);
    // Now shift all elements < partition-element to the left.
    // If an element is equal, shift every other element to avoid
    // degeneration. This trick is described by Jon Bentley in
    // UNIX Review, October 1992.
    // We do not have equal elements anymore (because of the stability
    // property introduced on 13-Feb-1995).
    Int j = 0;
    for (Int i=1; i<nr; i++) {
	if (compare (inx[0], inx[i]) <= 0) {
	    swap (i, ++j, inx);
	}
    }
    swap (0, j, inx);
    qkSort (j, inx);
    qkSort (nr-j-1, inx+j+1);
}


uInt Sort::heapSort (uInt nrrec, uInt* inx) const
{
    // Use the heapsort algorithm described by Jon Bentley in
    // UNIX Review, August 1992.
    Int j;
    inx--;
    for (j=nrrec/2; j>=1; j--) {
	siftDown (j, nrrec, inx);
    }
    for (j=nrrec; j>=2; j--) {
	swap (1, j, inx);
	siftDown (1, j-1, inx);
    }
    return nrrec;
}

uInt Sort::heapSortNoDup (uInt nrrec, uInt* inx) const
{
    heapSort (nrrec, inx);
    return insSortNoDup (nrrec, inx);
}

void Sort::siftDown (Int low, Int up, uInt* inx) const
{
    uInt sav = inx[low];
    Int c;
    Int i;
    for (i=low; (c=2*i)<=up; i=c) {
	if (c < up  &&  compare(inx[c+1], inx[c]) <= 0) {
	    c++;
	}
	inx[i] = inx[c];
    }
    inx[i] = sav;
    for ( ; (c=i/2)>=low; i=c) {
	if (compare (inx[i], inx[c]) > 0) {
	    break;
	}
	swap (c, i, inx);
    }
}


// Note that the block of SortKeys is defined as void*, to achieve
// that only 1 type of Block<pointer> is needed.
// Casting is perfectly save.
// The comparison functions return:
//   -1   when obj1 < obj2
//    0   when obj1 = obj2
//    1   when obj1 > obj2
// compare returns:
//    2   when data[i1],data[i2] is in correct order
//        (thus data[i1] < data[i2] for ascending sort)
//    1   when data is equal and indices are in order
//    0   when data is out of order
//   -1   when data is equal and indices are out of order
int Sort::compare (uInt i1, uInt i2) const
{
    int seq;
    SortKey* skp;
    for (uInt i=0; i<nrkey_p; i++) {
	skp = keys_p[i];
        seq = skp->cmpObj_p->comp ((char*)skp->data_p + i1*skp->incr_p,
                                   (char*)skp->data_p + i2*skp->incr_p);
	if (seq == skp->order_p)
	    return 2;                       // in order
	if (seq != 0) {
	    return 0;                       // out-of-order
	}
    }
    // Equal keys, so return i1<i2 to maintain stability.
    if (i1<i2)
	return 1;                           // equal keys; in order
    return -1;                              // equal keys; out-of-order
}

} //# NAMESPACE CASA - END

