//# SimOrdMap.cc: Simple map with ordered keys
//# Copyright (C) 1993,1994,1995
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

#ifndef CASA_SIMORDMAP_TCC
#define CASA_SIMORDMAP_TCC

#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class K, class V>
SimpleOrderedMap<K,V>::SimpleOrderedMap (const V& dflt, uInt incr)
: kvblk(incr),
  nrused(0),
  nrincr(incr),
  DefaultVal(dflt)
{
    if (nrincr < 16) {
	nrincr = 16;
    }
}

template<class K, class V>
SimpleOrderedMap<K,V>::SimpleOrderedMap (const V& dflt)
: kvblk (16),
  nrused(0),
  nrincr(16),
  DefaultVal(dflt)
{}

template<class K, class V>
SimpleOrderedMap<K,V>::SimpleOrderedMap
                           (const SimpleOrderedMap<K,V>& that)
: kvblk (that.kvblk.nelements()),
  nrused(that.nrused),
  nrincr(that.nrincr),
  DefaultVal(that.DefaultVal)
{
    copyBlock (that);
}

template<class K, class V>
SimpleOrderedMap<K,V>::~SimpleOrderedMap ()
{
    clear();
}

template<class K, class V>
void SimpleOrderedMap<K,V>::clear ()
{
    for (uInt i=0; i<nrused; i++) {
        delete KVBLKpair(i);
    }
    nrused = 0;
}

template<class K, class V>
void SimpleOrderedMap<K,V>::copyBlock (const SimpleOrderedMap<K,V>& that)
{
    for (uInt i=0; i<nrused; i++) {
        kvblk[i] = new OrderedPair<K,V> (that.getKey(i), that.getVal(i));
    }
}


template<class K, class V>
SimpleOrderedMap<K,V>& SimpleOrderedMap<K,V>::operator=
                                   (const SimpleOrderedMap<K,V>& that)
{
    if (&that == this) {
        return *this;
    }
    clear();
    if (kvblk.nelements() < that.nrused) {
        kvblk.resize (that.nrused, False, False);
    }
    nrused = that.nrused;
    copyBlock (that);
    return *this;
}


template<class K, class V>
uInt SimpleOrderedMap<K,V>::findKey (const K& k, Bool& defined) const
{
    //  Do a binary search for the key. Return the index of the first
    //  key which is >= the key searched for.
    //  In case of insertion this gives the index where to insert.
    Int st = 0;
    Int ent= nrused-1;
    Int i  = 0;
    defined = False;
    while (st<=ent) {
        i = (st+ent)/2;
        if (k < KVBLKpair(i)->x()) {
            ent = i-1;
        }else{
	    if (k > KVBLKpair(i)->x()) {
                i++;
                st = i;
            }else{
                defined = True;
                ent     = -1;
            }
        }
    }
    return (i);
}


template<class K, class V>
V *SimpleOrderedMap<K,V>::isDefined (const K& k)
{
    //  Locate the key. Error if the key already exists.
    Bool defined;
    uInt inx = findKey (k, defined);
    if (defined) {
        return(&(KVBLKpair(inx)->y()));
    } else {
        return 0;
    }
}

template<class K, class V>
V &SimpleOrderedMap<K,V>::operator()(const K &ky)
{
    V *vptr = isDefined(ky);
    return vptr  ?  *vptr : define(ky,DefaultVal);
}

template<class K, class V>
const V &SimpleOrderedMap<K,V>::operator()(const K &ky) const
{
    const V *vptr = isDefined(ky);
    if (! vptr)
	throw(indexError<K>(ky, "SimpleOrderedMap-operator()"));
    return *vptr;
}

template<class K, class V>
V &SimpleOrderedMap<K,V>::define (const K& k, const V& v)
{
    //  Locate the key. Error if the key already exists.
    Bool defined;
    uInt inx = findKey (k, defined);
    if (defined) {
        delete KVBLKpair(inx);
    }else{
        //  Extend the blocks if full.
        if (nrused==kvblk.nelements()) {
	    kvblk.resize (kvblk.nelements()+nrincr);
        }
        //  Shift the keys and values to the right and insert the new ones.
        for (uInt i=nrused; i>inx; i--) {
	    kvblk[i] = kvblk[i-1];
        }
        nrused++;
    }
    kvblk[inx] = new OrderedPair<K,V> (k,v);
    return(KVBLKpair(inx)->y());
}

template<class K, class V>
void SimpleOrderedMap<K,V>::remove (const K& k)
{
    //  Locate the key. Error if the key does not exist.
    Bool defined;
    uInt inx = findKey (k, defined);
    if (!defined) {
	throw(indexError<K>(k, "SimpleOrderedMap-remove"));
    }else{
        //  Remove the key and value and shift the others to the left.
        delete KVBLKpair(inx);
        nrused--;
        for (uInt i=inx; i<nrused; i++) {
            kvblk[i] = kvblk[i+1];
        }
    }
}

template<class K, class V>
void SimpleOrderedMap<K,V>::rename (const K& keynew, const K& keyold)
{
    //  Locate the old key. Error if the key does not exist.
    Bool defined;
    uInt inxold = findKey (keyold, defined);
    if (!defined) {
	throw(indexError<K>(keyold, "SimpleOrderedMap-rename"));
    }
    //  Locate the new key.
    //  Exit if the keys are equal.
    //  Remove if the key already exists (adjust index if needed).
    uInt inxnew = findKey (keynew, defined);
    if (defined) {
	if (inxold == inxnew)
	    return;
	remove (keynew);
	if (inxold >= inxnew)
	    inxold--;
    }
    // Save pointer to key/value.
    // If the new key is right of the old key, make index one less
    // (since the old key will be removed).
    OrderedPair<K,V>* p = KVBLKpair(inxold);
    if (inxnew > inxold)
	inxnew--;
    // Shift key/value pointers to the right or left (as far as needed).
    uInt i;
    for (i=inxold; i>inxnew; i--)
	kvblk[i] = kvblk[i-1];
    for (i=inxold; i<inxnew; i++)
	kvblk[i] = kvblk[i+1];
    // Now insert the new key value and the pointer.
    p->x() = keynew;
    kvblk[inxnew] = p;
}


//# Check if the invariants still hold.
template<class K, class V>
Bool SimpleOrderedMap<K,V>::ok() const
{
    if (ntot() < nrused)
	return False;
    for (uInt i=1; i<nrused; i++) {
	if (KVBLKpair(i)->x() <= KVBLKpair(i-1)->x())
	    return False;
    }
    return True;
}

} //# NAMESPACE CASACORE - END


#endif
