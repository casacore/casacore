//# HashMap.cc: this defines HashMap, which is a hashed associative array
//# Copyright (C) 1995,1996,1998,1999,2000
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

#ifndef CASA_HASHMAP_TCC
#define CASA_HASHMAP_TCC

#include <casacore/casa/Containers/HashMap.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class key> HashClass<key>::HashClass() { }
template<class key> HashClass<key>::~HashClass() { }

template<class key, class val> HashMap<key,val>::HashMap(const HashMap<key,val> &other)
: total_(other.total_),
  used_(other.used_),
  filled_(other.filled_),
  defs_(other.defs_),
  maxLoad_(other.maxLoad_),
  blk(other.blk),
  func(other.func),
  hashClass(other.hashClass ? other.hashClass->clone() : 0),
  dfltVal(other.dfltVal)
{
    for (uInt i=0; i < blk.nelements(); i++)
        if (blk[i])
	    blk[i] = new List<OrderedPair<key,val> >(blk[i]);
}

template<class key, class val> 
HashMap<key,val> &HashMap<key,val>::operator=(const HashMap<key,val> &other) {
    if ( this == &other )
	return *this;
    freeTable();
    blk = other.blk;
    func = other.func;
    used_ = other.used_;
    total_ = other.total_;
    defs_ = other.defs_;
    filled_ = other.filled_;
    maxLoad_ = other.maxLoad_;
    hashClass = other.hashClass ? other.hashClass->clone() : 0;
    dfltVal = other.dfltVal;
    for (uInt i=0; i < blk.nelements(); i++)
	if (blk[i])
	    blk[i] = new List<OrderedPair<key,val> >(blk[i]);
    return *this;
}

template<class key, class val> void HashMap<key,val>::freeTable() {
    for (uInt i=0; i < blk.nelements(); i++)
	if (blk[i]) {
	    delete blk[i];
	    blk[i] = 0;
	}
    filled_ = defs_ = 0;
    used_ = total_ = blk.nelements();
}

template<class key, class val> const val &HashMap<key,val>::operator() (const key &ky) const {
    uInt offset = hash(ky);
    if ( ! blk[offset] )
	return dfltVal;
    for (ConstListIter<OrderedPair<key,val> > iter(blk[offset]); ! iter.atEnd(); iter++)
	if ( iter.getRight().x() == ky )
	    return iter.getRight().y();

    return dfltVal;
}

template<class key, class val> val &HashMap<key,val>::operator() (const key &ky) {
    uInt offset = hash(ky);
    if ( ! blk[offset] ) {
	blk[offset] = new List<OrderedPair<key,val> >();
	++filled_;
    }
    ListIter<OrderedPair<key,val> > iter(blk[offset]);
    for (; ! iter.atEnd(); iter++)
	if ( iter.getRight().x() == ky )
	    break;

    if (iter.atEnd()) {
	++defs_;
	if ( loading() > maxLoad() && enlarge() == offset ) {
	    --defs_;
	    return operator()(ky);
	}
	iter.addRight(OrderedPair<key,val>(ky,dfltVal));
    }
    return iter.getRight().y();
}

template<class key, class val> val &HashMap<key,val>::define (const key &k, const val &v) {
    uInt offset = hash(k);
    if ( ! blk[offset] ) {
	blk[offset] = new List<OrderedPair<key,val> >();
	++filled_;
    }
    ListIter<OrderedPair<key,val> > iter(blk[offset]);
    for (; ! iter.atEnd(); iter++)
	if ( iter.getRight().x() == k )
	    break;

    if (iter.atEnd()) {
	++defs_;
	if ( loading() > maxLoad() && enlarge() == offset ) {
	    --defs_;
	    return define(k,v);
	}
	iter.addRight(OrderedPair<key,val>(k,v));
    }
    return iter.getRight().y();
}

template<class key, class val> void HashMap<key,val>::remove (const key &k) {
    uInt offset = hash(k);
    if ( ! blk[offset] )
	return;
    for (ListIter<OrderedPair<key,val> > iter(blk[offset]); ! iter.atEnd(); iter++)
	if ( iter.getRight().x() == k ) {
	    iter.removeRight();
	    --defs_;
	    break;
	}
}

template<class key, class val> Bool HashMap<key,val>::isDefined (const key &ky) const {
    uInt offset = hash(ky);
    if ( ! blk[offset] )
	return False;
    for (ListIter<OrderedPair<key,val> > iter(blk[offset]); ! iter.atEnd(); iter++)
	if ( iter.getRight().x() == ky )
	    return True;
    return False;
}

template<class key, class val> Block<uInt> HashMap<key,val>::distribution() const {
    Block<uInt> b( availableBuckets() );

    for (uInt i = 0; i < availableBuckets(); i++) 
	b[i] = blk[i] ? blk[i]->len() : 0;

    return b;
}

template<class key, class val> uInt HashMap<key,val>::totalSize() const {
    return sizeof(HashMap<key,val>) + usedBuckets() * sizeof(List<OrderedPair<key,val> >) +
	ndefined() * sizeof(Link<OrderedPair<key,val> >);
}

template<class key, class val> uInt HashMap<key,val>::enlarge () {
    used_++;
    if ( availableBuckets() > totalBuckets() )
	total_ *= 2;
    if ( totalBuckets() > allocBuckets() ) {
	uInt old = allocBuckets();
	blk.resize(allocBuckets() * 2,False,True);
	for (; old < allocBuckets(); old++)
	    blk[old] = 0;
    }
    return populate( availableBuckets() - 1 );
}

template<class key, class val> uInt HashMap<key,val>::populate ( uInt to ) {
    uInt from = to - (totalBuckets() >> 1);

    if ( blk[from] ) {
	ListIter<OrderedPair<key,val> > toIter;
	for ( ListIter<OrderedPair<key,val> > fromIter(blk[from]); ! fromIter.atEnd();)
	    if ( hash(fromIter.getRight().x()) != from ) {
		if ( ! blk[to] ) {
		    blk[to] = new List<OrderedPair<key,val> >();
		    toIter = blk[to];
		    ++filled_;
		}
		toIter.addRight(OrderedPair<key,val>(fromIter.getRight().x(),fromIter.getRight().y()));
		fromIter.removeRight();
	    } else {
		fromIter++;
	    }
    }
    return from;
}

template<class key, class val> HashMap<key,val>::~HashMap () {
    clear();
}

template<class t> uInt Hash(const t &) {
    throw(AipsError("Hash(const t&) called, but no specialization exists"));
    return 0; // NOTREACHED
}

template<class t> const t &defaultHashValue(const t *) {
    static t val;
    return val;
}


} //# NAMESPACE CASACORE - END


#endif
