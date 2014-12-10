//# OrderedPair.h: Ordered pair class
//# Copyright (C) 1993,1994,1995,1999
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

#ifndef CASA_ORDEREDPAIR_H
#define CASA_ORDEREDPAIR_H


//# Includes
#include <casacore/casa/aips.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>Ordered pair class</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <use visibility=local>

// <synopsis>
// This class is a simple class used in the Map<key,value> classes
// to manage key/value pairs for maps.
// The default constructor is needed for use in containers.
// This implies that ALL classes ever used in OrderedPair should
// have a default constructor!!!!
//
// <note>
// This should probably be cleaned up in the future and made into a 
// generally useful class.
// </note>
// </synopsis>


template<class K, class V> class OrderedPair
{
public:
    //
    // Needed for "operator>>(AipsIO &ios, Slist<elem> &list)"
    //
    OrderedPair();

    //
    // This is the "standard" constructor which takes a key and
    // a value and constructs an ordered pair.
    //
    OrderedPair(const K &k, const V &v);

    //
    // Copy constructor (copy semantics).
    //
    OrderedPair(const OrderedPair<K,V>& that);

    //
    // Assignment (copy semantics).
    //
    OrderedPair<K,V>& operator= (const OrderedPair<K,V>& that);

    // Get access to the key or value.
    // <group>
    K &x() {return Key;}
    const K &x() const {return Key;}
    V &y() {return Val;}
    const V &y() const {return Val;}
    // </group>

private:
    K Key;
    V Val;

    enum {OrderedPairVersion = 1};
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/OrderedPair.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
