//# Queue.cc: A First-In-First-Out (FIFO) data structure.
//# Copyright (C) 1995
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

#ifndef CASA_QUEUE_TCC
#define CASA_QUEUE_TCC

#include <casacore/casa/Containers/Queue.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> Queue<T>::Queue() : first_p(0), next_p(0), data_p(1)
{
    // Nothing
}

template<class T> Queue<T>::~Queue()
{
    // Nothing - storage reclaimed in Block dtor
}

template<class T> Queue<T>::Queue(const Queue<T> &other)
    : first_p(other.first_p), next_p(other.next_p), data_p(other.data_p)
{
    // The lazy solution. We could avoid some copying if we didn't copy
    // the empty elements first.
    compress();
}

template<class T> Queue<T> &Queue<T>::operator=(const Queue<T> &other)
{
    if (&other == this) {
	return *this;
    }

    // The lazy solution. We could avoid some copying if we didn't copy
    // the empty elements first.
    first_p = other.first_p;
    next_p = other.next_p;
    data_p = other.data_p;
    compress();
    return *this;
}

template<class T> void Queue<T>::clear()
{
    first_p = next_p = 0;
    data_p.resize(1, True, False);
}

template<class T> void Queue<T>::compress()
{
    // Take some care to do this efficiently
    uInt n = next_p - first_p + 1;

    T *oldstorage = data_p.storage();
    T *newstorage = new T[n];
    objcopy(newstorage, oldstorage + first_p, n);
    // The data_p Block now takes over responsibility for deleting newStorage.
    data_p.replaceStorage(n, newstorage, True);
    next_p -= first_p;
    first_p = 0;
}

template<class T> void Queue<T>::enqueue(const T &value)
{
    data_p[next_p++] = value;
    if (next_p >= Int(data_p.nelements())) {
	// If we only resized by nelements()+1 (say), we would have a quadratic
	// cost in adding elements. This exponential doubling results in only a
	// logarithmic cost.
	data_p.resize(2*data_p.nelements());
    }
}

template<class T> void Queue<T>::operator()(const T &value)
{
    enqueue(value);
}

template<class T> void Queue<T>::dequeue(T &value)
{
    if (nelements() == 0) {
	throw(AipsError("Queue<T>::dequeue(T &value) - empty queue"));
    }

    value = data_p[first_p++];
    if (first_p >= next_p) {
	clear();
    }
}

template<class T> T Queue<T>::operator()()
{
    T value;
    dequeue(value);
    return value;
}

} //# NAMESPACE CASACORE - END


#endif
