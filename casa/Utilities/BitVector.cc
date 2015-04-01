//# BitVector.cc: A BitVector class with variable bit vector size
//# Copyright (C) 1993,1994,1995,2001
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

#include <casacore/casa/Utilities/BitVector.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

BitVector::BitVector()
: size_p (0),
  bits_p   (0)
{}

BitVector::BitVector (uInt length, Bool state)
: size_p (length),
  bits_p   ((length + WORDSIZE - 1) / WORDSIZE, uInt(0))
{
    if (state) {
	set (state);
    }
}

BitVector::BitVector (const BitVector& that)
: size_p (that.size_p),
  bits_p   (that.bits_p)
{}

BitVector::~BitVector()
{}

BitVector& BitVector::operator= (const BitVector& that)
{
    size_p = that.size_p;
    bits_p   = that.bits_p;
    return *this;
}

BitVector& BitVector::operator= (Bool state)
{
    set (state);
    return *this;
}

void BitVector::putBit (uInt pos, Bool state)
{
    if (state) {
	setBit (pos);
    }else{
	clearBit (pos);
    }
}

Bool BitVector::toggleBit (uInt pos)
{
    Bool result = getBit (pos);
    putBit (pos,  (!result));
    return result;
}

Bool BitVector::getBit (uInt pos) const
{
    DebugAssert (pos < size_p, AipsError);
    uInt index = pos/WORDSIZE;
    Bool result = True;
    if ((bits_p[index] & (1 << (pos - index*WORDSIZE)))  ==  0) {
	result = False;
    }
    return result;
}

void BitVector::resize (uInt length, Bool state, Bool copy)
{
    //# Do a true resize.
    uInt oldSize = size_p;
    bits_p.resize ((length + WORDSIZE - 1) / WORDSIZE, True, copy);
    size_p = length;
    if (!copy) {
	set (state);
    }else{
	if (length > oldSize) {
	    set (oldSize, length-oldSize, state);
	}
    }
}

void BitVector::set (Bool state)
{
    uInt value = 0;
    if (state) {
	value = ~value;
    }
    for (uInt i=0; i<bits_p.nelements(); i++) {
	bits_p[i] = value;
    }
}
void BitVector::set (uInt start, uInt length, Bool state)
{
    //# Determine the end bit.
    uInt end = start + length;
    if (end > size_p) {
	throw (AipsError ("BitVector::set past end-of-vector"));
    }
    if (length == 0) {
	return;
    }
    //# Determine the full words that can be set.
    //# When setting till the end of the vector, make endWord last word.
    uInt beginWord = (start + WORDSIZE - 1) / WORDSIZE;
    uInt endWord   = end / WORDSIZE;
    if (end == size_p) {
	endWord = bits_p.nelements();
    }
    uInt i;
    //# When there are no full words, we have to do part of a word only.
    if (beginWord >= endWord) {
	for (i=start; i<end; i++) {
	    putBit (i, state);
	}
    }else{
	//# Do some full words and part of the begin and end word.
	uInt value = 0;
	if (state) {
	    value = ~value;
	}
	for (i=beginWord; i<endWord; i++) {
	    bits_p[i] = value;
	}
	for (i=start; i<beginWord*WORDSIZE; i++) {
	    putBit (i, state);
	}
	for (i=endWord*WORDSIZE; i<end; i++) {
	    putBit (i, state);
	}
    }
}

void BitVector::copy (uInt start, uInt length, const BitVector& that,
		      uInt thatStart)
{
    if (start+length > size_p) {
	throw (AipsError ("BitVector::set past end-of-thisvector"));
    }
    if (thatStart+length > that.size_p) {
	throw (AipsError ("BitVector::set past end-of-thatvector"));
    }
    for (uInt i=0; i<length; i++) {
	putBit (start+i, that.getBit (thatStart+i));
    }
}


BitVector BitVector::operator& (const BitVector& that) const
{
    BitVector result(*this);
    result &= that;
    return result;
}
void BitVector::operator&= (const BitVector& that)
{
    if (size_p != that.size_p) {
	throw (AipsError ("BitVector::operator&= with different lengths"));
    }
    for (uInt i=0; i<bits_p.nelements(); i++) {
	bits_p[i] &= that.bits_p[i];
    }
}
BitVector BitVector::operator| (const BitVector& that) const
{
    BitVector result(*this);
    result |= that;
    return result;
}
void BitVector::operator|= (const BitVector& that)
{
    if (size_p != that.size_p) {
	throw (AipsError ("BitVector::operator|= with different lengths"));
    }
    for (uInt i=0; i<bits_p.nelements(); i++) {
	bits_p[i] |= that.bits_p[i];
    }
}
BitVector BitVector::operator^ (const BitVector& that) const
{
    BitVector result(*this);
    result ^= that;
    return result;
}
void BitVector::operator^= (const BitVector& that)
{
    if (size_p != that.size_p) {
	throw (AipsError ("BitVector::operator^= with different lengths"));
    }
    for (uInt i=0; i<bits_p.nelements(); i++) {
	bits_p[i] ^= that.bits_p[i];
    }
}
BitVector BitVector::operator~ () const
{
    BitVector result(*this);
    result.reverse();
    return result;
}
void BitVector::reverse()
{
    for (uInt i=0; i<bits_p.nelements(); i++) {
	bits_p[i] = ~(bits_p[i]);
    }
}


Bool BitVector::operator== (const BitVector& that) const
{
    uInt endWord = size_p / WORDSIZE;
    uInt i;
    for (i=0; i<endWord; i++) {
	if (bits_p[i] != that.bits_p[i]) {
	    return False;
	}
    }
    for (i=endWord*WORDSIZE; i<size_p; i++) {
	if (getBit (i)  !=  that.getBit (i)) {
	    return False;
	}
    }
    return True;
}

Bool BitVector::operator!= (const BitVector& that) const
{
    if (*this == that) {
	return False;
    }
    return True;
}


ostream& operator<< (ostream& os, const BitVector& vector)
{
    // The operator<< write out the less significant bit first
    // (write out first the bit in position zero).
    os << "[";
    for (uInt i=0; i<vector.nbits(); i++) {
	if (vector.getBit (i)) {
            os << "1";
	}else{
            os << "0";
	}
    }
    os << "]" << endl;
    return os;
}

} //# NAMESPACE CASACORE - END

