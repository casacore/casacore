//# BitVector.h: Bit vectors of any size
//# Copyright (C) 1993,1994,1995,1999,2000,2001
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

#ifndef CASA_BITVECTOR_H
#define CASA_BITVECTOR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class BitVectorHelper;

// The size of a unsigned Integer ( assumes 8-bit char )
const uint32_t WORDSIZE = sizeof(uint32_t)*8;

// <summary>
// Bit vectors of any size
// </summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1995/03/13" tests="tBitVector" demos="">

// <etymology>
// A variable utilized as a discrete collection of bits is referred
// to as a bit vector.
// </etymology>

// <synopsis> 
// Bit vectors are an efficent method of keeping <em>true/false</em>
// information on a set of items or conditions. Class BitVector
// provides functions to manipulate individual bits in the vector and
// to perform logical operations on whole bit vectors.
// </synopsis> 

// <example>
// <srcblock>
// // Create a bit vector with 20 bits (and set them all to false). 
// BitVector bv (20, false);
//
// // Change some individual bits:
// // Turn On (make true) bit 19.
// bv.setBit (19);
// // Turn Off (make false) bit 12 (superfluous here).
// bv.clearBit (12);
// // Toggle bit 5 (here: change value from 0 (false) to 1 (true)).
// bv.toggleBit (5)
// // Another way of setting a bit using the index operator.
// bv[0] = true;
// // Assign the value of bit 0 to bit 1  (in three ways).
// bv[1] = bv.getBit(0);
// bv[1] = bv[0];
// bv.putBit (1, bv.getBit(0));
//
// // Show the bit vector size and its value on standard output.
// cout << "Size of bit vector:  "<< b.nbits() <<"\n";
// cout << "Value of bit vector: "<< bv        <<"\n";
//
// // Perform logical operations on bit vectors.
// // Create two more bit vectors.
// BitVector bv2 (40, false);
// BitVector bv3 (40, true);
// // bitwise OR
// bv = bv2 | bv3;
// // bitwise AND
// bv = bv2 & bv3;
// // bitwise XOR
// bv = bv2 ^ bv3;
// // bitwise NOT
// bv = ~bv2;
//
// // Reset all bits to false, and then to true
// bv = false;
// bv.set (true);
// // Change the vector's size to 10 (and copy the old values).
// bv.resize (10);
// // Change back to original size and set all bits to true.
// void bv.resize (size, true, false);
// </srcblock>
// </example> 


class BitVector
{
public:
    // BitVectorHelper is a helper class.
    friend class BitVectorHelper;

    // Create a bit vector of length 0.
    BitVector ();

    // Create a bit vector with <src>length</src> bits
    // and set all bits to to the specified state.
    BitVector (uint32_t length, bool state);

    // Copy constructor (copy semantics).
    BitVector (const BitVector& that);

    // Delete the bit vector.
   ~BitVector ();

    // Assignment (copy semantics).
    BitVector& operator= (const BitVector& that);

    // Set all bits to the given state.
    BitVector& operator= (bool state);

    // Return the number of bits in the bitvector.
    uint32_t nbits() const;

    // Set a bit at the given position (0-relative).
    // In debug-mode an exception is thrown when the position is invalid.
    void setBit (uint32_t pos); 

    // Clear a bit at the given position (0-relative).
    // In debug-mode an exception is thrown when the position is invalid.
    void clearBit (uint32_t pos); 

    // Toggle a bit at the given position (0-relative).
    // It returns the original state.
    // In debug-mode an exception is thrown when the position is invalid.
    bool toggleBit (uint32_t pos); 

    // Get a bit at the given position (0-relative).
    // In debug-mode an exception is thrown when the position is invalid.
    bool getBit (uint32_t pos) const;

    // Set a bit at the given position (0-relative) to the given state.
    // In debug-mode an exception is thrown when the position is invalid.
    void putBit (uint32_t pos, bool state);

    // Index operator to access the specified bit.
    // In debug-mode an exception is thrown when the position is invalid.
    // <group>
    bool operator[] (uint32_t pos) const;
    BitVectorHelper operator[] (uint32_t pos);
    // </group>

    // Logical operations on whole bit vectors.
    // The binary operators <src>&</src> (bitwise
    // AND), <src>|</src> (bitwise OR) and <src>^</src> (bitwise XOR),
    // and the unary operator <src>~</src> (bitwise NOT) are provided.
    // An exception is thrown if the lengths of the vectors differ.
    // <group>
    BitVector operator& (const BitVector& that) const;
    BitVector operator| (const BitVector& that) const;
    BitVector operator^ (const BitVector& that) const;
    BitVector operator~ () const;
    // </group>

    // Logical in-place operations on whole bit vectors.
    // The binary operators <src>&</src> (bitwise
    // AND), <src>|</src> (bitwise OR) and <src>^</src> (bitwise XOR),
    // and the unary operator <src>reverse</src> (bitwise NOT) are provided.
    // An exception is thrown if the lengths of the vectors differ.
    // <group>
    void operator&= (const BitVector& that);
    void operator|= (const BitVector& that);
    void operator^= (const BitVector& that);
    void reverse ();
    // </group>

    // Returns true if all bits are equal.
    // An exception is thrown if the lengths of the vectors differ.
    bool operator== (const BitVector& that) const;

    // Returns true if a bit differs.
    // An exception is thrown if the lengths of the vectors differ.
    bool operator!= (const BitVector& that) const;

    // Resize the bit vector to the new length.
    // By default the original bits are copied.
    // The remaining bits (or all bits in case of no copy) are
    // set the the given state.
    void resize (uint32_t length, bool state=false, bool copy=true);

    // Set all bits of the bit vector to the specified state.
    void set (bool state);

    // Set <src>length</src> bits starting at the start position
    // (0-relative) to the given state.
    // An exception is thrown if start+length exceeds the length
    // of the vector.
    void set (uint32_t start, uint32_t length, bool state);

    // Copy <src>length</src> bits starting at thatStart in the
    // other BitVector to this BitVector starting at thisStart.
    void copy (uint32_t thisStart, uint32_t length, const BitVector& that,
	       uint32_t thatStart);

    // Write a representation of the bit vector (a list of
    // <em>zeros</em> and <em>ones</em> enclosed in square
    // parentheses) to ostream.
    friend ostream& operator<< (ostream&, const BitVector& vector);

private:
    // Number of bits in the BitVector object.
    uint32_t size_p;

    // Pointer to the actual bit vector, stored as a contiguous
    // sequence of one or more unsigned integers.
    Block<uint32_t> bits_p;
};



// <summary> Helper class for BitVector </summary>
// <use visibility=local>
// <reviewed reviewer="Friso Olnon" date="1995/03/13" tests="tBitVector" demos="">

// <prerequisite>
// <li> class <linkto class=BitVector>BitVector</linkto>
// </prerequisite>

// <synopsis> 
// Helper class for class <linkto class=BitVector>BitVector</linkto>.
// For all practical purposes a BitVectorHelper object is the individual bit in
// a bit vector. It is the object returned by the index operator of
// BitVector.
// </synopsis> 

class BitVectorHelper
{
friend class BitVector;

public:
    // Copy constructor has to be public.
    BitVectorHelper (const BitVectorHelper& that);

    // Set the bit to the state of the bit in the other BitVector.
    // Thus assignment has not the usual copy semantics, but affects
    // the underlying BitVector bit.
    const BitVectorHelper& operator= (const BitVectorHelper& that) const;

    // Set to a state.
    const BitVectorHelper& operator= (bool state) const;

    // Defines the conversion from <src>BitVectorHelper</src> to
    // <src>bool</src>.
    operator bool() const;

private:
    uint32_t bitNumber_p;

    // Pointer back to the original vector.
    BitVector* vecPtr_p;

    // The constructor we actually use.
    BitVectorHelper (uint32_t bitNumber, BitVector* vector);
};



inline void BitVector::setBit (uint32_t pos)
{
    DebugAssert (pos < size_p, AipsError);
    uint32_t index = pos/WORDSIZE;
    bits_p[index] |= (1 << (pos - index*WORDSIZE));
}

inline void BitVector::clearBit (uint32_t pos)
{
    DebugAssert (pos < size_p, AipsError);
    uint32_t index = pos/WORDSIZE;
    bits_p[index] &= (~ (1 << (pos - index*WORDSIZE)));
}

inline bool BitVector::operator[] (uint32_t pos) const
{
    return getBit (pos);
}

inline uint32_t BitVector::nbits() const
{
  return size_p;
}


inline BitVectorHelper::BitVectorHelper (uint32_t bitNumber, BitVector* vector)
: bitNumber_p (bitNumber),
  vecPtr_p    (vector)
{}

inline BitVectorHelper BitVector::operator[] (uint32_t pos)
{
    return BitVectorHelper (pos, this);
}

inline BitVectorHelper::BitVectorHelper (const BitVectorHelper& that)
: bitNumber_p (that.bitNumber_p),
  vecPtr_p    (that.vecPtr_p)
{}

inline const BitVectorHelper& BitVectorHelper::operator= (bool state) const
{
    vecPtr_p->putBit (bitNumber_p, state);
    return *this;
}

inline BitVectorHelper::operator bool() const
{
    return vecPtr_p->getBit (bitNumber_p);
}

inline const BitVectorHelper& BitVectorHelper::operator=
                                    (const BitVectorHelper& that) const
{
    vecPtr_p->putBit (bitNumber_p, that.vecPtr_p->getBit (that.bitNumber_p));
    return *this;
}




} //# NAMESPACE CASACORE - END

#endif

