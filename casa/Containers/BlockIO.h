//# BlockIO.h: Functions to perform IO for the Block class
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
//#
//# $Id$

#ifndef CASA_BLOCKIO_H
#define CASA_BLOCKIO_H

#include <casacore/casa/aips.h>

//# Forward declarations.
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> class Block;
class AipsIO;

// <summary>IO functions for Block</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <use visibility=export>
// 
// <synopsis>
// These functions allow the user to write either an entire or a 
// partial <src>Block</src> out to an <src>ostream</src> or to
// <src>AipsIO</src>. These functions provide simple storage and
// display capabilities for <src>Block</src>.
// </synopsis>
//
// <linkfrom anchor=BlockIO classes="Block">
//    Global Block <here>IO functions</here>
// </linkfrom>
//
// <group name='BlockIO'>

//# It appears that (at least for the SUN compiler) the 3rd argument
//# cannot be an uInt (otherwise the compiler says no match when
//# called as e.g.  putBlock(ios,blk,10);).
//#
//# Note that as of 29-Dec-2008 the size of Block is a size_t, so nr should
//# also be a size_t. However, because AipsIO cannot handle sizes larger than
//# an uInt, it makes no sense to make that change.
//
// These functions allow the user to read and write <src>Block</src>s
// from the <src>AipsIO</src> stream. 
//
// <src>putBlock</src> writes the <src>Block</src> to the stream. If
// a number, <src>nr</src>, of elements is specified, only the first
// <src>nr</src> elements will be written out to <src>AipsI0</src>.
//
// <src>getBlock</src> reads a <src>Block</src> in from an 
// <src>AipsIO</src> stream.
//
// <group>
template<class T> void putBlock (AipsIO&, const Block<T>&, Int nr);

template<class T> void putBlock (AipsIO& ios, const Block<T>& blk)
    { putBlock (ios, blk, (Int)(blk.nelements())); }

template<class T> void getBlock (AipsIO&, Block<T>&);
// </group>


// These functions allow the user to write <src>Block</src>s out to
// a standard <src>ostream</src>. The user can either write the entire
// <src>Block</src> out to the stream, or if a number of elements,
// <src>nr</src>, is specified, only the first <src>nr</src> elements
// of the <src>Block</src> will be written out.
//
// <group>
template<class T> void showBlock (std::ostream&, const Block<T>&, Int nr);

template<class T> void showBlock (std::ostream& ios, const Block<T>& blk)
    { showBlock (ios, blk, (Int)(blk.nelements())); }
// </group>

// These are the standard shift operators for writing an entire
// <src>Block</src> out to a stream. Shift operators are provided
// to write the block out to either <src>AipsIO</src> or
// <src>ostream</src>. A shift operator is also provided for
// reading a <src>Block</src> in from <src>AipsIO</src>.
// <note> STL containers like vector and list are written in the same way as
// a Block, so they can be written one way and read back the other.
// </note>
//
// <group>
template<class T> AipsIO& operator<< (AipsIO& ios, const Block<T>& blk)
{
    putBlock (ios, blk, (Int)(blk.nelements()));
    return ios;
}
 
template<class T> AipsIO& operator>> (AipsIO& ios, Block<T>& blk)
{
    getBlock (ios, blk);
    return ios;
}

template<class T> std::ostream& operator<< (std::ostream& ios, const Block<T>& blk)
{
    showBlock (ios, blk, (Int)(blk.nelements()));
    return ios;
}
// </group>
// </group>


//# Implement the specialization for the void* data type.
//# This will not do anything at all.
//# This specialization is needed for StColMirAIO.cc.
inline void putBlock (AipsIO&, const Block<void*>&, Int)
{}
inline void getBlock (AipsIO&, Block<void*>&)
{}
inline void showBlock (AipsIO&, const Block<void*>&, Int)
{}



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/BlockIO.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
