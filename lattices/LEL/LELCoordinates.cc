//# LELCoordinates.cc: Envelope class for Lattice coordinates
//# Copyright (C) 1998,1999,2000,2001
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


#include <casacore/lattices/LEL/LELCoordinates.h>
#include <casacore/lattices/LEL/LELLattCoordBase.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Default constructor
LELCoordinates::LELCoordinates()
{}

// Construct the object from the given letter class.
// It takes over the pointer and takes care of destructing
// the LELLattCoordBase object.
LELCoordinates::LELCoordinates (LELLattCoordBase* coordinates)
: coords_p (coordinates)
{}

// Copy constructor 
LELCoordinates::LELCoordinates (const LELCoordinates& other)
: coords_p (other.coords_p)
{}

// Destructor does nothing
LELCoordinates::~LELCoordinates()
{}

// Assignment 
LELCoordinates& LELCoordinates::operator= (const LELCoordinates& other)
{
    if (this != &other) {
	coords_p = other.coords_p;
    }
    return *this;
}

// Return the underlying letter object.
const LELLattCoordBase& LELCoordinates::coordinates() const
{
    AlwaysAssert (!coords_p.null(), AipsError);
    return *coords_p;
}

// Does it have coordinates ?
Bool LELCoordinates::hasCoordinates() const
{
    if (coords_p.null()) {
        return False;
    }
    return coords_p->hasCoordinates();
}

// Check if the coordinates of this and that conform.
Int LELCoordinates::compare (const LELCoordinates& that) const
{
    if (coords_p.null()  ||  that.coords_p.null()) {
        return 9;
    }
    return coords_p->compare (*(that.coords_p));
} 

} //# NAMESPACE CASACORE - END

