//# LELCoordinates.cc: Envelope class for Lattice coordinates
//# Copyright (C) 1998,1999
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


#include <trial/Lattices/LELCoordinates.h>
#include <trial/Lattices/LatticeRegion.h>


// Default constructor
LELCoordinates::LELCoordinates()
: coords_p (new LELLattCoord())
{}

// Construct the object from the given letter class.
// It takes over the pointer and takes care of destructing
// the LELLattCoord object.
LELCoordinates::LELCoordinates (LELLattCoord* coordinates)
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
const LELLattCoord& LELCoordinates::coordinates() const
{
    return *coords_p;
}

// Does it have coordinates ?
Bool LELCoordinates::hasCoordinates() const
{
    return coords_p->hasCoordinates();
}

// Check if the coordinates of this and that conform.
Bool LELCoordinates::conform (const LELCoordinates& other) const
{
    return coords_p->conform (*(other.coords_p));
} 
