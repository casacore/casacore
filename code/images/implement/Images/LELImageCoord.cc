//# ImageCoord.cc: The letter class for image coordinates
//# Copyright (C) 1998
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


#include <trial/Images/ImageCoord.h>


ImageCoord::ImageCoord()
{}

ImageCoord::ImageCoord (const CoordinateSystem& coordinates)
: coords_p (new CoordinateSystem(coordinates))
{}

ImageCoord::~ImageCoord()
{}

const CoordinateSystem& ImageCoord::coordinates() const
{
    return *coords_p;
}

Bool ImageCoord::hasCoordinates() const
{
    return True;
}

String ImageCoord::classname() const
{
    return "ImageCoord";
}

Bool ImageCoord::conform (const LattCoord& other) const
{
// Call the virtual doConform function to be able to compare
// two ImageCoord objects.

    return other.doConform (*this);
}

Bool ImageCoord::doConform (const ImageCoord& other) const
{
// This is the real conformance checker.

    return coordinates().near (&(other.coordinates()));
}
