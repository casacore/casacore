//# Coordinates.h : Classes to interconvert computation positions with physical
//# Copyright (C) 1996,1997,1998
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

#if !defined (AIPS_COORDINATES_H)
#define AIPS_COORDINATES_H

//# Module includes
#include <trial/Coordinates/Coordinate.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/LinearCoordinate.h>
#include <trial/Coordinates/LinearXform.h>
#include <trial/Coordinates/Projection.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <trial/Coordinates/StokesCoordinate.h>
#include <trial/Coordinates/CoordinateUtil.h>

// <module>
//
// <summary>
// Classes to interconvert computation positions (pixel) with physical ones (world)
// </summary>

// <prerequisite>
//   <li> Knowledge of astronomical coordinate conversions in general. Probably the
//        most single useful document is "Representations of celestial coordinates in
//        FITS" Eric W. Greisen and Mark Calabretta. This is the "WCS" paper, and is
//        currently in late draft, available via anonymous ftp from
//        fits.cv.nrao.edu.
//   <li> Generic AIPS++ classes; especially those in the 
//        <linkto module=Arrays>Arrays</linkto> module.
//   <li> Perhaps some of the information in the
//        <linkto module=Measures>Measures</linkto> module.
// </prerequisite>
//

// <reviewed reviewer="" date="yyyy/mm/dd" demos="dCoordinateSystem.cc">
// </reviewed>

// <synopsis>

// The primary notion is that a <linkto class=Coordinate>Coordinate</linkto>
// can interconvert between a length <src>n</src> <src>Vector<Double></src> (the
// pixel coordinate) and a length <src>m</src> <src>Vector<Double></src> (the
// "world" coordinate). Note that <src>m</src> and <src>n</src> do not in 
// principle have to be the same (so that one can get both the RA and DEC from
//  an image slice, for example), however in practice they currently always are.
// Each coordinate has the full mapping from pixel to world coordinates, i.e.
// it has a reference value, reference pixel, increments, and an arbitrary
// transformation matrix. To go from a pixel to a world coordinate the following
// steps are applied:
// <ol>
//    <li> The reference pixel is subtracted from the pixel position.
//    <li> The result is multiplied by the transformation matrix.
//    <li> The result is multiplied by an increment per output axis to convert
//         it into physical coordinates.
//    <li> For some coordinate types (e.g., Direction), a nonlinear function is
//         applied to this result.
// </ol>
//
// A <linkto class=CoordinateSystem>CoordinateSystem</linkto> is the
// class that application programmers will usually interact
// with. A <src>CoordinateSystem</src> consists of one or more independent Coordinates
// which control the mapping of various axes. Normally one group will be for
// RA/DEC, another for a STOKES axis, and another group for the spectral axis.
// The axes may be transposed arbitrarily,
// for example RA could be the first axis, and DEC the third. 
//
// Normally the CoordinateSystem being manipulated will be embedded in a <src>PagedImage</src>
// or other object.    Note that the axes of the <src>PagedImage</src> do not
// necessarily map directly to the axes in the <src>CoordinateSystem</src>.  Functionality
// is provided to determine this mapping.
//
// One or more axes from the CoordinateSystem may be removed. Pixel axes and/or
// world axes may be removed. You are encouraged to leave all the world axes
// when you remove pixel axes.
// <br>
// If a world axis is removed, the corresponding pixel axis is also removed.
// This means that one can be sure that a pixel axis always has a
// corresponding world axis. The opposite is not necessarily true: a
// world axis can exist without a pixel axis.
//
// 
//
// The linear transformation and sky projection computations are carried out in an
// underlying library -- WCSLIB -- written by Mark Calabretta of the ATNF.
//
// </synopsis>
//
// <example>
// First, let's make a DirectionCoordinate --- used to represent a direction,
// usually an RA/DEC, but it could also be, e.g., an AZ/EL pair.
// <srcblock>
//    Matrix<Double> xform(2,2);                                    // 1
//    xform = 0.0; xform.diagonal() = 1.0;                          // 2
//    DirectionCoordinate radec(MDirection::J2000,                  // 3
//                            Projection(Projection::SIN),          // 4
//                            135*C::pi/180.0, 60*C::pi/180.0,      // 5
//                            -1*C::pi/180.0, 1*C::pi/180,          // 6
//                            xform,                                // 7
//                            128, 128);                            // 8
// </srcblock>
// <ul>
//    <li> <i>1-2:</i>Here we set up a diagonal transformation matrix.
//         Normally this matrix should be diagonal, however if you wanted
//         to introduce a rotation or skew, you would do it through this
//         matrix.
//    <li> <i>3:</i>This defines the astronomical type of the world 
//         coordinate. Most of the time it will probably be J2000
//         or B1950, but many other possibilities are possible as listed
//         in the <linkto class=MDirection>MDirection</linkto> class
//         header.
//    <li> <i>4:</i>The <linkto class=Projection>Projection</linkto> class
//         defines the "geometry" that is used to map <src>xy<-->world</src>. SIN
//         is the most common projection for radio interferometers. Note that
//         SIN can optionally take parameters as defined in Calabretta+Greisen.
//         If not provided, they default to 0.0, which is the "old" SIN
//         convention.
//    <li> <i>5:</i>Set the reference position to RA=135, DEC=60 degrees.
//         Note that the native units of a Direction is radians.
//    <li> <i>6:</i> Set the increments to -1 degree in RA, and +1 degree
//         in DEC.
//    <li> <i>7:</i> Set the previously defined transformation matrix.
//    <li> <i>8:</i> Set the zero-relative reference pixel. Note that it does
//         not have to be incremental. At the reference pixel, the world 
//         coordinate has the reference value.
// </ul>
//
// In this example is is more convenient to change the units to degrees. This can
// be accomplished as follows:
// <srcblock>
//    Vector<String> units(2); units = "deg";                       //  9
//    radec.setWorldAxisUnits(units);                               // 10
// </srcblock>
// The increment and reference value are updated appropriately.
//
// Set up a couple of vectors to use the world and pixel coordinate values.
// <srcblock>
//    Vector<Double> world(2), pixel(2);                            // 11
//    pixel = 138.0;                                                // 12
// </srcblock>
// We use 138 as an abitrary pixel position which is near the reference pixel
// so we can tell if the answers look foolish or not.
//
// We can actually perform a transformation like this as follows. If
// it succeeds we print the value of the world coordinate.
// <srcblock>
//    Bool ok = radec.toWorld(world, pixel);                        // 13
//    if (!ok) {                                                    // 14
//	cout << "Error: " << radec.errorMessage() << endl;          // 15
//	return 1;                                                   // 16
//    }                                                             // 17
//    cout << world.ac() << " <--- " << pixel.ac() << endl;         // 18
// </srcblock>
// There is an overloaded "toWorld" function that produces an MDirection
// in case you want to, e.g., find out what the position in B1950 coordinates
// would be.
//
// The reverse transformation takes place similarly:
// <srcblock>
//    ok = radec.toPixel(pixel, world);                             // 19
// </srcblock>
//
// Suppose we have an image with a Stokes axis. It can be set up as follows:
// <srcblock>
//    iquv(0) = Stokes::I; iquv(1) = Stokes::Q;                    // 21
//    iquv(2) = Stokes::U; iquv(3) = Stokes::V;                    // 22
//    StokesCoordinate stokes(iquv);                               // 23
// </srcblock>
// We create an integer array the same length as the Stokes axis, and place
// the corresponding Stokes enum into each element of the array. The values
// must be unique, e.g. there can only be one "I" plane.
// Besides the generic <src>Vector<Double></src> toWorld/fromWorld interface,
// you can also directly interconvert between Stokes enum and and (zero-relative)
// plane number:
// <srcblock>
//    Int plane;                                                   // 24
//    ok = stokes.toPixel(plane, Stokes::Q);                       // 25
// </srcblock>
// Here it will return <src>True</src> and set plane to 1. On the other
// hand, it would return <src>False</src> for:
// <srcblock>
//    ok = stokes.toPixel(plane, Stokes::XX);                      // 26
// </srcblock>
// since "XX" is not one of the Stokes enumerations we used to create this
// coordinate.
//
// A Spectral ("frequency") coordinate may be created as follows:
// <srcblock>
//    SpectralCoordinate spectral(MFrequency::TOPO,               // 27
//				1400 * 1.0E+6,                    // 28
//				20 * 1.0E+3,                      // 29
//				0,                                // 30
//				1420.40575 * 1.0E+6);             // 31
// </srcblock>
// The default frequency units of a spectral coordinate are Hz, although they
// may be changed to whatever is convenient. The first line (27) defines the
// type of frequency we have -- topocentric here. The second (28) line
// defines the frequency at the reference pixel, 0 (28) here. The channel
// increment is defined on line 29. A rest frequency of the spectral may
// be provided. It is useful in calculating doppler velocities. These calculations
// are carried out by the <linkto class=MFrequency>MFrequency</linkto> and
// <linkto class=MDoppler>MDoppler</linkto> classes of the Measures system.
// </example>
//
// <motivation>
// The primary motivation is to provide support for turning pixel locations in an
// image to physical ("world") positions.
// </motivation>

// <todo asof="1997/01/13">
// <li> Allow spectral axes which aren't regularly gridded in frequency?
// </todo>

// </module>

#endif
