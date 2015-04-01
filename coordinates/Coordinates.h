//# Coordinates.h : Classes to interconvert computation positions with physical
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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

#ifndef COORDINATES_COORDINATES_H
#define COORDINATES_COORDINATES_H

//# Module includes
#include <casacore/casa/aips.h>
#include <casacore/coordinates/Coordinates/Coordinate.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/LinearCoordinate.h>
#include <casacore/coordinates/Coordinates/LinearXform.h>
#include <casacore/coordinates/Coordinates/Projection.h>
#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/coordinates/Coordinates/StokesCoordinate.h>
#include <casacore/coordinates/Coordinates/TabularCoordinate.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
//
// <summary>
// Classes to interconvert pixel and world (physical) coordinates
// </summary>

// <prerequisite>
//   <li> Knowledge of astronomical coordinate conversions in general. Probably the
//        best documents are the papers by Mark Calabretta and Eric Greisen.
//        The initial draft from 1996 can be found at 
//        http://www.atnf.csiro.au/~mcalabre.  It is this draft that the
//        Coordinate classes are based upon.  Since then, this paper has evolved 
//        into three which can be found at the above address, and will be published in the
//        Astronomy and Astrophysics Supplement Series (probably in 2000).
//        The design has changed since the initial draft.  When these papers
//        are finalized, and the IAU has ratified the new standards, WCSLIB
//        (Mark Calabretta's implementation of these conventions) will be
//        revised for the new designs.  At that time, the Coordinate classes
//        may also be revised.
//   <li> Generic Casacore classes; especially those in the 
//        <linkto module=Arrays>Arrays</linkto> module.
//   <li> The <linkto module=Measures>Measures</linkto> module.
// </prerequisite>
//

// <reviewed reviewer="Peter Barnes" date="1999/12/24">
// </reviewed>

// <synopsis>
// The primary notion is that a <linkto class=Coordinate>Coordinate</linkto>
// can interconvert between a length "n" Vector<Double> (the
// pixel coordinate) and a length "m" Vector<Double> (the
// "world" coordinate). Note that "m" and "n" do not in 
// principle have to be the same (so that one can get both the RA and DEC from
// an image slice, for example), however in practice they currently always are.
// Each Coordinate has the full mapping from pixel to world coordinates, i.e.
// it has a reference value, reference pixel, increments, and an arbitrary
// transformation matrix. To go from a pixel to a world coordinate the following
// steps are applied:
// <ol>
//    <li> The reference pixel is subtracted from the pixel position.
//    <li> The result is multiplied by the transformation matrix.
//    <li> The result is multiplied by an increment per output axis to convert
//         it into physical coordinates.
//    <li> For some coordinate types (e.g., Direction), a non-linear function is
//         applied to this result.
// </ol>
//
// The classes are arranged as follows.  The base class is
// <linkto class=Coordinate>Coordinate</linkto> which defines the
// interface.  Classes derived from it are
// <ol>
//   <li> <linkto class=DirectionCoordinate>DirectionCoordinate</linkto> 
//   <li> <linkto class=LinearCoordinate>LinearCoordinate</linkto> 
//   <li> <linkto class=SpectralCoordinate>SpectralCoordinate</linkto> 
//   <li> <linkto class=TabularCoordinate>TabularCoordinate</linkto> 
//   <li> <linkto class=StokesCoordinate>StokesCoordinate</linkto> 
//   <li> <linkto class=CoordinateSystem>CoordinateSystem</linkto> 
// </ol>
//
// Other classes are <linkto class=Projection>Projection</linkto>  
// which is used to specify an astronomical projection for 
// DirectionCoordinates, and  <linkto class=LinearXform>LinearXform</linkto>  
// a helper class which the application programmer  will not interact with.
//
// <linkto class=CoordinateSystem>CoordinateSystem</linkto> is
// the class that application programmers will usually interact
// with. A CoordinateSystem consists of a collection of the other 
// classes derived from Coordinate.  Normally one group will be for
// RA/DEC, another for a Stokes axis, and another group for the spectral axis.
// The axes may be transposed arbitrarily, for example RA could be 
// the first axis, and DEC the third. 
//
// Normally the CoordinateSystem being manipulated will be embedded in a PagedImage
// or other object.    Note that the axes of the PagedImage do not
// necessarily map directly to the axes in the CoordinateSystem.  Functionality
// is provided to determine this mapping.
//
// One or more axes from the CoordinateSystem may be removed. Pixel axes and/or
// world axes may be removed. You are encouraged to leave all the world axes
// when you remove pixel axes.
// <br>
// If a world axis is removed, the corresponding pixel axis is also removed.
// This means that one can be sure that a pixel axis always has a
// corresponding world axis (it makes no sense otherwise).  The opposite is 
// not necessarily true: a world axis can exist without a pixel axis.
//
// The linear transformation and sky projection computations are carried out in an
// underlying library -- WCSLIB -- written by Mark Calabretta of the ATNF.
//
// </synopsis>
//
//
// <note role=caution>
// All pixels coordinates are zero relative.
// </note>
//
// <example>
// First, let's make a DirectionCoordinate --- used to represent a direction,
// usually an RA/DEC, but it could also be, e.g., an AZ/EL pair.
// <srcblock>
//    Matrix<Double> xform(2,2);                                    // 1
//    xform = 0.0; xform.diagonal() = 1.0;                          // 2
//    Quantum<Double> refLon(135.0, "deg");
//    Quantum<Double> refLat(60.0, "deg");
//    Quantum<Double> incLon(-1.0, "deg");
//    Quantum<Double> incLat(1.0, "deg");
//    DirectionCoordinate radec(MDirection::J2000,                  // 3
//                            Projection(Projection::SIN),          // 4
//                            refLon, refLat,                       // 5
//                            incLon, incLat,                       // 6
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
//         or B1950, but there are many other possibilities as listed
//         in the <linkto class=MDirection>MDirection</linkto> class
//         header.
//    <li> <i>4:</i>The <linkto class=Projection>Projection</linkto> class
//         defines the "geometry" that is used to map <src>xy<-->world</src>. SIN
//         is the most common projection for radio interferometers. Note that
//         SIN can optionally take parameters as defined in Calabretta and Greisen.
//         If not provided, they default to 0.0, which is the "old" SIN
//         convention.
//    <li> <i>5:</i>Set the reference position to RA=135, DEC=60 degrees.
//         Note that the native units of a DirectionCoordinate is radians.
//    <li> <i>6:</i> Set the increments to -1 degree in RA, and +1 degree
//         in DEC.
//    <li> <i>7:</i> Set the previously defined transformation matrix.
//    <li> <i>8:</i> Set the zero-relative reference pixel. Note that it does
//         not have to be incremental. At the reference pixel, the world 
//         coordinate has the reference value.
// </ul>
//
// Although we happeend to create our DirectionCoordinate with Quanta in degrees,
// these have been converted to radians by the constructor.  We can set the native units
// to degrees if we wish as follows:
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
//    cout << world << " <--- " << pixel << endl;         // 18
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
//    Vector<Int> iquv(4);
//    iquv(0) = Stokes::I; iquv(1) = Stokes::Q;                    // 21
//    iquv(2) = Stokes::U; iquv(3) = Stokes::V;                    // 22
//    StokesCoordinate stokes(iquv);                               // 23
// </srcblock>
// We create an integer array the same length as the Stokes axis, and place
// the corresponding Stokes enum into each element of the array. The values
// must be unique, e.g. there can only be one "I" plane.
// Besides the generic <src>Vector<Double></src> toWorld/toPixel interface,
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
//				  1.4E+9,                         // 28
//				  2.0E+4,                         // 29
//				  0,                              // 30
//				  1420.40575E+6);                 // 31
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
// The primary motivation is to provide support for converting pixel locations in an
// image to physical ("world") positions.
// </motivation>

// <todo asof="2000/01/01">
// <li> Add measures interfaces that handle reference frame conversions
// <li> offset coordinates
// </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif
