//# Projection.h: Geometric parameters needed for a sky projection to a plane
//# Copyright (C) 1997,1998,1999
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
//#
//# $Id$

#if !defined(AIPS_PROJECTION_H)
#define AIPS_PROJECTION_H

#include <aips/aips.h>
#include <aips/Arrays/Vector.h>

class DirectionCoordinate;

// <summary>
//  Geometric parameters needed for a sky projection to a plane
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> Knowledge of astronomical projections. The best source for these
//        purposes is: "Representations of celestial coordinates in FITS" by
//        Eric W. Greisen and Mark Calabretta. This paper (the "WCS" paper)
//        is currently (early 1997) 
//        in a late draft stage, and is available via anonymous ftp or WWW from
//        fits.cv.nrao.edu.
// </prerequisite>
//
// <synopsis>
// This class is used to hold:
// <ol>
//    <li> The type of the projection (e.g. SIN); and
//    <li> The parameters of the projection, if any. These parameters are described
//         in the "WCS" paper.
// </ol>
// </synopsis>
//
// <example>
// See the example in <linkto module=Coordinates>Coordinates.h</linkto>
// and the test program tProjection.cc
// </example>
//
// <todo asof="1997/1/13">
//   <li> Worry about projection parameters which are unit dependent (i.e. 
//        radians vs. degrees).
//   <li> LONGPOLE should probably go in here.
// </todo>

class Projection
{
public:
    // Hold all the known types of celestial projections.
    enum Type {
	// Zenithal/Azimuthal perspective.
	AZP,
	// Gnomonic.
	TAN, 
	// Orthographics/synthesis.
	SIN, 
	// Stereographic.
	STG, 
	// zenith/azimuthal equidistant.
	ARC, 
	// zenithal/azimuthal polynomial.
	ZPN, 
	// zenithal/azimuthal equal area.
	ZEA, 
	// Airy.
	AIR, 
	// Cylindrical perspective.
	CYP, 
	// Cartesian.
	CAR, 
	// Mercator.
	MER, 
	// Cylindrical equal area.
	CEA,
	// Conic perspective.
	COP, 
	// Conic equidistant.
	COD, 
	// Conic equal area.
	COE, 
	// Conic orthomorphic.
	COO, 
	// Bonne.
	BON, 
	// Polyconic.
	PCO, 
	// Sanson-Flamsteed (global sinusoidal).
	GLS, 
	// Parabolic.
	PAR, 
	// Hammer-Aitoff.
	AIT, 
	// Molweide.
	MOL, 
	// COBE quadrilateralized spherical cube.
	CSC, 
	// Quadrilateralized spherical cube.
	QSC,
	// Tangential spherical cube.
	TSC,
	// N_PROJ gives the number of supported projections - it shouldn't be used
	// as a projection
	N_PROJ };

    // Construct a projection which needs no parameters. SIN is unique in that
    // it can be created with 0 or 2 parameters.
    Projection(Projection::Type which=CAR);

    // Construct a projection which needs parameters. The parameter vector must be
    // the length of the required number of parameters.
    Projection(Projection::Type which, const Vector<Double> &parameters);

    // Copy constructor (copy semantics).
    Projection(const Projection &other);

    // Assignment (copy semantics)
    Projection &operator=(const Projection &other);

    // Destructor
    ~Projection();

    // What is the Type of this projection?
    Projection::Type type() const;

    // What is the type of this projection as a String (e.g. "SIN").
    // <group>
    String name() const;
    static String name(Projection::Type proj);
    // </group>

    // Turn a projection type name into a Type. Used during I/O primarily.
    // Returns N_PROJ if the projection is not known.
    static Projection::Type type(const String &name);

    // How many parameters does this projection have, and what are they?
    // <group>
    static uInt nParameters(Projection::Type proj);
    const Vector<Double> &parameters() const;
    // </group>

    // Comparison to fractional tolerance. 
    Bool near(const Projection &other, Double Tol) const;

private:
    Projection::Type which_p;
    Vector<Double> parameters_p;

    void validate();
};

//#---------- Inlines --------------------------------------------------------------
inline Projection::Type Projection::type() const {return which_p;}
inline const Vector<Double> & Projection::parameters() const {return parameters_p;}

#endif
