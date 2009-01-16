//# Projection.h: Geometric parameters needed for a sky projection to a plane
//# Copyright (C) 1997,1998,1999,2000,2001,2003
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

#ifndef COORDINATES_PROJECTION_H
#define COORDINATES_PROJECTION_H

#include <casa/aips.h>
#include <casa/Arrays/Vector.h>

namespace casa { //# NAMESPACE CASA - BEGIN


// <summary>
//  Geometric parameters needed for a sky projection to a plane
// </summary>

// <use visibility=export>

// <reviewed reviewer="Peter Barnes" date="1999/12/24" tests="tProjection">
// </reviewed>
//
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
// </prerequisite>
//
// <synopsis>
// This class is used to hold:
// <ol>
//    <li> The type of the projection (e.g. SIN); and
//    <li> The parameters of the projection, if any. These parameters are described
//         by Calabretta and Greisen (called PROJP) in the 1996 draft. 
//         In the recent versions, this paper has split into three, and the
//         projection parameters have been reworked into the PV matrix.
//         However, these have not yet been implemented in WCSLIB so we
//         stick with the old ones for now.
// </ol>
// </synopsis>
//
// <example>
// <srcblock>
//    Projection proj(Projection::CAR);
//    cerr << proj.parameters() << endl;
// </srcblock>
// This projection requires no parameters so the printed parameter
// vector would be of zero length.
// </example>
//
// <thrown>
//   <li>  AipsError
// </thrown>
//
// <todo asof="2000/01/01">
//   <li> Worry about projection parameters which are unit dependent (i.e. 
//        radians vs. degrees).
//   <li> LONGPOLE should probably go in here.
// </todo>
// 

class Projection
{
public:
    // Hold all the known types of celestial projections.
    enum Type {
	// Zenithal/Azimuthal perspective.
	AZP,
	// Slant zenithal perspective, new
	SZP,
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
	// Plate carree
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
        // The old GLS projection is now SFL. The 'GLS'
        // string will be converted to 'SFL'
	SFL, 
	// Parabolic.
	PAR, 
	// Hammer-Aitoff.
	AIT, 
	// Mollweide.
	MOL, 
	// COBE quadrilateralized spherical cube.
	CSC, 
	// Quadrilateralized spherical cube.
	QSC,
	// Tangential spherical cube.
	TSC,
	// HEALPix grid
	HPX, 
	// N_PROJ gives the number of supported projections - it shouldn't be used
	// as a projection
	N_PROJ };

    // Construct a projection which needs no parameters. SIN is unique in that
    // it can be created with 0 or 2 parameters.
    Projection(Projection::Type which=CAR);

    // Construct a projection from FITS CTYPE keywords
    Projection(const String& ctypeLin, const String& ctypeLat,
               const Vector<Double>& parameters);

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

    // Turn a projection type name into a Type. 
    // Returns N_PROJ if the projection is not known.
    static Projection::Type type(const String &name);

    // How many parameters does this projection have at most?
    // What is the minimum number of parameters that have to be supplied?
    // What are the parameter values?
    // <group>
    static uInt nParameters(Projection::Type proj);
    static uInt nMinParameters(Projection::Type proj);
    const Vector<Double> &parameters() const;
    // </group>

    // Comparison to fractional tolerance. 
    Bool near(const Projection &other, Double tol=1.0e-6) const;

    // Is this projection a 'zenithal' projection
    static Bool isZenithal (Projection::Type proj);

private:
    Projection::Type which_p;
    Vector<Double> parameters_p;

    void validate();
    Projection::Type type (String& ctypeLong, String& ctypeLat) const;
};

//#---------- Inlines --------------------------------------------------------------
inline Projection::Type Projection::type() const {return which_p;}
inline const Vector<Double> & Projection::parameters() const {return parameters_p;}

} //# NAMESPACE CASA - END

#endif

