//# MeasFrame.h: Container for Measure frame
//# Copyright (C) 1996,1997
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

#if !defined(AIPS_MEASFRAME_H)
#define AIPS_MEASFRAME_H

#if defined(_AIX)
#pragma implementation ("MeasFrame.cc")
#endif

//# Includes
#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Measures/Measure.h>

//# Forward Declarations
class MEpoch;
class MPosition;
class MDirection;
class MRadialVelocity;
class MVEpoch;
class MVPosition;
class MVDirection;
class MVRadialVelocity;
class FrameRep;
imported class ostream;

//# Constants (SUN compiler does not accept non-simple default arguments)

// <summary> Container for Measure frame </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
// <li> <linkto class=Measure>Measure</linkto> class
// <li> <linkto class=MeasRef>MeasRef</linkto> class
// </prerequisite>
//
// <etymology>
// From Measure and Frame
// </etymology>
//
// <synopsis>
// Measurements are made in a reference frame (epoch, position, with later
// maybe velocity, acceleration, dipole angle,...).<br>
// The class is a container for the reference frame Measures (MEpoch etc).
// Since a frame will possibly be used by many different Measures, it behaves
// as a smart pointer, with reference rather than copy characteristics.
// The actual data are stored in the class.<br>
// A MeasFrame is constructed by setting the appropiate Measures, either in
// a constructor, or with a set(). The input to the constructors and set are
// Measures.<br>
// Inside the frames automatic conversion to the most appropiate usage of
// its values is done (e.g. time to TBD time, position to astronomical
// longitude). Furthermore, a frame will in general be regularly updated
// (e.g. coordinate
// conversion for a series of times). To make use of cached information, and
// to speed up as much as possible, <src>reset...()</src> functions are 
// available. These reset functions accept the same range of input parameter
// types as the <linkto class=MeasConvert>MeasConvert</linkto> () operator,
// and will only change the used derivatives of the frame.<br>
// get...() functions return frame measures set.<br>
// <linkto class=MeasDetail>Meaure details</linkto> can be set for the
// internal conversion.
// </synopsis>
//
// <example>
// <srcblock>
//	MEpoch my_epoch(Quantity(MeasData::MJDB1950,"d")); // an epoch
//	MeasFrame frame(my_epoch);	// used in a frame
// </srcblock>
// </example>
//
// <motivation>
// To separate the frame definition from the measure type
// </motivation>
//
// <todo asof="1996/02/22">
// </todo>

class MeasFrame {

    public:
//# Friends
// Output a frame
    friend ostream &operator<<(ostream &os, const MeasFrame &mf);

    
//# Constructors
// Default constructor
    MeasFrame();
// Construct frame with specified measures
// <thrown>
//   <li> AipsError if a non-frame Measure
// </thrown>
// <grp>
    MeasFrame(const Measure &meas1);
    MeasFrame(const Measure &meas1, const Measure &meas2);
    MeasFrame(const Measure &meas1, const Measure &meas2,
	      const Measure &meas3);
// </grp>
// Copy constructor (reference semantics)
    MeasFrame(const MeasFrame &other);
// Copy assignment (reference semantics)
    MeasFrame &operator=(const MeasFrame &other);
// Destructor
    ~MeasFrame();

//# Operators
// Comparisons
// <group>
    Bool operator==(const MeasFrame &other) const;
    Bool operator!=(const MeasFrame &other) const;
// </group>

//# General member functions
// Test if empty
    Bool empty() const;

// Set frame elements
// <thrown>
//   <li> AipsError if a non-frame Measure
// </thrown>
// <group>
    void set(const Measure &meas1);
    void set(const Measure &meas1, const Measure &meas2);
    void set(const Measure &meas1, const Measure &meas2,
	     const Measure &meas3);
// </group>
// Reset a frame element and its cached derivatives.
// <thrown>
//   <li> AipsError if the specific Measure not yet present in frame
// </thrown>
// <group>
    void resetEpoch(Double val);
    void resetEpoch(const Vector<Double> &val);
    void resetEpoch(const Quantum<Double> &val);
    void resetEpoch(const Quantum<Vector<Double> > &val);
    void resetEpoch(const MVEpoch &val);
    void resetEpoch(const MEpoch &val);
    void resetPosition(const Vector<Double> &val);
    void resetPosition(const Quantum<Vector<Double> > &val);
    void resetPosition(const MVPosition &val);
    void resetPosition(const MPosition &val);
    void resetDirection(const Vector<Double> &val);
    void resetDirection(const Quantum<Vector<Double> > &val);
    void resetDirection(const MVDirection &val);
    void resetDirection(const MDirection &val);
    void resetRadialVelocity(const Vector<Double> &val);
    void resetRadialVelocity(const Quantum<Vector<Double> > &val);
    void resetRadialVelocity(const MVRadialVelocity &val);
    void resetRadialVelocity(const MRadialVelocity &val);
// </group>

// Get the epoch pointer (0 if not present)
    const MEpoch *const epoch() const;
// Get the position pointer (0 if not present)
    const MPosition *const position() const;
// Get the direction pointer (0 if not present)
    const MDirection *const direction() const;
// Get the radial velocity pointer (0 if not present)
    const MRadialVelocity *const radialVelocity() const;
// Get TDB in days
    Bool getTDB(Double &tdb) const;
// Get the longitude (in rad)
    Bool getLong(Double &tdb) const;
// Get the latitude (in rad)
    Bool getLat(Double &tdb) const;
// Get the gecentric position (in m)
    Bool getRadius(Double &tdb) const;
// Get the LAST (in days)
    Bool getLAST(Double &tdb) const;
// Get the LAST (in rad)
    Bool getLASTr(Double &tdb) const;
// Get J2000 coordinates (direction cosines)
    Bool getJ2000(MVDirection &tdb) const;
// Get B1950 coordinates (direction cosines)
    Bool getB1950(MVDirection &tdb) const;
// Get apparent coordinates (direction cosines)
    Bool getApp(MVDirection &tdb) const;
// Get LSR radial velocity (m/s)
    Bool getLSR(Double &tdb) const;
// Clear the frame
    void clear();

    private:
//# Data
// Representation of MeasFrame
    FrameRep *rep;

//# Member functions
// Create an instance of the MeasFrame class
    void create();
// Fill a MeasFrame element
    void fill(const Measure *in);
// Make full Epoch
    void makeEpoch();
// Make full Position
    void makePosition();
// Make full Direction
    void makeDirection();
// Make full RadialVelocity
    void makeRadialVelocity();
// Throw reset error
    void errorReset(const String &txt);
};

//# Global functions
// <summary> Global functions </summary>
// <group name=Output>
// Output a frame
    ostream &operator<<(ostream &os, const MeasFrame &mf);
// </group>

#endif
