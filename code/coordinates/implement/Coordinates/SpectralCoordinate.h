//# SpectralCoordinate.h: Map a channel number to frequency.
//# Copyright (C) 1997,1998
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

#if !defined(AIPS_SPECTRAL_COORDINATE_H)
#define AIPS_SPECTRAL_COORDINATE_H

#include <aips/aips.h>
#include <trial/Coordinates/Coordinate.h>
#include <trial/Coordinates/TabularCoordinate.h>
#include <aips/Measures/MFrequency.h>

class LogIO;

// <summary>
// Map a channel number to frequency.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Coordinate>Coordinate</linkto>
//   <li> <linkto class=MFrequency>MFrequency</linkto> and
//        <linkto class=MDoppler>MDoppler</linkto> classes if you want
//        radial velocities.
// </prerequisite>
//
// <synopsis>
// This class performs the mapping from channel number to frequency. An MFrequency
// may be obtained if you are interested in radial velocities.
// </synopsis>
//
// <example>
// See the example in <linkto module=Coordinates>Coordinates.h</linkto>.
// </example>
//
// <todo asof="1997/08/15">
//   <li> Do velocity calculations directly for the user rather than going
//        through the measures system?
//   <li> Allow other than linear interpolations for frequency lookup.
// </todo>

class SpectralCoordinate : public Coordinate
{
public:
    // f0 is the frequncy of th reference channel, inc is the channel increment,
    // refChan is the (0-relative) reference channel (often 0). You can
    // optionally store the rest frequency for later use in calculating radial
    // velocities.
    //
    // Frequencies and increments initially in Hz. This may be changed later
    // with setWorldAxisUnits().
    SpectralCoordinate(MFrequency::Types type, Double f0, Double inc, 
		       Double refChan, Double restFrequency = 0.0);

    // Construct a SpectralCoordinate with the specified frequencies. The
    // increments and related functions return the <src>average</src> values
    // (calculated from the first and last pixels frequencies).
    //
    // A linear interpolation/extrapolation is used for channels which are
    // not supplied. The refrence channel (pixel) is chosen to be 0.
    // The frequencies must increase or decreas monotonically (otherwise
    // the toPixel lookup would not be possible).
    SpectralCoordinate(MFrequency::Types type, const Vector<Double> &freqs,
		       Double restFrequency = 0.0);
    
    // Equivalent to SpectralCoordinate(MFrequency::TOPO, 0.0, 1.0, 0.0)
    SpectralCoordinate();

    // Overwrite this SpectralCoordinate with other (copy semantics).
    // <group>
    SpectralCoordinate(const SpectralCoordinate &other);
    SpectralCoordinate &operator=(const SpectralCoordinate &other);
    // </group>

    virtual ~SpectralCoordinate();

    // Always returns Coordinate::SPECTRAL.
    virtual Coordinate::Type type() const;

    // Always returns "Spectral"
    virtual String showType() const;

    // Always returns 1.
    // <group>
    virtual uInt nPixelAxes() const;
    virtual uInt nWorldAxes() const;
    // </group>

    // Convert a pixel position to a worl position or vice versa. Returns True
    // if the conversion succeeds, otherwise it returns False and
    // <src>errorMessage()</src> contains an error message.   The output 
    // vectors are appropriately resized.
    // <group>
    virtual Bool toWorld(Vector<Double> &world, 
			 const Vector<Double> &pixel) const;
    virtual Bool toPixel(Vector<Double> &pixel, 
			 const Vector<Double> &world) const;
    // </group>

    // Turn a pixel (channel number) into an MFrequency. Usually you will do
    // this for calculating velocities or converting frequencies from one frame
    // to another.
    Bool toWorld(MFrequency &world,
		 Double pixel) const;

    // Retrieve/set the rest frequency in the current units.
    // <group>
    Double restFrequency() const;
    Bool setRestFrequency(Double newFrequency);
    // </group>
  
    // Retrieve/set the frequency system.  Note that setting the
    // frequency system just changes the internal value of the
    // frequency system, it does not cause any recomputation
    // or cause the result of <src>toWorld</src> to change.
    // <group>
    MFrequency::Types frequencySystem() const;
    void  setFrequencySystem(MFrequency::Types type);
    // </group>

    // Report the value of the requested attributed.
    // <group>
    virtual Vector<String> worldAxisNames() const;
    virtual Vector<Double> referencePixel() const;
    virtual Matrix<Double> linearTransform() const;
    virtual Vector<Double> increment() const;
    virtual Vector<Double> referenceValue() const;
    virtual Vector<String> worldAxisUnits() const;
    // </group>

    // Set the value of the requested attributed. Note that these just
    // change the internal values, they do not cause any recomputation.
    // <group>
    virtual Bool setWorldAxisNames(const Vector<String> &names);
    virtual Bool setReferencePixel(const Vector<Double> &refPix);
    virtual Bool setLinearTransform(const Matrix<Double> &xform);
    virtual Bool setIncrement(const Vector<Double> &inc) ;
    virtual Bool setReferenceValue(const Vector<Double> &refval);
    // </group>

    // Get the table, i.e. the pixel and world values. The length of these
    // Vectors will be zero if this axis is pure linear (i.e. if the
    // channel and frequencies are related through an increment and offset).
    // <group>
    Vector<Double> pixelValues() const;
    Vector<Double> worldValues() const;
    // </group>

    // ctype, crval, crpix, and cdelt must already be created. Other header
    // words are created as needed.  Use <src>oneRelative=True</src> to
    // convert zero-relative SpectralCoordinate pixel coordinates to 
    // one-relative FITS coordinates, and vice-versa.
    void toFITS(RecordInterface &header, uInt whichAxis, 
		LogIO &logger, Bool oneRelative=True,
		Bool preferVelocity=True, Bool opticalVelDef=True) const;
    static Bool fromFITS(SpectralCoordinate &out, String &error,
			 const RecordInterface &header, 
			 uInt whichAxis,
			 LogIO &logger, Bool oneRelative=True);

    // Set the unit. If adjust is True, the unit must be compatible with
    // frequency.
    virtual Bool setWorldAxisUnits(const Vector<String> &units,
				   Bool adjust = True);

    // Comparison function. Any private Double data members are compared
    // with the specified fractional tolerance.  Don't compare on the specified 
    // axes in the Coordinate.  If the comparison returns False, 
    // <src>errorMessage()</src> contains a message about why.
    // <group>
    virtual Bool near(const Coordinate* pOther, 
                      Double tol=1e-6) const;
    virtual Bool near(const Coordinate* pOther, 
                      const Vector<Int>& excludeAxes,
                      Double tol=1e-6) const;
    // </group>

    // Format a SpectralCoordinate world value with the common format interface
    // (refer to the base class <linkto class=Coordinate>Coordinate</linkto> for
    // more details on this interface, particularly with regards polymorphic
    // use).
    //
    // A SpectralCoordinate can be formatted in either
    // <src>Coordinate::SCIENTIFIC</src> or <src>Coordinate::FIXED</src> formats
    // only.  The argument <src>absolute</src> is ignored.
    //<group>
    virtual void getPrecision(Int& precision,
                              Coordinate::formatType& format,
                              const Bool absolute,
                              const Int defPrecSci,
                              const Int defPrecFloat,
                              const Int defPrecRADEC) const;
    virtual String format(String& units,
                          const Coordinate::formatType format,
                          const Double worldValue,
                          const uInt worldAxis,
                          const Bool absolute,
                          const Int precision = -1) const;
    //</group>

    // Save ourself into the supplised record using the supplied field name.
    // The field must not exist, otherwise <src>False</src> is returned.
    virtual Bool save(RecordInterface &container,
		    const String &fieldName) const;

    // A null pointer means that the restoration did not succeed - probably 
    // because fieldName doesn't exist or doesn't contain a coordinate system.
    static SpectralCoordinate *restore(const RecordInterface &container,
				   const String &fieldName);

    // Make a copy of ourself using new. The caller is responsible for calling
    // delete.
    virtual Coordinate *clone() const;
private:
    MFrequency::Types type_p;
    Double restfreq_p;
    TabularCoordinate worker_p;

    // Check format type
    void checkFormat(Coordinate::formatType& format,         
                     const Bool absolute) const;


    // Undefined and inaccessible
};


#endif
