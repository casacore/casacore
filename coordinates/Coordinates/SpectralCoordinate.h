//# SpectralCoordinate.h: Interconvert between pixel and frequency.
//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003,2004
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


#ifndef COORDINATES_SPECTRALCOORDINATE_H
#define COORDINATES_SPECTRALCOORDINATE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/coordinates/Coordinates/Coordinate.h>
#include <casacore/coordinates/Coordinates/ObsInfo.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Utilities/PtrHolder.h>

#include <wcslib/wcs.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


class TabularCoordinate;
class LogIO;
class MVFrequency;
class VelocityMachine;
template<class T> class Quantum;


// <summary>
// Interconvert pixel and frequency values.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Peter Barnes" date="1999/12/24" tests="tSpectralCoordinate">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=Coordinate>Coordinate</linkto>
//   <li> <linkto class=MFrequency>MFrequency</linkto>,
//        <linkto class=MDoppler>MDoppler</linkto> and
//        <linkto class=VelocityMachine>VelocityMachine</linkto> 
//        classes if you want radial velocities.
// </prerequisite>
//
// <synopsis>
// This class performs the mapping from pixel to frequency. 
// This can be done via a Tabular lookup or via an algorithmic
// implementation which may be linear or non-linear.  The latter
// is implemented via the WCS library.
// 
// </synopsis>
//

// <note role=caution>
// All pixels coordinates are zero relative.
// </note>
//
// <example>
// Let us make a linear SpectralCoordinate first
// <srcblock>
//   Double restfreq = 1.420405752E9;
//   Double crpix = 10.0;
//   Double crval = 1.4e9;
//   Double cdelt = 1.0e6;
//   SpectralCoordinate sc(MFrequency::TOPO, crval, cdelt, crpix, restfreq);
//
//   Double world, pixel;
//   pixel = 12.1;
//   if (!sc.toWorld(world, pixel)) {
//      cerr << "Error : " << sc.errorMessage() << endl;
//   } else {
//      cerr << "pixel, world = " << pixel << ", " << world << endl;
//   }
//
// </srcblock>
// </example>
//
// <example>
// Now we make a non-linear SpectralCoordinate 
// <srcblock>
//   Vector<Double> freqs(5);
//   freqs(0) = 1.4e9; freqs(1) = 1.41e9;
//   freqs(2) = 1.43e9; freqs(3) = 1.44e9;
//   freqs(4) = 1.47e9;
//   SpectralCoordinate sc(MFrequency::LSRK, freqs, restfreq);
//
//   Double world, pixel;
//   world = 1.42e9;
//   if (!sc.toPixel(pixel, world)) {
//      cerr << "Error : " << sc.errorMessage() << endl;
//   } else {
//      cerr << "world, pixel = " << world << ", " << pixel << endl;
//   }
//
// </srcblock>
// </example>
//
// <motivation>
//  Spectral-line astronomy requires a specialized SpectralCoordinate.
// </motivation>

// <todo asof="2000/01/01">
//   <li> Allow other than linear interpolations for frequency lookup.
// </todo>
// 

class SpectralCoordinate : public Coordinate
{
public:
	 enum SpecType { // taken from the FITS spectral coordinate type codes
		 FREQ,
		 VRAD,
		 VOPT,
		 BETA,
		 WAVE,
		 AWAV
	 };

	 // Default constructor.    It is equivalent to doing
    // SpectralCoordinate(MFrequency::TOPO, 0.0, 1.0, 0.0)
    SpectralCoordinate();

    // Create a linear frequency axis SpectralCoordinate
    // <src>f0</src> is the frequency of the reference pixel, <src>inc</src> is the pixel increment,
    // <src>refPix</src> is the reference pixel. You can
    // optionally store the rest frequency for later use in calculating radial
    // velocities.  Use 0 for restFrequency if continuum.
    //
    // Frequencies and increments initially in Hz.
    SpectralCoordinate(MFrequency::Types type, Double f0, Double inc, 
		       Double refPix, Double restFrequency = 0.0);

    // Create linear frequency axis SpectralCoordinate with Quantum-based interface. 
    // Parameters are the same as above.
    // Regardless of the units of the Quanta, the initial units
    // of the SpectralCoordinate will be Hz.  You can change it to 
    // something else with the setWorldAxisUnits method later if you want.
    // Use 0 for restFrequency if continuum.
    SpectralCoordinate(MFrequency::Types type, const Quantum<Double>& f0, 
                       const Quantum<Double>& inc, Double refPix, 
                       const Quantum<Double>& restFrequency = Quantum<Double>(0.0,"Hz"));

    // Construct a SpectralCoordinate with the specified frequencies (in Hz).
    // This axis can be nonlinear; the increments and related 
    // functions return the <src>average</src> values
    // (calculated from the first and last pixel's frequencies).
    //
    // A linear interpolation/extrapolation is used for pixels which are
    // not supplied. The reference pixel is chosen to be 0.
    // The frequencies must increase or decrease monotonically (otherwise
    // the toPixel lookup would not be possible).
    // Use 0 for restFrequency if continuum.
    SpectralCoordinate(MFrequency::Types type, const Vector<Double> &freqs,
		       Double restFrequency = 0.0);
    
    // Construct a SpectralCoordinate with the specified frequencies
    // with Quantum-based interface. 
    // Parameters are the same as above.
    // Regardless of the units of the Quanta, the initial units
    // of the SpectralCoordinate will be Hz.
    // Use 0 for restFrequency if continuum.
    SpectralCoordinate(MFrequency::Types type, const Quantum<Vector<Double> >& freqs,
		       const Quantum<Double>& restFrequency = Quantum<Double>(0.0,"Hz"));

    // Construct a SpectralCoordinate with the specified velocities (in km/s).
    // They will be converted to Hz and the SpectralCoordinate constructed.
    // This axis can be nonlinear; the increments and related 
    // functions return the <src>average</src> values
    // (calculated from the first and last pixel's frequencies).
    //
    // A linear interpolation/extrapolation is used for pixels which are
    // not supplied. The reference pixel is chosen to be 0.
    // The velocities must increase or decrease monotonically (otherwise
    // the toPixel lookup would not be possible).
    SpectralCoordinate(MFrequency::Types freqType, MDoppler::Types velType, 
                       const Vector<Double>& velocities,  const String& velUnit,
                       Double restFrequency = 0.0);

    // Construct a SpectralCoordinate with the specified wavelengths (in mm).
    // They will be converted to Hz and the SpectralCoordinate constructed.
    // This axis can be nonlinear; the increments and related 
    // functions return the <src>average</src> values
    // (calculated from the first and last pixel's frequencies).
    // If inAir is True, the input wavelengths are assumed to be Air Wavelengths.
    // They are converted to vacuum frequency using the refractive index
    // which is calculated based on the mean input air wavelength.
    //
    // A linear interpolation/extrapolation is used for pixels which are
    // not supplied. The reference pixel is chosen to be 0.
    // The wavelengths must increase or decrease monotonically (otherwise
    // the toPixel lookup would not be possible).
    SpectralCoordinate(MFrequency::Types freqType,     
		       const Vector<Double>& wavelengths,  const String& waveUnit,
                       Double restFrequency = 0.0, Bool inAir = False);

    // Construct from wcs structure.  Must hold only a spectral wcs structure
    // Specify whether the absolute pixel coordinates in the wcs structure
    // are 0- or 1-relative.  The coordinate is always constructed with 0-relative
    // pixel coordinates
    SpectralCoordinate(MFrequency::Types freqType, const ::wcsprm& wcs, Bool oneRel=True);
    
    // Copy constructor (copy semantics).
    SpectralCoordinate(const SpectralCoordinate &other);

    // Assignment (copy semantics).
    SpectralCoordinate &operator=(const SpectralCoordinate &other);

    // Destructor.
    virtual ~SpectralCoordinate();

    // Always returns Coordinate::SPECTRAL.
    virtual Coordinate::Type type() const;

    // Always returns the String "Spectral".
    virtual String showType() const;

    // Always returns 1.
    // <group>
    virtual uInt nPixelAxes() const;
    virtual uInt nWorldAxes() const;
    // </group>

    // Set extra conversion layer.  Whenever a conversion from pixel to world is done,
    // the world value is then further converted to this MFrequency::Types value.
    // For example, your SpectralCoordinate may be defined in LSRK.
    // You can use this to get the world values out in say BARY. You must
    // specify the position on earth, the epoch and the direction for the conversions
    // and it is your responsibility to ensure they are viable.
    // Similarly, whenever you convert from world to pixel, the world
    // value is assumed to be that appropriate to the setReferenceConversion type.
    // It is first converted to the MFrequency::Types with which the
    // SpectralCoordinate was constructed and from there to pixel.
    // If you don't call this function, or you set the same type
    // for which the SpectralCoordinate was constructed, no extra
    // conversions occur.   Some conversions will fail.  These are the
    // ones that require extra frame information (radial velocity) such
    // as to REST. This will be added later.  In this case this function
    // returns False (and the conversion parameters are all left as they were),
   //  else it returns True.
    // <group>
    Bool setReferenceConversion (MFrequency::Types type,
                                 const MEpoch& epoch, const MPosition& position,
                                 const MDirection& direction);
    void getReferenceConversion (MFrequency::Types& type,
                                 MEpoch& epoch, MPosition& position,
                                 MDirection& direction) const
       {type = conversionType_p; epoch=epoch_p; 
        position=position_p; direction=direction_p;};
    // </group>

    // Convert a pixel to a world coordinate or vice versa. Returns True
    // if the conversion succeeds, otherwise it returns False and
    // <src>errorMessage()</src> contains an error message.  The input vectors
    // must be of length one and the output vectors are resized if they are not
    // already of length one.
    // <group>
    virtual Bool toWorld(Vector<Double> &world, 
  		         const Vector<Double> &pixel) const;
    virtual Bool toPixel(Vector<Double> &pixel, 
  		         const Vector<Double> &world) const;
    Bool toWorld(Double& world, const Double& pixel) const;
    Bool toPixel(Double& pixel, const Double& world) const;
    // </group>

    // Convert a pixel (channel number) into an MFrequency or MVFrequency and vice 
    // versa. Usually you will do
    // this for calculating velocities or converting frequencies from one frame
    // to another.
    // <group>
    Bool toWorld(MFrequency &world,
		 Double pixel) const;
    Bool toPixel(Double& pixel, const MFrequency &world) const;
    Bool toWorld(MVFrequency &world,
		 Double pixel) const;
    Bool toPixel(Double& pixel, const MVFrequency &world) const;
    // </group>

    // Batch up a lot of transformations. The first (most rapidly varying) axis
    // of the matrices contain the coordinates. Returns False if any conversion
    // failed  and  <src>errorMessage()</src> will hold a message.
    // The <src>failures</src> array (True for fail, False for success)
    // is the length of the number of conversions and
    // holds an error status for each conversion.  
    // <group>
    virtual Bool toWorldMany(Matrix<Double>& world,
                              const Matrix<Double>& pixel,
                              Vector<Bool>& failures) const;
    virtual Bool toPixelMany(Matrix<Double>& pixel,
                             const Matrix<Double>& world,   
                             Vector<Bool>& failures) const;
    // </group>

    // Set the state that is used for conversions from pixel and frequency to velocity
    // or wavelength. The SpectralCoordinate is constructed  with 
    // <src>MDoppler::RADIO</src> and <src>km/s</src> as the velocity conversion state
    // and <src>mm</src> as the wavelength conversion state.
    // The functions in this class which use this state are those that convert
    // to or from velocity.  Also, function <src>format</src> uses the Doppler
    // state set here.  If the function returns False it means the unit was 
    // not valid.  There will be an error message in function <src>errorMessage</src>
    // <group>
    Bool setVelocity (const String& velUnit=String("km/s"),
                      MDoppler::Types velType=MDoppler::RADIO);

    MDoppler::Types velocityDoppler () const {return velType_p;};
    String velocityUnit () const {return velUnit_p;};
    //
    Bool setWavelengthUnit (const String& waveUnit=String("mm"));
    String wavelengthUnit () const {return waveUnit_p;};
    //
    Bool setNativeType (const SpectralCoordinate::SpecType spcType);
    SpectralCoordinate::SpecType nativeType() const {return nativeType_p;}

    // </group>
    // Functions to convert to velocity (uses the current active
    // rest frequency) or wavelength.  There is no reference frame
    // change but you can specify the velocity Doppler and the output
    // units of the velocity with function <src>setVelocity</src>
    // or <src>setWavelength</src> respectively. When the input is a frequency stored 
    // as a Double it must be in the current units of the SpectralCoordinate.  
    // 
    // Note that the extra conversion layer (see function <src>setReferenceConversion</src>)
    // is active in the <src>pixelToVelocity</src> functions (because internally
    // the use <src>toWorld</src>) but not in the <src>frequencyToVelocity</src> 
    // or <src>frequencyToWavelength</src> functions.
    // <group>  
    Bool pixelToVelocity (Quantum<Double>& velocity, Double pixel) const;
    Bool pixelToVelocity (Double& velocity, Double pixel) const;
    Bool pixelToVelocity (Vector<Double>& velocity, const Vector<Double>& pixel) const;
    //
    Bool frequencyToVelocity (Quantum<Double>& velocity, Double frequency) const;
    Bool frequencyToVelocity (Quantum<Double>& velocity, const MFrequency& frequency) const;
    Bool frequencyToVelocity (Quantum<Double>& velocity, const MVFrequency& frequency) const;
    Bool frequencyToVelocity (Double& velocity, Double frequency) const;
    Bool frequencyToVelocity (Vector<Double>& velocity, const Vector<Double>& frequency) const;
    //
    Bool frequencyToWavelength (Vector<Double>& wavelength, const Vector<Double>& frequency) const;
    Bool frequencyToAirWavelength (Vector<Double>& wavelength, const Vector<Double>& frequency) const;
    // The refractive index of air (argument can be wavelength or airwavelength)
    // according to Greisen et al., 2006, A&A, 464, 746.
    // If airwavelength is used there is an error of the order of 1E-9.
    // Argument must be in micrometers!  
    //static Double refractiveIndex(const Double& lambda_um);
    // </group>

    // Functions to convert from velocity (uses the current active
    // rest frequency) or wavelength.  There is no reference frame
    // change but you can specify the velocity Doppler and the output
    // units of the velocity with function <src>setVelocity</src>
    // and those of the wavelength with <src>setWavelength</src>. 
    // When the input is a frequency stored 
    // as a Double it must be  in the current units of the SpectralCoordinate.  
    //
    // Note that the extra conversion layer (see function <src>setReferenceConversion</src>)
    // is active in the <src>pixelToVelocity</src> functions (because internally
    // the use <src>toPixel</src>) but not in the <src>frequencyToVelocity</src> functions.
    // <group>  
    Bool velocityToPixel (Double& pixel, Double velocity) const;
    Bool velocityToPixel (Vector<Double>& pixel, const Vector<Double>& velocity) const;
    // 
    Bool velocityToFrequency (Double& frequency, Double velocity) const;
    Bool velocityToFrequency (Vector<Double>& frequency, const Vector<Double>& velocity) const;
    //
    Bool wavelengthToFrequency (Vector<Double>& frequency, const Vector<Double>& wavelength) const;
    Bool airWavelengthToFrequency (Vector<Double>& frequency, const Vector<Double>& wavelength) const;
    // </group>

    // The SpectralCoordinate can maintain a list of rest frequencies
    // (e.g. multiple lines within a band).  However, only
    // one of them is active (e.g. for velocity conversions) at any 
    // one time.  Function <src>restFrequency</src> returns that
    // frequency.    Function <src>restFrequencies</src> returns   
    // all of the possible restfrequencies.
    //
    // When you construct the SpectralCoordinate, you give it one rest frequency
    // and it is the active one.  Thereafter you can add a new restfrequency
    // with function <src>setRestFrequency</src> (<src>append=True</src>) and 
    // that frequency will become the active one.    With this function
    // and <src>append=False</src>, the current active restfrequency will
    // be replaced by the one you give.
    //
    // You can change the list of
    // restfrequencies with function <src>setRestFrequencies</src>. When
    // you do so, you can either replace the list of rest frequencies or append to it.
    // You specify which frequency of the new (appended) list
    // becomes active.  
    //
    // You can also select the active rest frequency either by an index into
    // the current list (exception if out of range) given by
    // <src>restFrequencies()</src> or by the value in the list
    // nearest to the frequency you give.
    //
    // Whenever you change the active rest frequency, the class internals
    // are adjusted (e.g. the velocity machine is updated).
    // <group>
    Double restFrequency() const;
    const Vector<Double>& restFrequencies() const;
    Bool setRestFrequency(Double newFrequency, Bool append=False);
    void setRestFrequencies(const Vector<Double>& newFrequencies, uInt which=0,
                            Bool append=False);
    void selectRestFrequency(uInt which);
    void selectRestFrequency(Double frequency);
    String formatRestFrequencies () const;
    // </group>
  
    // Retrieve/set the frequency system.  Note that setting the
    // frequency system just changes the internal value of the
    // frequency system.  In addition, it will reset the internal
    // conversion frequency system to the new type and delete any
    // conversion machines.  
    // <group>
    MFrequency::Types frequencySystem(Bool showConversion=False) const;
    void setFrequencySystem(MFrequency::Types type, Bool verbose=True);

    // Transform the SpectralCoordinate to a different native reference frame
    // keeping the conversion layer as is
    Bool transformFrequencySystem(MFrequency::Types type, 
				  const MEpoch& epoch,
                                  const MPosition& position,
				  const MDirection& direction);
    // </group>

    // Report the value of the requested attribute.
    // <group>
    virtual Vector<String> worldAxisNames() const;
    virtual Vector<Double> referencePixel() const;
    virtual Matrix<Double> linearTransform() const;
    virtual Vector<Double> increment() const;
    virtual Vector<Double> referenceValue() const;
    // </group>

    // Set the value of the requested attribute. Note that these just
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

    // Set/get the unit. Adjust the increment and
    // reference value by the ratio of the old and new units.
    // The unit must be compatible with  frequency.
    //<group>
    virtual Bool setWorldAxisUnits(const Vector<String> &units);
    virtual Vector<String> worldAxisUnits() const;
    //</group>

    // Comparison function. Any private Double data members are compared
    // with the specified fractional tolerance.  Don't compare on the specified 
    // axes in the Coordinate.  If the comparison returns False, 
    // <src>errorMessage()</src> contains a message about why.
    // <group>
    virtual Bool near(const Coordinate& other, 
                      Double tol=1e-6) const;
    virtual Bool near(const Coordinate& other, 
                      const Vector<Int>& excludeAxes,
                      Double tol=1e-6) const;
    // </group>


    // Find the Coordinate for when we Fourier Transform ourselves.  This pointer
    // must be deleted by the caller. Axes specifies which axes of the Coordinate
    // you wish to transform.   Shape specifies the shape of the image
    // associated with all the axes of the Coordinate.   Currently the
    // output reference pixel is always shape/2.  Cannot transform tabular
    // coordinates.  If the pointer returned is 0, it failed with a message
    // in <src>errorMessage</src>
    virtual Coordinate* makeFourierCoordinate (const Vector<Bool>& axes,
                                               const Vector<Int>& shape) const;


    // Format a SpectralCoordinate coordinate world value nicely through the
    // common format interface.  See <linkto class=Coordinate>Coordinate</linkto>
    // for basics.
    //
    // Format types SCIENTIFIC, FIXED, MIXED and DEFAULT are supported.
    // DEFAULT will use MIXED.
    //
    // The world value must always be given in native frequency units.
    // Use argument <src>unit</src> to determine what it will be 
    // converted to for formatting. If <src>unit</src> is given, it 
    // must be dimensionally consistent with Hz, m, or m/s.   
    // If you give a unit consistent with m/s then the
    // appropriate velocity Doppler type is taken from that set by
    // function <src>setVelocity</src>.  There is no frame conversion.
    // If <src>unit</src> is empty, the unit given by <src>setFormatUnit</src> 
    // is used.  If this is turn empty, then native units are used.
    virtual String format(String& unit,
                          Coordinate::formatType format,
                          Double worldValue,  
                          uInt worldAxis,
                          Bool isAbsolute=True,
                          Bool showAsAbsolute=True,
                          Int precision=-1, Bool usePrecForFixed=False) const;

    // Set the default formatter unit (which is initialized to empty).  Must 
    // be consistent with Hz or km/s.  
    // If the given unit is illegal, False is returned and the internal state unchanged.
    // This unit is used by the function <src>format</src> when the given
    // unit is empty.  
    // <group>
    String formatUnit () const {return formatUnit_p;}
    Bool setFormatUnit (const String& unit);
    // </group>

    // Convert to FITS header record.  When writing the FITS record,
    // the fields "ctype, crval, crpix", and "cdelt" must already be created. Other header
    // words are created as needed.  Use <src>oneRelative=True</src> to
    // convert zero-relative SpectralCoordinate pixel coordinates to 
    // one-relative FITS coordinates, and vice-versa.  If <src>preferVelocity=True</src>
    // the primary axis type will be velocity, if <src>preferWavelength=True</src> it will
    // be wavelength, else frequency. For a velocity axis, if <src>opticalVelDef=False</src>,
    // the radio velocity definition will be used, else optical definition. Similarly for a
    // wavelength axis, if <src>airWaveDef=True</src> air wavelength will be used, the
    // default is vacuum wavelength.
    //<group>
    void toFITS(RecordInterface &header, uInt whichAxis, 
		LogIO &logger, Bool oneRelative=True,
		Bool preferVelocity=True, Bool opticalVelDef=True,
		Bool preferWavelength=False, Bool airWaveDef=False) const;

// Old interface.  Handled by wcs in new interface in FITSCoordinateUtil.cc
//    static Bool fromFITSOld(SpectralCoordinate &out, String &error,
//   			 const RecordInterface &header, 
//			 uInt whichAxis,
//			 LogIO &logger, Bool oneRelative=True);
    //</group>

    // Save the SpectralCoordinate into the supplied record using the supplied field name.
    // The field must not exist, otherwise <src>False</src> is returned.
    virtual Bool save(RecordInterface &container,
		    const String &fieldName) const;

    // Recover the SpectralCoordinate from a record.
    // A null pointer means that the restoration did not succeed.
    static SpectralCoordinate* restore(const RecordInterface &container,
                                       const String &fieldName);

    // Convert from String to spectral type and vice versa.
    static Bool specTypetoString(String &stypeString, const SpecType &specType);
    static Bool stringtoSpecType(SpecType &specType, const String &stypeString);

    // Make a copy of the SpectralCoordinate using new. The caller is responsible for calling
    // delete.
    virtual Coordinate* clone() const;

    ostream& print(ostream& os) const;

    // is this a tabular coordinate?
    Bool isTabular() const;

private:

    SPtrHolder<TabularCoordinate> _tabular;            // Tabular coordinate OR
    mutable ::wcsprm wcs_p;                            // wcs structure is used 
    Double to_hz_p;                                    // Convert from current world units to Hz
    Double to_m_p;                                     // Convert from current wavelength units to m
//
    MFrequency::Types type_p, conversionType_p;        // Frequency system and conversion system
    Vector<Double> restfreqs_p;                        // List of possible rest frequencies
    uInt restfreqIdx_p;                                // Current active rest frequency index

                                                               // Conversion machines; for pixel<->world conversions only.
    mutable MFrequency::Convert* pConversionMachineTo_p;       // For type_p -> conversionType_p
    mutable MFrequency::Convert* pConversionMachineFrom_p;     // For conversionType_p -> type_p

    VelocityMachine* pVelocityMachine_p;           // The velocity machine does all conversions between world & velocity.
    MDoppler::Types velType_p;                     // Velocity Doppler
    String velUnit_p;                              // Velocity unit
//
    String waveUnit_p;                             // Wavelength unit for conversions between world & wavelength
    SpectralCoordinate::SpecType nativeType_p;     // The native spectral type
//
    Unit unit_p;                                   // World axis unit
    String axisName_p;                             // The axis name
    String formatUnit_p;                           // The default unit for the format function
// 
    MDirection direction_p;                // These are a part of the frame set for
    MPosition position_p;                  // the reference conversions machines
    MEpoch epoch_p;                        // They are only private so we can save their state

// Format checker
    void checkFormat(Coordinate::formatType& format,
                     const Bool ) const;

// Copy private data
   void copy (const SpectralCoordinate& other);

// Convert to and from conversion reference type
    virtual void convertTo (Vector<Double>& world) const;
    virtual void convertFrom (Vector<Double>& world) const;

// Deletes and sets pointer to 0
    void deleteVelocityMachine ();

// Deletes and sets pointers to 0
    void deleteConversionMachines ();

// Set up pixel<->world conversion machines
// Returns: 3 (machines were noOPs, machines deleted)
//          2 (types the same, machines deleted), 
//          1 (machines created and functioning)
//         -1 (machines could not make trial conversion, machines deleted)
    Int makeConversionMachines (MFrequency::Types type,  MFrequency::Types conversionType,
                                 const MEpoch& epoch, 
                                 const MPosition& position, 
                                 const MDirection& direction);

// Create velocity<->frequency machine 
    void makeVelocityMachine (const String& velUnit,                  
                              MDoppler::Types velType,
                              const Unit& freqUnit,
                              MFrequency::Types freqType,
                              Double restFreq);

// Make spectral wcs structure (items in Hz)
   static void makeWCS(wcsprm& wcs, const String& ctype, Double refPix, Double refVal, 
                       Double inc, Double pc, Double restFreq);

// Record restoration handling
// <group>
   static SpectralCoordinate* restoreVersion1 (const RecordInterface& container);
   static SpectralCoordinate* restoreVersion2 (const RecordInterface& container);
   static void restoreVelocity (SpectralCoordinate*& pSpectral,
                                const RecordInterface& subrec);
   static void restoreRestFrequencies (SpectralCoordinate*& pSpectral,
                                       const RecordInterface& subrec,
                                       Double restFreq);
   static void restoreConversion (SpectralCoordinate*& pSpectral,
                                  const RecordInterface& subrec);

// </group>

// Interconvert between the current units and wcs units (Hz)
// <group>
    void toCurrent(Vector<Double>& value) const;
    void fromCurrent(Vector<Double>& value) const;
// </group>

// Return unit conversion vector for converting to current units
   const Vector<Double> toCurrentFactors () const;

// Update Velocity Machine
   void updateVelocityMachine (const String& velUnit, 
                               MDoppler::Types velType);
// Restore wcs stuff from Record
   static Bool wcsRestore (Double& crval, Double& crpix, Double& cdelt,
                           Double& pc, String& ctype,
                           const RecordInterface& rec);

// Save wcs stuff into Record
   Bool wcsSave (RecordInterface& rec, const wcsprm& wcs,
                 const String& fieldName) const;

   void _setTabulatedFrequencies(const Vector<Double>& freqs);

};

ostream &operator<<(ostream &os, const SpectralCoordinate& spcoord);

} //# NAMESPACE CASACORE - END


#endif
