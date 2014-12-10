//# ImageInfo.h: Miscellaneous information related to an image
//# Copyright (C) 1998,1999,2000,2001,2002
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

#ifndef IMAGES_IMAGEINFO_H
#define IMAGES_IMAGEINFO_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/RecordTransformable.h>

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/BasicSL/String.h>

#include <casacore/coordinates/Coordinates/CoordinateSystem.h>

#include <casacore/images/Images/ImageBeamSet.h>

//# Forward declarations
#include <casacore/casa/iosfwd.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

class LoggerHolder;

// <summary>
// Miscellaneous information related to an image.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=RecordTransformable>RecordTransformable</linkto>
// </prerequisite>
//
// <synopsis>
// This class is used to record information about an image.
// At present it contains the following:
// <ol>
//    <li> The restoring beam(s)
//    <li> A parameter describing what quantity the image holds.
//    <li> The image object name.
// </ol>
//
// Support for per plane (eg channel) dependent beams have been added.
// </synopsis>
//
// <example>
// The interface is a simple get/set interface. Note that the "set" methods
// can be chained together since each set method returns a reference to its
// object (rather like cout).
// <srcblock>
//    ImageInfo ii;
//    ii.setRestoringBeam(Quantity(30,"arcsec"), Quantity(10,"arcsec"),
//                        Quantity(-18,"deg"));
//    ...
//    cout << "The restoring beam is : " << oi.restoringBeam() << endl;
// </srcblock>
// </example>
//
// <motivation>
// This sort of information needed a standard place to go with a
// standard interface so it could be moved out of MiscInfo.
// </motivation>

class ImageInfo : public RecordTransformable
{
public:

    // This enum defines the actual quantity being held in an image
    // It's really only used for descriptive information.
    enum ImageTypes {
       Undefined=0,
       Intensity,
       Beam,
       ColumnDensity,
       DepolarizationRatio,
       KineticTemperature,
       MagneticField,
       OpticalDepth,
       RotationMeasure,
       RotationalTemperature,
       SpectralIndex,
       Velocity,
       VelocityDispersion,
       nTypes
    };

// Default constructor

    ImageInfo();

// Destructor
    ~ImageInfo();

// Copy constructor (copy semantics)
    ImageInfo(const ImageInfo &other);

// Assignment (copy semantics)
    ImageInfo &operator=(const ImageInfo &other);

    // Set and get the Image Type.
    // <group>
    ImageInfo::ImageTypes imageType () const;
    ImageInfo& setImageType(ImageTypes type);
    static String imageType(ImageInfo::ImageTypes type);
    static ImageInfo::ImageTypes imageType(String type);
    // </group>

// Set and get the Image object name
// <group>
    String objectName () const;
    ImageInfo& setObjectName (const String& object);
// </group>

// Functions to interconvert between an ImageInfo and a record. These 
// functions are inherited from class
// <linkto class=RecordTransformable>RecordTransformable</linkto>. As new
// fields get added to ImageInfo these functions should be augmented. Missing
// fields should not generate an error to in fromRecord to allow for 
// backwards compatibility - null values should be supplied instead.
// The record field names are: "restoringbeam, imagetype, objectname".
// <group>
    virtual Bool toRecord(String& error, RecordInterface& outRecord) const;
    virtual Bool fromRecord(String& error, const RecordInterface& inRecord);
// </group>

// In some circumstances it might be useful to know what the defaults for
// the various values are so you can check if they have been set.
// The default restoring beam is a null vector.
// <group>
    static ImageTypes defaultImageType();
    static String defaultObjectName();
    static GaussianBeam defaultRestoringBeam();
// </group>

// Functions to interconvert between an ImageInfo and FITS keywords
// (converted to a Record).    Failure of <src>fromFITS</src>
// should probably not be regarded as fatal as the default ImageInfo
// values are viable.  For each item contained
// in the ImageInfo, an attempt to decode it from FITS is made.
// If any of them fail, False is returned, but it attempts to decode
// them all.  For those that fail an error message is held in <src>error</src> 
// in the order restoring beam, and image type.
// <src>error</src> will be returned of length 0 if the return 
// value is True, else it will be length 2.
// <group>
    Bool toFITS(String & error, RecordInterface & outRecord) const;
    Bool fromFITS(Vector<String>& error, const RecordInterface & inRecord);
// </group>

// This function takes an unofficial fitsValue found on the Stokes axis
// and returns the appropriate ImageType.  The idea is that you 
// detect the unofficial value, drop the Stokes axis, and store
// the value as an ImageType in ImageInfo.  Only values pertaining
// to beam, optical depth and spectral index are handled here.  All others
// give back Undefined.  See usage in Image FITS conversion classes.
    static ImageInfo::ImageTypes imageTypeFromFITS(Int fitsValue);

// It might be useful to know what FITS keyword names are used in to/from
// FITS so we can remove them so they won't be used more than once. The
// names are in lower case.
    static Vector<String> keywordNamesFITS();

// Convert the Miriad 'btype' strings to the ImageType.  Some 
// Miriad 'btype's are dealt with in Casacore via the Stokes
// axis (fractional_polarization, polarized_intensity, position_angle)
// and so these will return Undefined.
   static ImageInfo::ImageTypes MiriadImageType (const String& type);

    // Set and get the beam.
    // Zero-based <src>channel</src> and <src>stokes</src> are
    // necessary and used if and only if the ImageBeamSet
    // has multiple beams for such an axis. If just a single beam, that beam
    // is returned. If no (or a null) beam, a null beam is returned.
    GaussianBeam restoringBeam(Int channel=-1, Int stokes=-1) const;

    // Set the single global restoring beam. An exception will be
    // thrown if this object already has multiple beams. In that case,
    // the caller must call removeRestoringBeam() first.
    void setRestoringBeam(const GaussianBeam& beam);
  //#/// Added to build casarest with nrao-nov12
    void setRestoringBeam(const Quantum<Double>& major,
                          const Quantum<Double>& minor,
                          const Quantum<Double>& pa)
      { setRestoringBeam (GaussianBeam (major, minor, pa)); }

    // Remove all beams (global or per plane) associated with this object.
    void removeRestoringBeam();

   // Get the beam set associated with this object
   const ImageBeamSet& getBeamSet() const;

    // Set the beam for a specific plane.
    // A value of <src>channel</src> or <src>stokes</src> of less than 0
    // means that particular coordinate does not exist. Obviously, at least
    // one of these must be zero or greater. The only consistency checking
    // that is done is to ensure the values of <src>channel</src> and
    // <src>stokes</src> are consistent with the size of the beam array.
    // Additional consistency checks are done when this object is added via
    // ImageInterface<T>::setImageInfo().
    // <br>This function cannot be used if no beams have been set via set(All)Beams.
    // <group>
    void setBeam(Int channel, Int stokes, const Quantity& major,
                 const Quantity& minor, const Quantity& pa);

    void setBeam(Int channel, Int stokes, const GaussianBeam& beam);
    // </group>

    // does this object contain multiple beams?
    Bool hasMultipleBeams() const
      { return _beams.hasMultiBeam(); }

    // does this object contain a single beam
    Bool hasSingleBeam() const
      { return _beams.hasSingleBeam(); }

    // Does this object contain one or more beams?
    Bool hasBeam() const
      { return ! _beams.empty(); }

    // <group>
    // Number of channels and stokes in per hyper-plane beam array
    uInt nChannels() const
      { return _beams.nchan(); }
    uInt nStokes() const
      { return _beams.nstokes(); }
    // </group>

    // <group>
    // Initialize all per-plane beams to the same value
    void setAllBeams(
    	const uInt nChannels, const uInt nStokes,
    	const GaussianBeam& beam
    );

    // Set the per plane beams array directly.
    void setBeams(const ImageBeamSet& beams);
    // </group>

    // This method is not meant for common use. New code should not use it.
    // Get the restoring beam from a LoggerHolder (where the history is stored)
    // as AIPS writes the beam in the FITS history rather than the header
    // keywords. If there is no beam,  False is returned, and the internal
    // state of the object is unchanged.
    Bool getRestoringBeam (LoggerHolder& logger);

    // Convert the given beam to a Record.
    Record beamToRecord(Int channel, Int stokes) const;

    // Check if the beam set matches the coordinate axes sizes.
    void checkBeamSet (const CoordinateSystem& coords,
                       const IPosition& shape,
                       const String& imageName) const;

    // Append the other beamset to this one.
    void appendBeams (ImageInfo& infoThat,
                      Int axis, Bool relax, LogIO& os,
                      const CoordinateSystem& csysThis,
                      const CoordinateSystem& csysThat,
                      const IPosition& shapeThis,
                      const IPosition& shapeThat);

    // Check if the beam shape matches the coordinates.
    void checkBeamShape (uInt& nchan, uInt& npol,
                         const ImageInfo& info,
                         const IPosition& shape,
                         const CoordinateSystem& csys) const;

    // Combine beam sets for the concatenation of images and replace
    // the beamset in this object by the result.
    // If channel or stokes is the concatenation axis, that beam axis
    // is concatenated. Otherwise it is checked if both beam sets
    // match and are merged.
    // If relax=False, an exception is thrown if mismatching.
    void combineBeams (const ImageInfo& infoThat,
                       const IPosition& shapeThis,
                       const IPosition& shapeThat,
                       const CoordinateSystem& csysThis,
                       const CoordinateSystem& csysThat,
                       Int axis,
                       Bool relax,
                       LogIO& os);

    // Concatenate the beam sets along the frequency axis.
    void concatFreqBeams (ImageBeamSet& beamsOut,
                          const ImageInfo& infoThat,
                          Int nchanThis,
                          Int nchanThat,
                          Bool relax,
                          LogIO& os) const;

    // Concatenate the beam sets along the stokes axis.
    void concatPolBeams (ImageBeamSet& beamsOut,
                         const ImageInfo& infoThat,
                         Int npolThis,
                         Int npolThat,
                         Bool relax,
                         LogIO& os) const;

    // Merge the beam sets and check if they match.
    void mergeBeams (ImageBeamSet& beamsOut,
                     const ImageInfo& infoThat,
                     Bool relax,
                     LogIO& os) const;

    // If relax=True, give a warning message if warn=True and set to False.
    // Otherwise give an error showing msg1 only.
    static void logMessage(Bool& warn, LogIO& os, Bool relax,
                           const String& msg1, const String msg2=String());

    // Get the beam area in terms of pixel size of the specified
    // DirectionCoordinate
    Double getBeamAreaInPixels(Int channel, Int stokes,
                               const DirectionCoordinate&) const;
 
    static Double getBeamAreaInPixels(
    	const GaussianBeam& beam, const DirectionCoordinate& dc
    );

private:
  // Common copy ctor/assignment operator code.
  void copy_other(const ImageInfo &other);

  // Set the restoring beam from the record.
  void _setRestoringBeam(const Record& inRecord);

  //# Data members
  ImageBeamSet _beams;
  mutable Bool _warnBeam;   //# tell if warning is already given
  ImageTypes   itsImageType;
  String       itsObjectName;
};

// <summary> Global functions </summary>
// <group name=Output>
// Output declaration - useful for debugging.
ostream &operator<<(ostream &os, const ImageInfo &info);
// </group>



} //# NAMESPACE CASACORE - END

#endif
