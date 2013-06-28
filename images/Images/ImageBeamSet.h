//# ImageBeamSet.h: Collection of image beams in frequency and stokes
//# Copyright (C) 2012
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

#ifndef IMAGES_IMAGEBEAMSET_H
#define IMAGES_IMAGEBEAMSET_H

#include <casa/aips.h>
#include <casa/Arrays/Matrix.h>
#include <components/ComponentModels/GaussianBeam.h>

namespace casa {

//# Forward Declarations
class CoordinateSystem;
class Slicer;


// <summary>
// Represents a set of restoring beams associated with an image.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
// </prerequisite>

// <etymology>
// A Set of Beams associated with an Image.
// </etymology>

// <synopsis>
// This class represents a set of restoring beams associated with
// a deconvolved image. Internally, the beams are stored in a Matrix in
// which the first dimension represents the spectral axis and the second
// dimension represents the polarization axis. Methods which take the number
// of channels and stokes as the input parameters will accept 0 for either of
// these, in the cases where the corresponding axis is absent, but internally,
// the associated axis of the storage Matrix will be set to one since a Matrix
// with one dimension of length 0 must be empty. If one (or both) of the axes is
// of length 1, the beam associated with that position is valid for all channels
// or stokes at the position.
// For example, if one has an image with 10 spectral channels and 4 stokes,
// and one has a beam set of dimensions (1, 4) associated with that image,
// all channels for a given stokes will have the same beam.
// Similarly, if the beam set is of shape (10, 1) in this case, all stokes
// will have the same beam for a given channel.
// If the axis lengths of the beam set are greater than one, they must be exactly
// the same length of the corresponding axes in the associated image.
// </synopsis>
//
// <example>
// </example>

// <motivation>
// Restoring beams are used many places in image analysis tasks.
// </motivation>

// <todo>
// </todo>

  class ImageBeamSet {
  public:

    // Construct an empty beam set.
    ImageBeamSet();

    // Construct a beam set from an 2-D array of beams representing
    // the frequency and stokes axis.
    // Axis length 1 means it is valid for all channels cq. stokes.
    // If the image has 0 spectral channels or stokes, the corresponding
    // length of the axis in the provided matrix should be 1.
    ImageBeamSet(const Matrix<GaussianBeam>& beams);

    // Construct an ImageBeamSet representing a single beam wich is
    // valid for all channels and stokes.
    ImageBeamSet(const GaussianBeam& beam);

    // Create an ImageBeamSet of the specified shape with all
    // GaussianBeams initialized to <src>beam</src>.
    ImageBeamSet(uInt nchan, uInt nstokes,
                 const GaussianBeam& beam = GaussianBeam::NULL_BEAM);

    // The copy constructor (reference semantics).
    ImageBeamSet(const ImageBeamSet& other);

    ~ImageBeamSet();

    // Assignment can change the shape (reference semantics).
    ImageBeamSet& operator=(const ImageBeamSet& other);

    // Beam sets are equal if the shapes and all beams are equal.
    Bool operator== (const ImageBeamSet& other) const;
    Bool operator!= (const ImageBeamSet& other) const;

    // Beam sets are equivalent if both have no beams or if the
    // expanded sets are equal. Expanded means that an axis can have
    // length 1 and is (virtually) expanded to the length of the matching
    // axis in the other beam set.
    Bool equivalent (const ImageBeamSet& that) const;

    // Get the number of elements in the beam array.
    // <group>
    uInt nelements() const
      { return _beams.size(); }
    uInt size() const
      { return _beams.size(); }
    // </group>

    // Does this beam set contain only a single beam?
    Bool hasSingleBeam() const
      { return _beams.size() == 1; }

    // Does this beam set contain multiple beams?
    Bool hasMultiBeam() const
      { return _beams.size() > 1; }

    // Is the beam set empty?
    Bool empty() const
      { return _beams.empty(); }

    // Get the shape of the beam array.
    const IPosition& shape() const
      { return _beams.shape(); }

    // Get the number of dimensions in the beam array.
    uInt ndim() const
      { return _beams.ndim(); }

    // Get the number of channels in the beam array (which is 1 or more).
    uInt nchan() const
      { return _beams.shape()[0]; }

    // Get the number of stokes in the beam array (which is 1 or more).
    uInt nstokes() const
      { return _beams.shape()[1]; }

    // Get the single global beam. If there are multiple beams,
    // an exception is thrown.
    const GaussianBeam& getBeam() const;

    // Get the beam at the specified location.
    // Note that a single channel or stokes in the beam set is valid for
    // all channels cq. stokes.
    // <group>
    const GaussianBeam& getBeam(Int chan, Int stokes) const;
    const GaussianBeam &operator()(Int chan, Int stokes) const
      { return getBeam (chan, stokes); }
    // </group>

    // Set the beam at the given location.
    // The location must be within the beam set shape.
    void setBeam(Int chan, Int stokes, const GaussianBeam& beam);

    // Resize the beam array.
    // A value 0 is silently changted to 1.
    void resize(uInt nchan, uInt nstokes);

    // Return a subset of the beam array.
    // The slicer is usually the slicer used for a subimage.
    // The slicer can contain multiple stokes or channels, even if the
    // beam set has only one.
    ImageBeamSet subset (const Slicer& imageSlicer,
                         const CoordinateSystem& csys) const;

    // Get the beam array.
    const Matrix<GaussianBeam>& getBeams() const
      { return _beams; }

    // Set the beams in this beam set.
    // The shape of the given array must match the beam set.
    // It also matches if an axis in array or beam set has length 1, which
    // means that it expands to the other length.
    void setBeams(const Matrix<GaussianBeam>& beams);

    // Set all beams to the same value.
    void set(const GaussianBeam& beam);

    // Get the beam in the set which has the smallest area.
    GaussianBeam getMinAreaBeam() const
      { return _minBeam; }

    // Get the beam in the set which has the largest area.
    GaussianBeam getMaxAreaBeam() const
      { return _maxBeam; }

    // Get the position of the beam with the minimum area.
    IPosition getMinAreaBeamPosition() const
      { return _minBeamPos; }

    // Get the position of the beam with the maximum area.
    IPosition getMaxAreaBeamPosition() const
      { return _maxBeamPos; }

    // Get the minimal, maximal, or median area beam for the given stokes
    // along the channel axis and set <src>pos</src> to its position in the
    // beam set.
    // If the stokes axis in the beam set has length 1, the same beam is
    // used for all stokes and no checking on stokes is done.
    // If the number of stokes in the beam matrix is >1, it is checked that
    // the given <src>stokes</src> stokes value is valid; if not,
    // an exception is thrown.
    // <group>
    const GaussianBeam& getMinAreaBeamForPol(IPosition& pos,
                                             uInt stokes) const;
    const GaussianBeam& getMaxAreaBeamForPol(IPosition& pos,
                                             uInt stokes) const;
    const GaussianBeam& getMedianAreaBeamForPol(IPosition& pos,
                                                uInt stokes) const;
    // </group>

    // Get a beam to which all other beams in the set can be convolved.
    // If all other beams can be convolved to the maximum area beam in the set,
    // that beam will be returned. If not, this is guaranteed to be the minimum
    // area beam to which all beams in the set can be convolved if all but one
    // of the beams in the set can be convolved to the beam in the set with the
    // largest area. Otherwise, the returned beam may or may not be the smallest
    // possible beam to which all the beams in the set can be convolved.
    GaussianBeam getCommonBeam() const;

    static const String& className();

  private:
    // Calculate and store the areas 0f all beams.
    void _calculateAreas();

    static void _transformEllipseByScaling
    (Double& transformedMajor, Double& transformedMinor,
     Double& transformedPa, Double major, Double minor,
     Double pa, Double xScaleFactor, Double yScaleFactor);

    //# Data members
    Matrix<GaussianBeam> _beams;
    Matrix<Double>       _areas;
    String               _areaUnit;
    GaussianBeam         _minBeam, _maxBeam;
    IPosition            _minBeamPos, _maxBeamPos;
    static const String _DEFAULT_AREA_UNIT;
  };

  // Show beam info.
  ostream &operator<<(ostream &os, const ImageBeamSet& beamSet);

} //# end namespace

#endif
