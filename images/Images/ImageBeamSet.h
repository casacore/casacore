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
// a deconvolved image.
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
    ImageBeamSet(const Matrix<GaussianBeam>& beams);

    // Construct an ImageBeamSet representing a single beam wich is
    // valid for all channels and stokes.
    ImageBeamSet(const GaussianBeam& beam);

    // Create an ImageBeamSet of the specified shape with all
    // GaussianBeams initialized to null beams.
    ImageBeamSet(uInt nchan, uInt nstokes);
    ImageBeamSet(const IPosition& shape2D);

    // Create an ImageBeamSet of the specified shape with all
    // GaussianBeams initialized to <src>beam</src>.
    ImageBeamSet(uInt nchan, uInt nstokes, const GaussianBeam& beam);

    // The copy constructor.
    ImageBeamSet(const ImageBeamSet& other);

    ~ImageBeamSet();

    // Assignment can change the shape.
    ImageBeamSet& operator=(const ImageBeamSet& other);

    // Beam sets are equal if the shapes and all beams are equal.
    Bool operator== (const ImageBeamSet& other) const;
    Bool operator!= (const ImageBeamSet& other) const;

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

    // Get the number of channels in the beam array.
    uInt nchan() const
      { return _beams.shape()[0]; }

    // Get the number of stokes in the beam array.
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

    // Get a beam at the given 2-dim IPosition. It should match exactly,
    // thus a single channel or stokes in the beam set is not valid for all.
    const GaussianBeam& getBeam(const IPosition& pos) const
      { return _beams(pos); }

    // Set the beam at the given location.
    // The location must be within the beam set shape.
    void setBeam(uInt chan, uInt stokes, const GaussianBeam& beam);

    // Resize the beam array.
    void resize(uInt nchan, uInt nstokes);

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

    // Get the minimal area beam and its position in the beam set for
    // the given stokes. If the stokes axis has length 1,
    // it is valid for all stokes.
    const GaussianBeam& getMinAreaBeamForPol(IPosition& pos,
                                             uInt stokes) const;

    // Get the maximal area beam and its position in the beam set for
    // the given stokes. If the stokes axis has length 1,
    // it is valid for all stokes.
    const GaussianBeam& getMaxAreaBeamForPol(IPosition& pos,
                                             uInt stokes) const;

    // Get the median area beam and its position in the beam set for
    // the given stokes. If the stokes axis has length 1,
    // it is valid for all stokes.
    const GaussianBeam& getMedianAreaBeamForPol(IPosition& pos,
                                                uInt stokes) const;

    static const String& className();

  private:
    // Calculate and store the areas 0f all beams.
    void _calculateAreas();

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
