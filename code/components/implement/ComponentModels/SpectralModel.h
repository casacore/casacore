//# SpectralModel.h:
//# Copyright (C) 1998
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

#if !defined(AIPS_SPECTRALMODEL_H)
#define AIPS_SPECTRALMODEL_H

#include <aips/aips.h>
#include <trial/ComponentModels/ComponentType.h>

template <class T> class Vector;
class MFrequency;

// <summary></summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> SomeClass
//   <li> SomeOtherClass
//   <li> some concept
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class SpectralModel
{
public:
  // a virtual destructor is needed so that the actual destructor in the
  // derived class will be used.
  virtual ~SpectralModel();

  // return the actual spectral shape.
  virtual ComponentType::SpectralShape spectralShape() const = 0;

  // set/get the reference frequency
  // <group>
  virtual void setRefFrequency(const MFrequency & newRefFreq) = 0;
  virtual const MFrequency & refFrequency() const = 0;
  virtual void refFrequency(MFrequency & refFreq) const;
  // </group>

  // Return the scaling factor used to scale the flux of the component at the
  // specified frequency. The scaling factor is always 1 at the reference
  // frequency.
  virtual void scale(Double & scaleFactor, 
		     const MFrequency & centerFrequency) const;
  virtual Double scale(const MFrequency & centerFrequency) const = 0;

  // return the number of parameters in this spectral shape and set/get them.
  // <group>
  virtual uInt nSpectralParameters() const = 0;
  virtual void setSpectralParameters(const Vector<Double> & newParms) = 0;
  virtual void spectralParameters(Vector<Double> & compParms) const = 0;
  // </group>

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool ok() const = 0;
};
#endif
