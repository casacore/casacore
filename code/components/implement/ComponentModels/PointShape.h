//# PointShape.h:
//# Copyright (C) 1998,1999,2000
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

#if !defined(AIPS_POINTSHAPE_H)
#define AIPS_POINTSHAPE_H

#include <aips/aips.h>
#include <trial/ComponentModels/ComponentShape.h>
#include <trial/ComponentModels/ComponentType.h>
#include <aips/Mathematics/Complex.h>

class MVAngle;
class MDirection;
class RecordInterface;
class String;
template <class T> class Vector;

// <summary>A shape where emission comes from only one direction</summary>

// <use visibility=export> 

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tPointShape" demos="dPointShape">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=ComponentShape>ComponentShape</linkto>
// </prerequisite>

// <synopsis> 

// This class represents the shape of components where the emission comes from
// only one point in the sky. 

// This class like the other component shapes becomes more useful when used
// through the <linkto class=SkyComponent>SkyComponent</linkto> class, which
// incorperates the flux and spectral variation of the emission, or through the
// <linkto class=ComponentList>ComponentList</linkto> class, which handles
// groups of SkyComponent objects.

// The reference direction for a point shape is the direction at which all the
// emission comes from and is defined in celestial co-ordinates, using a
// <linkto class=MDirection>MDirection</linkto> object.  The direction can be
// specified both in the constructor or with the <src>setRefDirection</src>
// function.

// The <src>sample</src> member functions are used to determine the proportion
// of flux the component at any point on the sky. For a point component this is
// either zero or one depending on whether the specified pixel contains the
// point source or not.

// The <src>visibility</src> functions treturn the Fourier transform of the
// component at a specified spatial frequency. For a point shape the Fourier
// transform is a constant value. Hence these functions return one, regardless
// of the input parameters.

// This class also contains functions (<src>toRecord</src> &
// <src>fromRecord</src>) which perform the conversion between Records and
// PointShape objects. These functions define how a PointShape object is
// represented in glish. The format of the record that is generated and
// accepted by these functions is:
// <srcblock>
// c := [type = 'point',
//       direction = [type = 'direction',
//                    refer = 'j2000',
//                    m0 = [value = 0, unit = 'deg']
//                    m1 = [value = 0, unit = 'deg']
//                   ]
//      ]
// </srcblock>
// The direction field contains a record representation of a direction measure
// and its format is defined in the Measures module. Its refer field defines
// the reference frame for the direction and the m0 and m1 fields define the
// latitude and longitude in that frame.
// </synopsis>
//

// <example>
// Suppose I had an image of a region of the sky and we wanted to subtract
// a point source from it. This could be done as follows:
// <li> 
// <li> Construct a SkyComponent with to represent the point source
// <li> Project the component onto an image
// <li> Convolve the image by the point spread function
// <li> subtract the convolved model from the dirty image.
// </li>
// Shown below is the code to perform the first step in this process, ie
// construct the SkyComponent. This example is also available in the
// <src>dPointShape.cc</src> file.  Note that it is more accurate to do
// subtraction of point components in the (u,v) domain
// <srcblock>
//  MDirection J1934_dir;
//  { // get the right direction into J1934_dir
//    Quantity J1934_ra; MVAngle::read(J1934_ra, "19:39:");
//    Quantity J1934_dec; MVAngle::read(J1934_dec, "-63.43.");
//    J1934_dir = MDirection(J1934_ra, J1934_dec, MDirection::J2000);
//  }
//  { // One way to construct the SkyComponent
//    SkyComponent J1934(ComponentType::POINT, ComponentType::CONSTANT_SPECTRUM);
//    J1934.shape().setRefDirection(J1934_dir);
//    J1934.flux() = Flux<Double>(6.28, 0.1, 0.15, 0.01);
//    printShape(J1934.shape());
//  }
//  { // An alternative way to construct the SkyComponent
//    const Flux<Double> flux(6.28, 0.1, 0.15, 0.01);
//    const PointShape shape(J1934_dir);
//    const ConstantSpectrum spectrum;
//    SkyComponent component(flux, shape, spectrum);
//    printShape(component.shape());
//  }
// </srcblock>
// Note how the member functions of the PointShape class (the setDirection
// function) are accessable through the shape function in the SkyComponent
// class. The printShape function is the example shown for the ComponentShape
// class.
// </example>
//
// <todo asof="1999/11/11">
//   <li> Use Measures & Quanta in the interface to the visibility functions.
// </todo>

// <linkfrom anchor="PointShape" 
//           classes="ComponentShape GaussianShape DiskShape">
//  <here>PointShape</here>
// - a shape where emission comes from only one direction
// </linkfrom>

class PointShape: public ComponentShape
{
public:
  // The default PointShape is at the J2000 North Pole.
  PointShape();
  
  // Construct a point shape at the specified direction.
  PointShape(const MDirection& direction);
  
  // The copy constructor uses copy semantics.
  PointShape(const PointShape& other);

  // The destructor does nothing special.
  virtual ~PointShape();

  // The assignment operator uses copy semantics.
  PointShape& operator=(const PointShape& other);

  // Return the type of shape. This function always returns
  // ComponentType::POINT.
  virtual ComponentType::Shape type() const;

  // Calculate the proportion of the flux that is in a pixel of the specified
  // size centered on the specified direction. Because this is a point shape
  // the returned value is either zero or one.  It is one if the specified
  // direction is less than half a pixelSize away from the reference direction.
  virtual Double sample(const MDirection& direction, 
			const MVAngle& pixelLatSize,
			const MVAngle& pixelLongSize) const;

  // Same as the previous function except that many directions can be sampled
  // at once. The reference frame and pixel size must be the same for all the
  // specified directions. This is a customised version.
  virtual void sample(Vector<Double>& scale, 
		      const Vector<MDirection::MVType>& directions, 
		      const MDirection::Ref& refFrame, 
		      const MVAngle& pixelLatSize,
		      const MVAngle& pixelLongSize) const;

  // Return the Fourier transform of the component at the specified point in
  // the spatial frequency domain. The point is specified by a 3 element vector
  // (u,v,w) that has units of meters and the frequency of the observation, in
  // Hertz. These two quantities can be used to derive the required spatial
  // frequency <src>(s = uvw*freq/c)</src>. The w component is not used in
  // these functions.

  // The reference position for the transform is the direction of the
  // component. Hence the returned value is always a constant real value of
  // one.  The input arguments are ignored except in debug mode where the
  // length of the uvw Vector and sign of the frequency variable are checked.
  virtual DComplex visibility(const Vector<Double>& uvw,
			      const Double& frequency) const;

  // Same as the previous function except that many (u,v,w) points can be
  // sampled at once. As with the previous function the returned value is
  // always a constant real vector of one.  The input arguments are ignored
  // except in debug mode where the shape of the uvw Matrix and the scale
  // Vector are checked as is the  sign of the frequency variable.
  virtual void visibility(Vector<DComplex>& scale, const Matrix<Double>& uvw,
			  const Double& frequency) const;

  // A point shape is symmetric so this function always returns True;
  virtual Bool isSymmetric() const;

  // Return a pointer to a copy of this object upcast to a ComponentShape
  // object. The class that uses this function is responsible for deleting the
  // pointer. This is used to implement a virtual copy constructor.
  virtual ComponentShape* clone() const;

  // return the number of parameters in this shape and set/get them.  As this
  // is a point shape there are none. So calling <src>setParameters</src> or
  // <src>parameters</src> with anything other than a zero length Vector will
  // throw an exception (when compiled in debug mode).
  // <group>
  virtual uInt nParameters() const;
  virtual void setParameters(const Vector<Double>& newParms);
  virtual void parameters(Vector<Double>& compParms) const;
  // </group>

  // This functions convert between a Record and a PointShape. These functions
  // define how a point shape is represented in glish and this is detailed in
  // the synopsis above.  They return False if the supplied Record is malformed
  // and append an error message to the supplied String giving the reason.
  // <group>
  virtual Bool fromRecord(String& errorMessage,
			  const RecordInterface& record);
  virtual Bool toRecord(String& errorMessage, RecordInterface& record) const;
  // </group>

  // Convert the parameters of the component to the specified units. As a point
  // component has no parameters this function does nothing and always returns
  // True.
  virtual Bool convertUnit(String&, const RecordInterface&);

  // Function which checks the internal data of this class for consistent
  // values. Returns True if everything is fine otherwise returns False.
  virtual Bool ok() const;
};
#endif
