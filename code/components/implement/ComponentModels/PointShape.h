//# PointShape.h:
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
//#
//# $Id$

#if !defined(AIPS_POINTSHAPE_H)
#define AIPS_POINTSHAPE_H

#include <aips/aips.h>
#include <trial/ComponentModels/ComponentShape.h>
#include <trial/ComponentModels/ComponentType.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MVDirection.h>

class MVAngle;
class RecordInterface;
class String;
class doubleG_COMPLEX;
typedef doubleG_COMPLEX DComplex;
template <class T> class Vector;

// <summary>A point model for the spatial distribution of flux</summary>

// <use visibility=export> 

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tPointCompRep" demos="dPointCompRep">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=SkyCompRep>SkyCompRep</linkto>
//   <li> <linkto class=MDirection>MDirection</linkto>
//   <li> <linkto class=Flux>Flux</linkto>
//   <li> <linkto class=Vector>Vector</linkto>
// </prerequisite>

// <synopsis> 

// A PointCompRep models radiation from the sky as a point source with a user
// specified direction and flux.

// The direction is defined in celestial co-ordinates, using a
// <linkto class=MDirection>MDirection</linkto> object, and converted
// internally to J2000 coordinates. The direction can be specified both in the
// constructor or with the <src>setDirection</src> function. Similarly the flux
// of the component can be specified with the <src>setFlux</src> function or
// defined at construction time.

// The height, which for a point source is the same as the flux, or integrated
// intensity, is specified using a Flux object.

// The <src>sample</src> member function is used to sample the component at any
// point on the sky and the project function is used to project the component
// onto a supplied Image (ie. anything derived from ImageInterface). Because
// the class behaves as a true delta function it will only return a non-zero
// value if the sample direction is co-incident, to within half a pixel with
// the direction of the point source. The pixel size must be specified when
// using the sample function. 

// When using the <src>project</src> function the entire flux of the point
// component is projected onto the pixel nearest to its true direction. This
// may shift the direction of the point component by up to half a pixel in both
// direction axes.

// This class also contains functions which perform the conversion between
// glish records and PointCompRep objects. The format of the glish record that
// is generated and accepted by these functions is:
// <srcblock>
// c := [type = "point",
//       flux = [value = [1,.5,.2,.1],
//               unit = "Jy"]
//       position = [type = "direction",
//                   refer = "j2000",
//                   m0 = [value = 0, unit = "deg"]
//                   m1 = [value = 0, unit = "deg"]
//                   ev0 = [value = 1, unit = ""]
//                   ev1 = [value = 0, unit = ""]
//                   ev2 = [value = 0, unit = ""]
//                  ]
//      ]
// </srcblock>
// The refer field defines the reference frame for the component and the m0 and
// m1 fields define the latitude and longitutde in that frame. If the m0 or m1
// fields and missing or malformed the ev0, ev1 and ev2 fields are parsed to
// extract the direction. These contain the direction cosines.

// This class differs from the
// <linkto class=PointComponent>PointComponent</linkto> class in that it uses
// copy semantics. This means that both the copy constructor and the assignment
// operator make a physical copy of the component and that changing the
// component DOES NOT change other copies of it. If instead you want reference
// semantics you should use the PointComponent class.  This class
// (PointCompRep) is the one that is used by the PointComponent class to do all
// the work.

// </synopsis>
//
// <example>
// Suppose I had an image of a region of the sky and we wanted to subtract
// a point source from it. This could be done as follows:
// <ul> 
// <li> Construct a PointCompRep to represent the point source
// <li> Project the component onto an image
// <li> Convolve the image by the point spread function
// <li> subtract the convolved model from the dirty image.
// </ul>
// Shown below is the code to perform the first two steps in this process. See
// the <linkto class=Convolver>Convolver</linkto> class and the
// <linkto module=Lattice>Lattice</linkto> module for the functions necessary
// to perform the last two items. This example is also available in the
// <src>dPointCompRep.cc</src> file.
// <srcblock>
// Quantity J1934_ra = Quantity(19.0/24*360, "deg") + Quantity(39, "'");
// Quantity J1934_dec = Quantity(-63, "deg") + Quantity(43, "'");
// MDirection J1934_dir(J1934_ra, J1934_dec, MDirection::J2000);
// ComponentFlux<Double> J1934_flux(6.28, 0.1, 0.15, 0.01);
// PointCompRep J1934(J1934_flux, J1934_dir);
// // This component can now be projected onto an image
// CoordinateSystem coords;
// {
//   Double pixInc = Quantity(1, "''").getValue("rad");
//   Matrix<Double> xform(2,2);
//   xform = 0.0; xform.diagonal() = 1.0;
//   Double refPixel = 32.0;
//   DirectionCoordinate dirCoord(MDirection::J2000,
// 				 Projection(Projection::SIN),
// 				 J1934_ra.getValue("rad"),
// 				 J1934_dec.getValue("rad"),
// 				 pixInc , pixInc, xform,
// 				 refPixel, refPixel);
//   coords.addCoordinate(dirCoord);
// }
// CoordinateUtil::addIQUVAxis(coords);
// CoordinateUtil::addFreqAxis(coords);
// PagedImage<Float> skyModel(IPosition(4,64,64,4,8), coords, 
// 			     "model_tmp.image");
// skyModel.set(0.0f);
// J1934.project(skyModel);
// </srcblock>
// </example>
//
// <todo asof="1997/07/16">
//   <li> Nothing so far
// </todo>

// <linkfrom anchor="PointCompRep" classes="SkyCompRep PointComponent">
//  <here>PointCompRep</here> - a point component with copy semantics
// </linkfrom>

class PointShape: public ComponentShape
{
public:
  // The default PointShape is at the J2000 North Pole.
  PointShape();

  // Construct a point shape at the specified direction.
  // flux argument.
  PointShape(const MDirection & direction);

  // The copy constructor uses copy semantics.
  PointShape(const PointShape & other);

  // The destructor does nothing.
  virtual ~PointShape();

  // The assignment operator uses copy semantics.
  PointShape & operator=(const PointShape & other);

  // get the shape of the component. This function always returns
  // ComponentType::POINT.
  virtual ComponentType::Shape shape() const;

  // set/get the reference direction of the point.
  // <group>
  virtual void setRefDirection(const MDirection & newRefDir);
  virtual const MDirection & refDirection() const;
  // </group>

  // Return the scaling factor that indicates the proportion of the flux that
  // is in the specified pixel in the specified direction. The pixels are
  // assumed to be square.
  //
  // Because this is a point shape this function will return one whenever the
  // direction and pixelSize contain the point. Otherwise it will return zero.
  virtual Double scale(const MDirection & direction, 
		       const MVAngle & pixelSize) const;

  // Return the Fourier transform of the component at the specified point in
  // the spatial frequency domain. The point is specified by a 3 element vector
  // (u,v,w) that has units of meters and the frequency of the observation, in
  // Hertz. These two quantities can be used to derive the required spatial
  // frequency <src>(s = uvw*freq/c)</src>. The w component is not used in
  // these functions.

  // The reference position for the transform is the direction of the
  // component. Hence the transform is always a constant real value of one.
  // <group>
  virtual void visibility(DComplex & result, const Vector<Double> & uvw,
			  const Double & frequency) const;
  virtual Double visibility(const Vector<Double> & uvw,
			    const Double & frequency) const;
  // </group>

  // always returns True as a Point source is symmetric.
  virtual Bool isSymmetric();

  // return the number of parameters in this shape and set/get them.
  //
  // As this is a point shape there are none. So calling
  // <src>setShapeParameters</src> or <src>shapeParameters</src> with anything
  // other than a zero length Vector will throw an exception.
  // <group>
  virtual uInt nShapeParameters() const;
  virtual void setShapeParameters(const Vector<Double> & newParms);
  virtual void shapeParameters(Vector<Double> & compParms) const;
  // </group>

  // This functions convert between a record and a PointShape. These functions
  // define how a point shape is represented in glish and this is detailed in
  // the synopsis above.  They return False if the glish record is malformed
  // and append an error message to the supplied string giving the reason.
  // <group>
  virtual Bool fromRecord(String & errorMessage,
			  const RecordInterface & record);
  virtual Bool toRecord(String & errorMessage,
			RecordInterface & record) const;
  // </group>

  // Function which checks the internal data of this class for correct
  // dimensionality and consistent values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool ok() const;

private:
  MDirection itsDir;
  MVDirection itsDirValue;
  MDirection::Types itsRefFrame;
};
#endif
