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
class GlishRecord;
class String;
class doubleG_COMPLEX;
typedef doubleG_COMPLEX DComplex;
template <class T> class Vector;
template <class T> class Flux;

// <summary>A point model for the spatial distribution of emission</summary>

// <use visibility=export> 

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=ComponentShape>ComponentShape</linkto>
// </prerequisite>

// <synopsis> 

// A PointShape object models the spatial distribution of radiation from the
// sky as a point source.

// The reference direction is defined in celestial co-ordinates, using a
// <linkto class=MDirection>MDirection</linkto> object. It indicates where the
// point is on the sky. The direction can be specified both in the constructor
// or with the <src>setRefDirection</src> function.

// The flux, or integrated intensity, is always one. This class does not model
// the actual flux or its variation with frequency. It solely models the way
// the emission varies with position on the sky.

// The <src>scale</src> member function is used to sample the component at any
// point on the sky. The scale factor calculated by this function is the
// proportion of the flux that is within a specified pixel size centered on the
// specified direction. It will return a value of one if the sample direction
// is co-incident, to within half a pixel with the direction of the point
// source. Otherwise it will return zero.

// This class contains functions that return the Fourier transform of the
// component at a specified spatial frequency. There are described more fully
// in the description of the <src>visibility</src> functions below.

// This class also contains functions which perform the conversion between
// Records and PointShape objects. This defines how a PointShape object is
// represented in glish. The format of the record that is generated and
// accepted by these functions is:
// <srcblock>
// c := [type = "point",
//       direction = [type = "direction",
//                    refer = "j2000",
//                    m0 = [value = 0, unit = "deg"]
//                    m1 = [value = 0, unit = "deg"]
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
// Flux<Double> J1934_flux(6.28, 0.1, 0.15, 0.01);
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

  // Calculate the flux at the specified direction, in a pixel of specified
  // size, given the total flux of the component. The total flux of the
  // component must be supplied in the flux variable and the proportion of the
  // flux in the specified pixel is returned in the same variable.
  //
  // Because this is a point shape this function will not change the flux
  // unless the direction is more than half a pixelSize away from the reference
  // direction. Then the returned flux is zero.
  virtual void sample(Flux<Double> & flux, const MDirection & direction, 
		      const MVAngle & pixelSize) const;

  // Return the Fourier transform of the component at the specified point in
  // the spatial frequency domain. The point is specified by a 3 element vector
  // (u,v,w) that has units of meters and the frequency of the observation, in
  // Hertz. These two quantities can be used to derive the required spatial
  // frequency <src>(s = uvw*freq/c)</src>. The w component is not used in
  // these functions.

  // The reference position for the transform is the direction of the
  // component. Hence the transform is always a constant real value of one.

  // The total flux of the component must be supplied in the flux variable and
  // the corresponding visibility is returned in the same variable.
  virtual void visibility(Flux<Double> & flux, const Vector<Double> & uvw,
			  const Double & frequency) const;

  // Return a pointer to a copy of this object upcast to a ComponentShape
  // object. The class that uses this function is responsible for deleting the
  // pointer. This is used to implement a virtual copy constructor.
  virtual ComponentShape * cloneShape() const;

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
  virtual Bool fromRecord(String & errorMessage, const GlishRecord & record);
  virtual Bool toRecord(String & errorMessage, RecordInterface & record) const;
  virtual Bool toRecord(String & errorMessage, GlishRecord & record) const;
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
