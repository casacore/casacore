//# LimbDarkenedDiskShape.h: defines LimbDarkened Disk shape
//
//#  CASA - Common Astronomy Software Applications (http://casa.nrao.edu/)
//# Copyright (C) 2012
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Lesser General Public License as published by
//# the Free Software Foundation; either version 2.1 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Lesser General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
//#
//#
//#
//# $Id: LimbDarkenedDiskShape.h 23251 2013-03-15 23:57:28Z tak.tsutsumi $

#ifndef COMPONENT_LIMBDARKENED_DISKSHAPE_H
#define COMPONENT_LIMBDARKENED_DISKSHAPE_H

//#! Includes go here
#include <casa/aips.h>
#include <casa/BasicSL/Complex.h>
#include <components/ComponentModels/TwoSidedShape.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations

// <summary>A limb-darkened disk model for the spatial distribution of emission</summary>

//<use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li><linkto class=TwoSidedShape>TwoSidedShape</linkto>
// </prerequisite>
//
// <etymology>
//#! Except when it is obvious (e.g., "Array") explain how the class name
//#! expresses the role of this class.  Example: IPosition is short for
//#! "Integral Position" - a specialized integer vector for specifying
//#! array dimensions and indices.
// </etymology>
//
// <synopsis>
// A LimbDarkenedDiskShape models the spatial distribution of radiation from 
// the sky as a using a limb-darkened elliptical disk with user specified major axis
// width, minor axis width and position angle.
//
// This class like the other component shapes becomes more useful when used
// through the <linkto class=SkyComponent>SkyComponent</linkto> class, which
// incorperates the flux and spectral variation of the emission, or through the
// <linkto class=ComponentList>ComponentList</linkto> class, which handles
// groups of SkyComponent objects.
//
// <note role=caution> This class requires the GSL package. If that cannot
// be found, a run-time error is given when the constructor is called.
// </note>
// </synopsis>
//
// <example>
//#! One or two concise (~10-20 lines) examples, with a modest amount of
//#! text to support code fragments.  Use <srcblock> and </srcblock> to
//#! delimit example code.
// </example>
//
// <motivation>
//#! Insight into a class is often provided by a description of
//#! the circumstances that led to its conception and design.  Describe
//#! them here.
// </motivation>
//
// <templating arg=T>
//#! A list of member functions that must be provided by classes that
//#! appear as actual template arguments.  For example:  imagine that you
//#! are writing a templated sort class, which does a quicksort on a
//#! list of arbitrary objects.  Anybody who uses your templated class
//#! must make sure that the actual argument class (say, Int or
//#! String or Matrix) has comparison operators defined.
//#! This tag must be repeated for each template formal argument in the
//#! template class definition -- that's why this tag has the "arg" attribute.
//#! (Most templated classes, however, are templated on only a single
//#! argument.)
//    <li>
//    <li>
// </templating>
//
// <thrown>
//#! A list of exceptions thrown if errors are discovered in the function.
//#! This tag will appear in the body of the header file, preceding the
//#! declaration of each function which throws an exception.
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//#! A List of bugs, limitations, extensions or planned refinements.
//#! The programmer should fill in a date in the "asof" field, which
//#! will usually be the date at which the class is submitted for review.
//#! If, during the review, new "todo" items come up, then the "asof"
//#! date should be changed to the end of the review period.
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class LimbDarkenedDiskShape: public TwoSidedShape
{
public:

//#! Please arrange class members in the sections laid out below.  A
//#! possible exception (there may be others) -- some operator functions
//#! may perform the same task as "General Member Functions", and so you
//#! may wish to group them together.

//#! Friends

//#! Enumerations

//#! Constructors
  //Default constsructor
  LimbDarkenedDiskShape();

  LimbDarkenedDiskShape(const MDirection& direction,
            const Quantum<Double>& majorAxis,
            const Quantum<Double>& minorAxis,
            const Quantum<Double>& positionAngle,
            const Float& n);
  LimbDarkenedDiskShape(const MDirection& direction, const Quantum<Double>& width,
            const Double axialRatio,
            const Quantum<Double>& positionAngle,
            const Float& n);
  // </group>

  // The copy constructor 
  LimbDarkenedDiskShape(const LimbDarkenedDiskShape& other);

  // The destructor
  virtual ~LimbDarkenedDiskShape();

  //#! Operators
  //The assignment operator
  LimbDarkenedDiskShape& operator=(const LimbDarkenedDiskShape& other);

  //#! General Member Functions
  // get the type of the shape (always returns ComponentType::LimbDakenedDisk)
  virtual ComponentType::Shape type() const;

  // use diskshape ones?
  virtual void setWidthInRad(const Double majorAxis,
                             const Double minorAxis,
                             const Double positionAngle);
  virtual Double majorAxisInRad() const;
  virtual Double minorAxisInRad() const;
  virtual Double positionAngleInRad() const;
  virtual Float getAttnFactor() const;
  //set n factor in darkening equation, I=I0(1-rho^2)^n/2
  virtual void setAttnFactor(const Float attnFactor);  
  virtual Vector<Double> optParameters() const;
  virtual void setOptParameters(const Vector<Double>& newOptParms);

  // Calculate the proportion of the flux that is in a pixel of specified size
  // centered in the specified direction. The returned value will always be
  // between zero and one (inclusive).
  virtual Double sample(const MDirection& direction,
                        const MVAngle& pixelLatSize,
                        const MVAngle& pixelLongSize) const;


  // Same as the previous function except that many directions can be sampled
  // at once. The reference frame and pixel size must be the same for all the
  // specified directions.
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
  // component. As this component is symmetric about this point the transform
  // is always a real value.
  virtual DComplex visibility(const Vector<Double>& uvw,
                              const Double& frequency) const;


  // Same as the previous function except that many (u,v,w) points can be
  // sampled at once. The uvw Matrix must have a first dimension of three, and
  // a second dimension that is the same as the length of the scale
  // Vector. Otherwise and exception is thrown (when compiled in debug mode).
  virtual void visibility(Vector<DComplex>& scale, const Matrix<Double>& uvw,
                          const Double& frequency) const;

  //same as above except with many frequencies
  virtual void visibility(Matrix<DComplex>& scale, const Matrix<Double>& uvw,
                          const Vector<Double>& frequency) const;

  // Return a pointer to a copy of this object upcast to a ComponentShape
  // object. The class that uses this function is responsible for deleting the
  // pointer. This is used to implement a virtual copy constructor.
  virtual ComponentShape* clone() const;

  // Function which checks the internal data of this class for correct
  // dimensionality and consistent values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool ok() const;

  // return a pointer to this object.
  virtual const ComponentShape* getPtr() const;

  virtual String sizeToString() const;

private:
  Double calcSample(const MDirection::MVType& compDirValue,
                    const MDirection::MVType& dirVal,
                    const Double majRad, const Double minRad,
                    const Double pixValue) const;

  Double calcVis(Double u, Double v, const Double factor) const;
  static void rotateVis(Double& u, Double& v,
                        const Double cpa, const Double spa);

  //# The parameters of the limb-darkened disk
  // <group>
  Double itsMajValue;
  Double itsMinValue;
  Double itsPaValue;
  Double itsHeight;
  Float  itsAttnFactor;  
  // </group>
};

} //# NAMESPACE CASA - END

#endif
