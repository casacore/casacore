//# TabularCoordinate.cc: Table lookup 1-D coordinate, with interpolation
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


#include <casacore/coordinates/Coordinates/TabularCoordinate.h>

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/MatrixMath.h>
#include <casacore/coordinates/Coordinates/LinearCoordinate.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/LinearSearch.h>
#include <casacore/scimath/Functionals/Interpolate1D.h>
#include <casacore/scimath/Functionals/ScalarSampledFunctional.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Quanta/Quantum.h>

#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


TabularCoordinate::TabularCoordinate()
: Coordinate(),
  crval_p(0), 
  cdelt_p(1), 
  crpix_p(0), 
  matrix_p(1.0), 
  unit_p(""),
  name_p("Tabular"), 
  channel_corrector_p(0), 
  channel_corrector_rev_p(0)
{
   setDefaultWorldMixRanges();
}

TabularCoordinate::TabularCoordinate(double refval, double inc, double refpix,
				     const String &unit, const String &axisName)
: Coordinate(),
  crval_p(refval), 
  cdelt_p(inc), 
  crpix_p(refpix), 
  matrix_p(1.0), 
  unit_p(unit),
  name_p(axisName), 
  channel_corrector_p(0), 
  channel_corrector_rev_p(0)
{
   setDefaultWorldMixRanges();
}

TabularCoordinate::TabularCoordinate(const Quantum<double>& refval,
                                     const Quantum<double>& inc,   
                                     double refpix, const String &axisName)
: Coordinate(),
  crpix_p(refpix), 
  matrix_p(1.0), 
  name_p(axisName), 
  channel_corrector_p(0), 
  channel_corrector_rev_p(0)
{
// Check and assign

   if (refval.isConform(inc)) {
      crval_p = refval.getValue();
      unit_p = refval.getUnit();

// Convert inc to units of reference value

      cdelt_p = inc.getValue(Unit(unit_p));
   } else {
      throw (AipsError("Units of reference value and increment inconsistent"));
   }
//
   setDefaultWorldMixRanges();
}


TabularCoordinate::TabularCoordinate(const Vector<double> &pixelValues,
				     const Vector<double> &worldValues,
				     const String &unit, const String &axisName)
: Coordinate(),
  crval_p(0.0), 
  cdelt_p(0.0), 
  crpix_p(0.0), 
  matrix_p(0.0), 
  unit_p(unit), 
  name_p(axisName), 
  channel_corrector_p(0), 
  channel_corrector_rev_p(0)
{
   makeNonLinearTabularCoordinate(pixelValues, worldValues);
   setDefaultWorldMixRanges();   
}

TabularCoordinate::TabularCoordinate(const Vector<double>& pixelValues,
                                     const Quantum<Vector<double> >& worldValues,
                                     const String &axisName)
: Coordinate(),
  crval_p(0.0), 
  cdelt_p(0.0), 
  crpix_p(0.0), 
  matrix_p(0.0), 
  name_p(axisName), 
  channel_corrector_p(0), 
  channel_corrector_rev_p(0)
{
   unit_p = worldValues.getUnit();
   Vector<double> world = worldValues.getValue();
   makeNonLinearTabularCoordinate(pixelValues, world);
   setDefaultWorldMixRanges();   
}


void TabularCoordinate::clear_self()
{
    crval_p = cdelt_p = crpix_p = matrix_p = -999.0;
    unit_p = "UNSET";
    name_p = "UNSET";
    if (channel_corrector_p) {
	delete channel_corrector_p;
	delete channel_corrector_rev_p;
    }
    channel_corrector_p = channel_corrector_rev_p = 0;
}


TabularCoordinate::TabularCoordinate(const TabularCoordinate &other)
: Coordinate(),
  crval_p(0.0), 
  cdelt_p(0.0), 
  crpix_p(0.0), 
  matrix_p(0.0), 
  unit_p("UNSET"),
  name_p("UNSET"), 
  channel_corrector_p(0), 
  channel_corrector_rev_p(0)
{
    copy(other);
}

TabularCoordinate &TabularCoordinate::operator=(const TabularCoordinate &other)
{
    if (this != &other) {
	copy(other);
    }
    return *this;
}

void TabularCoordinate::copy(const TabularCoordinate &other)
{
    clear_self();
    Coordinate::operator=(other);
    crval_p = other.crval_p;
    cdelt_p = other.cdelt_p;
    crpix_p = other.crpix_p;
    unit_p = other.unit_p;    
    name_p = other.name_p;
    matrix_p = other.matrix_p;
    if (other.channel_corrector_p != 0) {
	channel_corrector_p = 
	    new Interpolate1D<double,double>(*other.channel_corrector_p);
	channel_corrector_rev_p = 
	    new Interpolate1D<double,double>(*other.channel_corrector_rev_p);
	AlwaysAssert(channel_corrector_p != 0 &&
		     channel_corrector_rev_p != 0, AipsError);
    }
}


TabularCoordinate::~TabularCoordinate()
{
    clear_self();
}

Coordinate::Type TabularCoordinate::type() const
{
    return Coordinate::TABULAR;
}

String TabularCoordinate::showType() const
{
    return String("Tabular");
}

uint32_t TabularCoordinate::nPixelAxes() const
{
    return 1;
}

uint32_t TabularCoordinate::nWorldAxes() const
{
    return 1;
}

bool TabularCoordinate::toWorld(double& world, double pixel) const
{
    if (channel_corrector_p) {
	pixel = (*channel_corrector_p)(pixel);
    }
    world = crval_p + cdelt_p * matrix_p * (pixel - crpix_p);
    return true;
}

bool TabularCoordinate::toPixel(double &pixel, double world) const
{
    pixel = (world - crval_p)/(cdelt_p * matrix_p) + crpix_p;
    if (channel_corrector_rev_p) {
	pixel = (*channel_corrector_rev_p)(pixel);
    }
    return true;
}

bool TabularCoordinate::toWorld(Vector<double> &world, 
  	                        const Vector<double> &pixel, bool) const
{
   bool rval = true;
   world.resize(pixel.nelements());
   for(uint32_t i=0; i<pixel.nelements(); i++){
     rval = toWorld(world(i), pixel(i));
     if(!rval){
       break;
     }
   }
   return rval;
}

bool TabularCoordinate::toPixel(Vector<double> &pixel, 
                                const Vector<double> &world) const
{
   DebugAssert(world.nelements()==1,AipsError);
//
   pixel.resize(1);
   return toPixel(pixel(0), world(0));
}


bool TabularCoordinate::toWorldMany(Matrix<double>& world,
                                    const Matrix<double>& pixel,
                                    Vector<bool>& failures) const
{
    const uint32_t nTransforms = pixel.ncolumn();
    double alpha = cdelt_p * matrix_p;
    double beta = crval_p -alpha * crpix_p;
//
    world.resize(nWorldAxes(),nTransforms);
    Vector<double> worlds(world.row(0));     // Only 1 axis in TC
    Vector<double> pixels(pixel.row(0));
//
    if (channel_corrector_p) {
       for (uint32_t j=0; j<nTransforms; j++) { 
          worlds[j] = beta + alpha*((*channel_corrector_p)(pixels[j]));
       }
    } else {
       for (uint32_t j=0; j<nTransforms; j++) { 
          worlds[j] = beta + alpha*pixels[j];
       }
    }
//
    failures.resize(nTransforms);
    failures = false;
    return true;
}


bool TabularCoordinate::toPixelMany (Matrix<double>& pixel,
                                    const Matrix<double>& world,
                                    Vector<bool>& failures) const
{
    const uint32_t nTransforms = world.ncolumn();
    double alpha = cdelt_p * matrix_p;
    double beta = crpix_p - crval_p / alpha;
//
    pixel.resize(nPixelAxes(),nTransforms);
    Vector<double> worlds(world.row(0));      // Only 1 axis in TC
    Vector<double> pixels(pixel.row(0));
//
    if (channel_corrector_rev_p) {
       for (uint32_t j=0; j<nTransforms; j++) { 
          pixels[j] = worlds[j]/alpha + beta;
          pixels[j] = (*channel_corrector_rev_p)(pixels[j]);
       }
    } else {
       for (uint32_t j=0; j<nTransforms; j++) { 
          pixels[j] = worlds[j]/alpha + beta;
       }
    }
//
    failures.resize(nTransforms);
    failures = false;
    return true;
}




Vector<String> TabularCoordinate::worldAxisNames() const
{
    Vector<String> tmp(1);
    tmp(0) = name_p;
    return tmp;
}

Vector<String> TabularCoordinate::worldAxisUnits() const
{
    Vector<String> tmp(1);
    tmp(0) = unit_p;
    return tmp;
}

Vector<double> TabularCoordinate::referencePixel() const
{
    Vector<double> tmp(1);
    tmp(0) = crpix_p;
    return tmp;
}

Vector<double> TabularCoordinate::referenceValue() const
{
    Vector<double> tmp(1);
    tmp(0) = crval_p;
    return tmp;
}

Vector<double> TabularCoordinate::increment() const
{
    Vector<double> tmp(1);
    tmp(0) = cdelt_p;
    return tmp;
}

Matrix<double> TabularCoordinate::linearTransform() const
{
    Matrix<double> tmp(1,1);
    tmp(0,0) = matrix_p;
    return tmp;
}

bool TabularCoordinate::setWorldAxisNames(const Vector<String> &names)
{
    bool ok = (names.nelements()==1);
    if (!ok) {
       set_error ("names vector must be of length 1");
    } else {
       name_p = names(0);
    }
    return ok;
}

bool TabularCoordinate::setWorldAxisUnits(const Vector<String> &units)
{
    bool ok = (units.nelements()==1);
    if (!ok) {
       set_error ("units vector must be of length 1");
    } else {
       Vector<double> d1 = increment();
       ok = Coordinate::setWorldAxisUnits(units);
       if (ok) {
          unit_p = units(0);
//
          Vector<double> d2 = increment();
          worldMin_p *= d2 / d1;
          worldMax_p *= d2 / d1;
       }
    }
    return ok;
}

bool TabularCoordinate::overwriteWorldAxisUnits(const Vector<String> &units)
{
   bool ok = (units.nelements()==1);
   if (ok) {
      unit_p = units(0);
   } else {
      set_error ("units vector must be of length 1");
   }
   return ok;
}


bool TabularCoordinate::setReferencePixel(const Vector<double> &refPix)
{
    bool ok = (refPix.nelements()==1);
    if (!ok) {
       set_error ("reference pixel vector must be of length 1");
    } else {
       crpix_p = refPix(0);
    }
    return ok;
}

bool TabularCoordinate::setLinearTransform(const Matrix<double> &xform)
{
    bool ok = (xform.nelements()==1);
    if (!ok) {
       set_error ("linear transform matrix must be of length 1");
    } else {
       matrix_p = xform(0,0);
    }
    return ok;
}

bool TabularCoordinate::setIncrement(const Vector<double> &inc) 
{
    bool ok = (inc.nelements()==1);
    if (!ok) {
       set_error ("increment vector must be of length 1");
    } else {
       cdelt_p = inc(0);
    }
    return ok;
}

bool TabularCoordinate::setReferenceValue(const Vector<double> &refval)
{
    bool ok = (refval.nelements()==1);
    if (!ok) {
       set_error ("reference values vector must be of lenth 1");
    } else {
       crval_p = refval(0);
    }
    return ok;
}

Vector<double> TabularCoordinate::pixelValues() const
{
    Vector<double> pixels;
    if (channel_corrector_p) {
	pixels = channel_corrector_p->getX();
    }
    return pixels;
}

Vector<double> TabularCoordinate::worldValues() const
{
    Vector<double> tmp = pixelValues();
    const uint32_t n = tmp.nelements();
    for (uint32_t i=0; i<n; i++) {
	AlwaysAssert(toWorld(tmp(i), tmp(i)), AipsError);
    }
    return tmp;
}



bool TabularCoordinate::near(const Coordinate& other,
                             double tol) const

{
   Vector <int32_t> excludeAxes;
   return near(other, excludeAxes, tol);
}


bool TabularCoordinate::near(const Coordinate& other,
                             const Vector<int32_t>& excludeAxes,
                             double tol) const

{
   if (other.type() != this->type()) {
      set_error(String("Comparison is not with another TabularCoordinate"));
      return false;
   }

// The TabularCoordinate has only one axis (pixel and world).
// Therefore, if excludeAxes contains "0", we are done.
// Add an assertion failure check should this ever change

   AlwaysAssert(nPixelAxes() == 1, AipsError);
   AlwaysAssert(nWorldAxes() == 1, AipsError);
   bool found;
   if (linearSearch(found, excludeAxes, 0, excludeAxes.nelements()) >= 0)
      return true;

   
// Check units and name

   if (unit_p != other.worldAxisUnits()(0)) {
      set_error("The TabularCoordinates have differing axis units");
      return false;
   }
   if (name_p != other.worldAxisNames()(0)) {
      set_error("The TabularCoordinates have differing world axis names");
      return false;
   }

// Private data crval_p, cdelt_p, crpix_p, matrix_p are formed
// from the Table values so in principle there is no need to
// check them.  However, if they differ, that might be faster
// than working through the table so check them anyway.

   const TabularCoordinate& tCoord = dynamic_cast<const TabularCoordinate&>(other);
   if (!casacore::near(crval_p,tCoord.crval_p,tol)) {
      set_error("The TabularCoordinates have differing average reference values");
      return false;
   }
   if (!casacore::near(crpix_p,tCoord.crpix_p,tol)) {
      set_error("The TabularCoordinates have differing average reference pixels");
      return false;
   }
   if (!casacore::near(cdelt_p,tCoord.cdelt_p,tol)) {
      set_error("The TabularCoordinates have differing average increments");
      return false;
   }
   if (!casacore::near(matrix_p,tCoord.matrix_p,tol)) {

// It's really just one component of the matrix

      set_error("The TabularCoordinates have differing linear transformation matrices");
      return false;
   }



// Check the table 

   Vector<double> data1 =   this->pixelValues();
   Vector<double> data2 = tCoord.pixelValues();
   if (data1.nelements() != data2.nelements()) {
      set_error("The TabularCoordinates have differing numbers of entries in the pixel value table");
      return false;
   }
   uint32_t i;
   for (i=0; i<data1.nelements(); i++) {
      if (!casacore::near(data1(i),data2(i),tol)) {
         set_error("The TabularCoordinates have differing pixel value tables");
         return false;
      }
   }

   data1 =   this->worldValues();
   data2 = tCoord.worldValues();
   if (data1.nelements() != data2.nelements()) {
      set_error("The TabularCoordinates have differing numbers of entries in the world value table");
      return false;
   }
   for (i=0; i<data1.nelements(); i++) {
      if (!casacore::near(data1(i),data2(i),tol)) {
         set_error("The TabularCoordinates have differing world value tables");
         return false;
      }
   }

   return true;
}



bool TabularCoordinate::save(RecordInterface &container,
			     const String &fieldName) const
{
    bool ok = (!container.isDefined(fieldName));
    if (ok) {
	Record subrec;
	subrec.define("crval", referenceValue());
	subrec.define("crpix", referencePixel());
	subrec.define("cdelt", increment());
	subrec.define("pc", linearTransform());
	subrec.define("axes", worldAxisNames());
	subrec.define("units", worldAxisUnits());
	if (channel_corrector_p) {
	    subrec.define("pixelvalues", pixelValues());
	    subrec.define("worldvalues", worldValues());
	} else {
	    Vector<double> tmp;
	    subrec.define("pixelvalues", tmp);
	    subrec.define("worldvalues", tmp);
	}

	container.defineRecord(fieldName, subrec);
    }
    return ok;
}

TabularCoordinate* TabularCoordinate::restore(const RecordInterface &container,
                                              const String &fieldName)
{
    if (! container.isDefined(fieldName)) {
	return 0;
    }

    Record subrec(container.asRecord(fieldName));
    
    if (!subrec.isDefined("crval")) {
	return 0;
    }
    Vector<double> crval(subrec.toArrayDouble("crval"));

    if (!subrec.isDefined("crpix")) {
	return 0;
    }
    Vector<double> crpix(subrec.toArrayDouble("crpix"));

    if (!subrec.isDefined("cdelt")) {
	return 0;
    }
    Vector<double> cdelt(subrec.toArrayDouble("cdelt"));

    if (!subrec.isDefined("pc")) {
	return 0;
    }
    Matrix<double> pc(subrec.toArrayDouble("pc"));

    
    if (!subrec.isDefined("axes")) {
	return 0;
    }
    Vector<String> axes;
    subrec.get("axes", axes);
    
    if (!subrec.isDefined("units")) {
	return 0;
    }
    Vector<String> units;
    subrec.get("units", units);

    if (!subrec.isDefined("pixelvalues") || !subrec.isDefined("worldvalues")) {
	return 0;
    }
    Vector<double> pixels(subrec.toArrayDouble("pixelvalues"));
    Vector<double> world (subrec.toArrayDouble("worldvalues"));

    TabularCoordinate *retval = 0;
    if (pixels.nelements() > 0) {
	retval = new TabularCoordinate(pixels, world, units(0), axes(0));
    } else {
	retval = new TabularCoordinate(crval(0), cdelt(0), crpix(0), units(0),
				       axes(0));
    }
//
    return retval;
}

Coordinate *TabularCoordinate::clone() const
{
    return new TabularCoordinate(*this);
}




Coordinate* TabularCoordinate::makeFourierCoordinate (const Vector<bool>& axes, 
                                                      const Vector<int32_t>& shape) const
//
// axes says which axes in the coordinate are to be transformed
// shape is the shape of the image for all axes in this coordinate
//
{   
   if (channel_corrector_p) {
      set_error("Cannot Fourier Transform a non-linear TabularCoordinate");
      return 0;
   }
//
   if (axes.nelements() != nPixelAxes()) {
      set_error ("Invalid number of specified axes");
      return 0;
   }
   uint32_t nT = 0;
   for (uint32_t i=0; i<nPixelAxes(); i++) if (axes(i)) nT++;
   if (nT==0) {
      set_error ("You have not specified any axes to transform");
      return 0;
   }
//
   if (shape.nelements() != nPixelAxes()) {
      set_error ("Invalid number of elements in shape");
      return 0;
   }
//
   const Vector<String>& units = worldAxisUnits();
   const Vector<String>& names = worldAxisNames();
//
   Vector<String> unitsCanon(worldAxisUnits().copy());
   Vector<String> unitsOut(worldAxisUnits().copy());
   Vector<String> namesOut(worldAxisNames().copy());
//
   for (uint32_t i=0; i<nPixelAxes(); i++) {
      if (axes(i)) {
         fourierUnits(namesOut(i), unitsOut(i), unitsCanon(i), Coordinate::TABULAR, i, 
                      units(i), names(i));
      }
   }

// Make a copy of ourselves so we can change the units (else we would
// need to make this a non-const function)

    TabularCoordinate tc = *this;
    if (!tc.setWorldAxisUnits(unitsCanon)) {
      set_error("Could not set world axis units");
      return 0;
    }

// Set the Fourier coordinate parameters.  

   Vector<double> crval(tc.referenceValue().copy());
   Vector<double> crpix(tc.referencePixel().copy());
   Vector<double> cdelt(tc.increment().copy());
   for (uint32_t i=0; i<nPixelAxes(); i++) {
      if (axes(i)) { 
         crval(i) = 0.0;
         cdelt(i) = 1.0 / (shape(i) * cdelt(i));
         crpix(i) = int32_t(shape(i)/2);
      }
   }

// Now create the new output LinearCoordinate

    Matrix<double> pc(1, 1);
    pc = 0.0; 
    pc.diagonal() = 1.0;
    return new LinearCoordinate(namesOut, unitsOut, crval, cdelt, pc, crpix);
}


void TabularCoordinate::makeNonLinearTabularCoordinate(const Vector<double> &pixelValues,
                                                       const Vector<double> &worldValues)
{
    const uint32_t n = pixelValues.nelements();

    if (n < 1 || n != worldValues.nelements()) {
	throw(AipsError("TabularCoordinate::TabularCoordinate - illegal table "
			"(length 0 or n(pixelvalues) != n(worldvalues)"));
    }

    if(n==1){ // trivial case

      // Work out "global" crval etc.
      crval_p = worldValues(0);
      crpix_p = pixelValues(0);
      cdelt_p = 0.;
      matrix_p = 1.0;

      Vector<double> averagePixel(1,pixelValues(0));

      ScalarSampledFunctional<double> in(pixelValues), avg(averagePixel);
      channel_corrector_p = 
	new Interpolate1D<double,double>(in, avg, true, true);
      channel_corrector_rev_p = 
	new Interpolate1D<double,double>(avg, in, true, true);
      AlwaysAssert(channel_corrector_p != 0 && channel_corrector_rev_p != 0,
		   AipsError);

      channel_corrector_p->setMethod(Interpolate1D<double,double>::nearestNeighbour);
      channel_corrector_rev_p->setMethod(Interpolate1D<double,double>::nearestNeighbour);

    }
    else{ // n>1

      if (pixelValues(n-1) - pixelValues(0) == 0) {
	throw(AipsError("TabularCoordinate::TabularCoordinate - illegal table "
			"first and last pixel values are the same"));
      }

      // Work out "global" crval etc.
      crval_p = worldValues(0);
      crpix_p = pixelValues(0);
      cdelt_p = (worldValues(n-1) - worldValues(0)) /
	(pixelValues(n-1) - pixelValues(0));
      matrix_p = 1.0;
      
      if (cdelt_p == 0.0) {
        throw(AipsError("TabularCoordinate - start and "
                        "end values in table must differ"));
      }
      
      double signworld = ((worldValues(n-1) - worldValues(0))  > 0 ? 1.0 : -1.0);
      double signpixel = ((pixelValues(n-1) - pixelValues(0))  > 0 ? 1.0 : -1.0);
      
      // Check that the pixel values and values monotonically increase or decrease
      // and if so, work out the difference between the actual supplied pixel and
      // the "average" pixel value.
      Vector<double> averagePixel(n);
      for (uint32_t i=0; i<n; i++) {
 	if (i>1) {
	  double diffworld = signworld*(worldValues(i) - worldValues(i-1));
	  double diffpixel = signpixel*(pixelValues(i) - pixelValues(i-1));
	  if (diffworld <= 0 || diffpixel <= 0) {
	    throw(AipsError("TabularCoordinate - pixel and world values "
			    "must increase or decrease monotonically"));
	  }
	}
	averagePixel(i) = (worldValues(i) - crval_p)/cdelt_p + crpix_p;
      }
      
      ScalarSampledFunctional<double> in(pixelValues), avg(averagePixel);
      channel_corrector_p = 
	new Interpolate1D<double,double>(in, avg, true, true);
      channel_corrector_rev_p = 
	new Interpolate1D<double,double>(avg, in, true, true);
      AlwaysAssert(channel_corrector_p != 0 && channel_corrector_rev_p != 0,
		   AipsError);
      
      channel_corrector_p->setMethod(Interpolate1D<double,double>::linear);
      channel_corrector_rev_p->setMethod(Interpolate1D<double,double>::linear);
    } // endif

}


} //# NAMESPACE CASACORE - END

