//# StokesCoordinate.cc: this defines StokesCoordinate which shoe-horns Stokes axes into a Coordinate
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


#include <casacore/coordinates/Coordinates/StokesCoordinate.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>

#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/LinearSearch.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/IO/ArrayIO.h>

#include <casacore/casa/sstream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


StokesCoordinate::StokesCoordinate(const Vector<int32_t> &whichStokes)
: Coordinate(),
  values_p(whichStokes.nelements()), 
  crval_p(0), 
  crpix_p(0), 
  matrix_p(1),
  cdelt_p(1), 
  name_p("Stokes"), 
  unit_p("")
{
    setStokes(whichStokes);
    nValues_p = values_p.nelements();
    setDefaultWorldMixRanges();
}


StokesCoordinate::StokesCoordinate(const StokesCoordinate &other)
: Coordinate(other),
  values_p(other.values_p),
  crval_p(other.crval_p),
  crpix_p(other.crpix_p),
  matrix_p(other.matrix_p),
  cdelt_p(other.cdelt_p),
  name_p(other.name_p),
  unit_p(other.unit_p),
  nValues_p(other.nValues_p)
{  
    setDefaultWorldMixRanges();
}

StokesCoordinate &StokesCoordinate::operator=(const StokesCoordinate &other)
{
    if (this != &other) {
        Coordinate::operator=(other);
	values_p = other.values_p;
	crval_p = other.crval_p;
	crpix_p = other.crpix_p;
	matrix_p = other.matrix_p;
	cdelt_p = other.cdelt_p;
	name_p = other.name_p;
	unit_p = other.unit_p;
        nValues_p = other.nValues_p;
    }

    return *this;
}

StokesCoordinate::~StokesCoordinate()
{}

Coordinate::Type StokesCoordinate::type() const
{
    return Coordinate::STOKES;
}

String StokesCoordinate::showType() const
{
    return String("Stokes");
}

uint32_t StokesCoordinate::nPixelAxes() const
{
    return 1;
}

uint32_t StokesCoordinate::nWorldAxes() const
{
    return 1;
}

bool StokesCoordinate::toWorld(Stokes::StokesTypes &stokes, int32_t pixel) const
{
    double world;
    if (toWorld (world, static_cast<double>(pixel))) {
       stokes = Stokes::type(values_p[pixel]);
       return true;
    }
    return false;
}


bool StokesCoordinate::toPixel(int32_t& pixel, Stokes::StokesTypes stokes) const
{
    double tmp;
    if (toPixel(tmp, static_cast<double>(stokes))) {
       pixel = int32_t(tmp + 0.5);    
       return true;
    }
    return false;
}


bool StokesCoordinate::toWorld(Vector<double>& world, 
			       const Vector<double>& pixel, bool) const
{
    DebugAssert(pixel.nelements()==1, AipsError);
    world.resize(1);
//
    double tmp;
    if (toWorld(tmp, pixel(0))) {
       world(0) = tmp;
       return true;
    }
    return false;
}


bool StokesCoordinate::toPixel(Vector<double> &pixel, 
    	                       const Vector<double> &world) const
{
    DebugAssert(world.nelements()==1, AipsError);
    pixel.resize(1);
//
    double tmp;
    if (toPixel(tmp, world(0))) {
       pixel(0) = tmp;
       return true;
    }
    return false;
}

double StokesCoordinate::toWorld (Stokes::StokesTypes stokes) 
{
    return static_cast<double>(stokes);
}


Stokes::StokesTypes StokesCoordinate::toWorld (double world) 
{
    int32_t i = int32_t(world + 0.5);
    if (i < 0 ||  i>=Stokes::NumberOfTypes) {
       return Stokes::Undefined;
    }
//
    return static_cast<Stokes::StokesTypes>(i);
}


Vector<int32_t> StokesCoordinate::stokes() const
{
    return Vector<int32_t>(values_p.begin(), values_p.end());
}

Vector<String> StokesCoordinate::stokesStrings() const {
    uint32_t n = values_p.size();
    Vector<String> ret(n);
    for (uint32_t i=0; i<n; i++) {
        ret[i] = Stokes::name(Stokes::type(values_p[i]));
    }
    return ret;
}

void StokesCoordinate::setStokes (const Vector<int32_t> &whichStokes)
{
    AlwaysAssert(whichStokes.nelements()>0, AipsError);

// Make sure the stokes occur at most once

    Block<bool> alreadyUsed(Stokes::NumberOfTypes);
    alreadyUsed = false;
    for (uint32_t i=0; i<whichStokes.nelements(); i++) {
/*
	if (Stokes::type(whichStokes(i))== Stokes::Undefined) {
           throw(AipsError("You have specified an Undefined Stokes type"));
        }
*/
	if (alreadyUsed[whichStokes(i)]) {
           throw(AipsError("You have specified the same Stokes more than once"));
        }
	alreadyUsed[whichStokes(i)] = true;
    }
//
    values_p.resize(whichStokes.nelements());
    values_p = makeBlock(whichStokes);
    nValues_p = values_p.nelements();
//
    crval_p = whichStokes(0);
    crpix_p = 0;
    matrix_p = 1.0;
    cdelt_p = 1.0;
}



Vector<String> StokesCoordinate::worldAxisNames() const
{
    Vector<String> names(1);
    names = name_p;
    return names;
}

Vector<String> StokesCoordinate::worldAxisUnits() const
{
    Vector<String> units(1);
    units = unit_p;
    return units;
}

Vector<double> StokesCoordinate::referencePixel() const
{
    Vector<double> crpix(1);
    crpix = crpix_p;
    return crpix;
}

Matrix<double> StokesCoordinate::linearTransform() const
{
    Matrix<double> matrix(1,1);
    matrix(0,0) = matrix_p;
    return matrix;
}

Vector<double> StokesCoordinate::increment() const
{
    Vector<double> cdelt(1);
    cdelt = cdelt_p;
    return cdelt;
}

Vector<double> StokesCoordinate::referenceValue() const
{
    Vector<double> crval(1);
    crval = crval_p;
    return crval;
}

bool StokesCoordinate::setWorldAxisNames(const Vector<String> &names)
{
    bool ok = names.nelements()==1;
    if (!ok) {
       set_error ("names vector must be of length 1");
    } else {
       name_p = names(0);
    }
    return ok;
}

bool StokesCoordinate::setWorldAxisUnits(const Vector<String> &)
{
    return true;
}

bool StokesCoordinate::setReferencePixel(const Vector<double> &)
{
   return true;
}


bool StokesCoordinate::setLinearTransform(const Matrix<double> &)
{
   return true;
}

bool StokesCoordinate::setIncrement(const Vector<double> &) 
{
   return true;
}

bool StokesCoordinate::setReferenceValue(const Vector<double> &)
{
   return true;
}


bool StokesCoordinate::near(const Coordinate& other,
                            double tol) const
{
   Vector<int32_t> excludeAxes;
   return near(other, excludeAxes, tol);
}

bool StokesCoordinate::near(const Coordinate& other,
                            const Vector<int32_t>& excludeAxes,
                            double) const
{
   if (other.type() != this->type()) {
      set_error("Comparison is not with another StokesCoordinate");
      return false;
   }

// Check name

   const StokesCoordinate& sCoord = dynamic_cast<const StokesCoordinate&>(other);
   if (name_p != sCoord.name_p) {
      set_error("The StokesCoordinates have differing world axis names");
      return false;
   }

// Number of pixel and world axes is the same for a StokesCoordinate
// and it always 1.   SO if excludeAxes contains "0" we are done.
// Add an assertion check should this change 
 
   bool found;
   if (linearSearch(found, excludeAxes, 0, excludeAxes.nelements()) >= 0)  return true;


// The only other thing that really matters in the STokesCoordinate
// is the values along the axis.    Nothing else (e.g. crval_p etc)
// is ever actually used.

   if (nValues_p != sCoord.nValues_p) {
      set_error("The StokesCoordinates have different numbers of Stokes values");
      return false;
   }

// Conformance testing usually verifies aspects of the Coordinate
// such as refval, refpix etc.  Because the StokesCoordinate
// violates that model, and all the matters are the actual Stokes
// values, it is a bit hard to know what to test.  E.g. I make
// a LatticeExprNode from complex(q,u).  The conformance check
// comes in here and will fail if I test actual values on the
// Stokes axis.  Until I know what to do better, comment it out.

/*
   for (int32_t i=0; i<nValues_p; i++) {
      if (values_p[i] != sCoord.values_p[i]) {
         set_error("The StokesCoordinates have different Stokes values");
         return false;
      }
   }
*/
// 
   return true;
}

bool StokesCoordinate::doNearPixel (const Coordinate& other,
                                    const Vector<bool>&,
                                    const Vector<bool>&,
                                    double) const
{
   if (other.type() != Coordinate::STOKES) {
      set_error("Other Coordinate type is not Stokes");
      return false;
   }

//
// The only other thing that really matters in the STokesCoordinate
// is the values along the axis.    Nothing else (e.g. crval_p etc)
// is ever actually used.  However, if we check these, we getinto
// trouble for things like ImageExpr making P from Q and U say
// (i.e. the two coordinates have differnet values).  So we
// simply test that the number of stokes is the same and that
// is that
//
   const StokesCoordinate& sCoord = dynamic_cast<const StokesCoordinate&>(other);
   if (nValues_p != sCoord.nValues_p) {
      set_error("The StokesCoordinates have different numbers of Stokes values");
      return false;
   }
//
   return true;
}



bool StokesCoordinate::save(RecordInterface &container,
			    const String &fieldName) const

{
    bool ok = !container.isDefined(fieldName);
    if (ok) {
	Record subrec;
	subrec.define("axes", worldAxisNames());
//
	Vector<String> stokes(nValues_p);
	for (int32_t i=0; i<nValues_p; i++) {
	    stokes(i) = Stokes::name(Stokes::type(values_p[i]));
	}
	subrec.define("stokes", stokes);
	container.defineRecord(fieldName, subrec);
//
// Increment, refpix, refval, pc are meaningless to StokesCoordinate. But 
// we save them anyway as maybe one day I will make use of them again, 
// so keep them for  compatibility.
//
        subrec.define("crval", referenceValue());
        subrec.define("crpix", referencePixel());
        subrec.define("cdelt", increment());
        subrec.define("pc", linearTransform());

// it never makes sense to set the units
// subrec.define("units", worldAxisUnits()); 

        container.defineRecord(fieldName, subrec); 
    }
    return ok;
}


StokesCoordinate *StokesCoordinate::restore(const RecordInterface &container,
                                            const String &fieldName)
{
    if (! container.isDefined(fieldName)) {
	return 0;
    }

    Record subrec(container.asRecord(fieldName));

    if (!subrec.isDefined("axes")) {
	return 0;
    }
    Vector<String> axes;
    subrec.get("axes", axes);
//
    if (!subrec.isDefined("stokes")) {
	return 0;
    }
    Vector<String> stokes;
    subrec.get("stokes", stokes);
    Vector<int32_t> istokes(stokes.nelements());
    for (uint32_t i=0; i<istokes.nelements(); i++) {
	istokes(i) = Stokes::type(stokes(i));
    }

// refpix,refval, increment, pc are all meaningless to StokesCoordinate.  So 
// we don't bother reading them now.  Can be read if need be in the future if
// they become useful again

/*
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
*/

    StokesCoordinate* retval = new StokesCoordinate(istokes);
    AlwaysAssert(retval, AipsError);
    retval->setWorldAxisNames(axes);
    AlwaysAssert(retval, AipsError);
//
// retval->setWorldAxisUnits(units); // It never makes sense to set the units
//
/*
    retval-> setIncrement(cdelt);  
    retval->setReferenceValue(crval);
    retval->setReferencePixel(crpix);
//
*/
//
    return retval;
}

Coordinate *StokesCoordinate::clone() const
{
    return new StokesCoordinate(*this);
}

String StokesCoordinate::format(String& units,
                                Coordinate::formatType,
                                double worldValue,
                                uint32_t worldAxis,
                                bool, bool, int32_t, bool) const
//
// world  abs=rel for Stokes
//
{
   units = worldAxisUnits()(worldAxis);
   return Stokes::name(StokesCoordinate::toWorld (worldValue));
}



void StokesCoordinate::makePixelRelative (Vector<double>& pixel) const
//       
// rel = abs - ref
//
{ 
   DebugAssert(pixel.nelements()==1, AipsError);
//
   int32_t index = int32_t(pixel(0) + 0.5);
   if (index >= 0 && index < nValues_p) {
      pixel -= referencePixel();
   } else {
      ostringstream os;
      os << "Absolute pixel " << index << " is out of range [0.." << nValues_p-1 << "]";
      String s(os);
      throw(AipsError(s));
   }
}
   
   
 
void StokesCoordinate::makePixelAbsolute (Vector<double>& pixel) const
//
// abs = rel + ref
//
{ 
   DebugAssert(pixel.nelements()==1, AipsError);
   pixel += referencePixel();
//
   int32_t index = int32_t(pixel(0) + 0.5);
   if (index < 0 ||  index >= nValues_p) {
      ostringstream os;
      os << "Absolute pixel " << index << " is out of range [0.." << nValues_p-1 << "]";
      String s(os);
      throw(AipsError(s));
   }
} 
   

void StokesCoordinate::makeWorldRelative (Vector<double>&) const
//
// By definition, for StokesCoordinate, world abs = rel
//
{
}


void StokesCoordinate::makeWorldAbsolute (Vector<double>&) const
//
// By definition, for StokesCoordinate, world abs = rel
//
{}



// Private functions


bool StokesCoordinate::toWorld(double& world, const double pixel) const
{
    int32_t index = int32_t(pixel + 0.5);
    if (index >= 0 && index < nValues_p) {
	world = values_p[index];
	return true;
    } else {
	ostringstream os;
	os << "Pixel " << index << " is out of range [0.." << nValues_p-1 << "]";
	set_error(os);
	return false;
    }
}


bool StokesCoordinate::toPixel(double& pixel,  const double world) const
{
    bool found = false;
    int32_t index;
    for (index=0; index<nValues_p; index++) {
	found = casacore::near(world, double(values_p[index]));
	if (found) break;
    }
    if (!found) {
	ostringstream os;
        Stokes::StokesTypes t0 = toWorld(world);
        String t = Stokes::name(t0);
	os << "Stokes value " << t << " is not contained in this StokesCoordinate";
	set_error(os);
	return false;
    }
//
    pixel = double(index);
    return true;
}

bool StokesCoordinate::setWorldMixRanges (const IPosition&)
{
   setDefaultWorldMixRanges();
   return true;
}


void StokesCoordinate::setDefaultWorldMixRanges ()
{
   Vector<double> pixel(nPixelAxes());
   pixel(0) = 0;
   toWorld(worldMin_p, pixel);
   pixel(0) = nValues_p - 1;
   toWorld(worldMax_p, pixel);
}


} //# NAMESPACE CASACORE - END

