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
//#
//#
//# $Id$


#include <casacore/coordinates/Coordinates/StokesCoordinate.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>

#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/LinearSearch.h>
#include <casacore/casa/BasicMath/Math.h>

#include <casacore/casa/sstream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


StokesCoordinate::StokesCoordinate(const Vector<Int> &whichStokes)
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

uInt StokesCoordinate::nPixelAxes() const
{
    return 1;
}

uInt StokesCoordinate::nWorldAxes() const
{
    return 1;
}

Bool StokesCoordinate::toWorld(Stokes::StokesTypes &stokes, Int pixel) const
{
    Double world;
    if (toWorld (world, static_cast<Double>(pixel))) {
       stokes = Stokes::type(values_p[pixel]);
       return True;
    }
    return False;
}


Bool StokesCoordinate::toPixel(Int& pixel, Stokes::StokesTypes stokes) const
{
    Double tmp;
    if (toPixel(tmp, static_cast<Double>(stokes))) {
       pixel = Int(tmp + 0.5);    
       return True;
    }
    return False;
}


Bool StokesCoordinate::toWorld(Vector<Double>& world, 
			       const Vector<Double>& pixel) const
{
    DebugAssert(pixel.nelements()==1, AipsError);
    world.resize(1);
//
    Double tmp;
    if (toWorld(tmp, pixel(0))) {
       world(0) = tmp;
       return True;
    }
    return False;
}


Bool StokesCoordinate::toPixel(Vector<Double> &pixel, 
    	                       const Vector<Double> &world) const
{
    DebugAssert(world.nelements()==1, AipsError);
    pixel.resize(1);
//
    Double tmp;
    if (toPixel(tmp, world(0))) {
       pixel(0) = tmp;
       return True;
    }
    return False;
}

Double StokesCoordinate::toWorld (Stokes::StokesTypes stokes) 
{
    return static_cast<Double>(stokes);
}


Stokes::StokesTypes StokesCoordinate::toWorld (Double world) 
{
    Int i = Int(world + 0.5);
    if (i < 0 ||  i>=Stokes::NumberOfTypes) {
       return Stokes::Undefined;
    }
//
    return static_cast<Stokes::StokesTypes>(i);
}


Vector<Int> StokesCoordinate::stokes() const
{
    return Vector<Int>(values_p);
}

Vector<String> StokesCoordinate::stokesStrings() const {
    uInt n = values_p.size();
    Vector<String> ret(n);
    for (uInt i=0; i<n; i++) {
        ret[i] = Stokes::name(Stokes::type(values_p[i]));
    }
    return ret;
}

void StokesCoordinate::setStokes (const Vector<Int> &whichStokes)
{
    AlwaysAssert(whichStokes.nelements()>0, AipsError);

// Make sure the stokes occur at most once

    Block<Bool> alreadyUsed(Stokes::NumberOfTypes);
    alreadyUsed = False;
    for (uInt i=0; i<whichStokes.nelements(); i++) {
/*
	if (Stokes::type(whichStokes(i))== Stokes::Undefined) {
           throw(AipsError("You have specified an Undefined Stokes type"));
        }
*/
	if (alreadyUsed[whichStokes(i)]) {
           throw(AipsError("You have specified the same Stokes more than once"));
        }
	alreadyUsed[whichStokes(i)] = True;
    }
//
    values_p.resize(whichStokes.nelements());
    whichStokes.toBlock(values_p);
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

Vector<Double> StokesCoordinate::referencePixel() const
{
    Vector<Double> crpix(1);
    crpix = crpix_p;
    return crpix;
}

Matrix<Double> StokesCoordinate::linearTransform() const
{
    Matrix<Double> matrix(1,1);
    matrix(0,0) = matrix_p;
    return matrix;
}

Vector<Double> StokesCoordinate::increment() const
{
    Vector<Double> cdelt(1);
    cdelt = cdelt_p;
    return cdelt;
}

Vector<Double> StokesCoordinate::referenceValue() const
{
    Vector<Double> crval(1);
    crval = crval_p;
    return crval;
}

Bool StokesCoordinate::setWorldAxisNames(const Vector<String> &names)
{
    Bool ok = names.nelements()==1;
    if (!ok) {
       set_error ("names vector must be of length 1");
    } else {
       name_p = names(0);
    }
    return ok;
}

Bool StokesCoordinate::setWorldAxisUnits(const Vector<String> &)
{
    return True;
}

Bool StokesCoordinate::setReferencePixel(const Vector<Double> &)
{
   return True;
}


Bool StokesCoordinate::setLinearTransform(const Matrix<Double> &)
{
   return True;
}

Bool StokesCoordinate::setIncrement(const Vector<Double> &) 
{
   return True;
}

Bool StokesCoordinate::setReferenceValue(const Vector<Double> &)
{
   return True;
}


Bool StokesCoordinate::near(const Coordinate& other,
                            Double tol) const
{
   Vector<Int> excludeAxes;
   return near(other, excludeAxes, tol);
}

Bool StokesCoordinate::near(const Coordinate& other,
                            const Vector<Int>& excludeAxes,
                            Double) const
{
   if (other.type() != this->type()) {
      set_error("Comparison is not with another StokesCoordinate");
      return False;
   }

// Check name

   const StokesCoordinate& sCoord = dynamic_cast<const StokesCoordinate&>(other);
   if (name_p != sCoord.name_p) {
      set_error("The StokesCoordinates have differing world axis names");
      return False;
   }

// Number of pixel and world axes is the same for a StokesCoordinate
// and it always 1.   SO if excludeAxes contains "0" we are done.
// Add an assertion check should this change 
 
   Bool found;
   if (linearSearch(found, excludeAxes, 0, excludeAxes.nelements()) >= 0)  return True;


// The only other thing that really matters in the STokesCoordinate
// is the values along the axis.    Nothing else (e.g. crval_p etc)
// is ever actually used.

   if (nValues_p != sCoord.nValues_p) {
      set_error("The StokesCoordinates have different numbers of Stokes values");
      return False;
   }

// Conformance testing usually verifies aspects of the Coordinate
// such as refval, refpix etc.  Because the StokesCoordinate
// violates that model, and all the matters are the actual Stokes
// values, it is a bit hard to know what to test.  E.g. I make
// a LatticeExprNode from complex(q,u).  The conformance check
// comes in here and will fail if I test actual values on the
// Stokes axis.  Until I know what to do better, comment it out.

/*
   for (Int i=0; i<nValues_p; i++) {
      if (values_p[i] != sCoord.values_p[i]) {
         set_error("The StokesCoordinates have different Stokes values");
         return False;
      }
   }
*/
// 
   return True;
}

Bool StokesCoordinate::doNearPixel (const Coordinate& other,
                                    const Vector<Bool>&,
                                    const Vector<Bool>&,
                                    Double) const
{
   if (other.type() != Coordinate::STOKES) {
      set_error("Other Coordinate type is not Stokes");
      return False;
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
      return False;
   }
//
   return True;
}



Bool StokesCoordinate::save(RecordInterface &container,
			    const String &fieldName) const

{
    Bool ok = !container.isDefined(fieldName);
    if (ok) {
	Record subrec;
	subrec.define("axes", worldAxisNames());
//
	Vector<String> stokes(nValues_p);
	for (Int i=0; i<nValues_p; i++) {
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
    Vector<Int> istokes(stokes.nelements());
    for (uInt i=0; i<istokes.nelements(); i++) {
	istokes(i) = Stokes::type(stokes(i));
    }

// refpix,refval, increment, pc are all meaningless to StokesCoordinate.  So 
// we don't bother reading them now.  Can be read if need be in the future if
// they become useful again

/*
    Vector<Double> crval(subrec.toArrayDouble("crval"));

    if (!subrec.isDefined("crpix")) {
        return 0;
    }
    Vector<Double> crpix(subrec.toArrayDouble("crpix"));
    
    if (!subrec.isDefined("cdelt")) {
        return 0;
    }
    Vector<Double> cdelt(subrec.toArrayDouble("cdelt"));

    if (!subrec.isDefined("pc")) {
        return 0;
    }
    Matrix<Double> pc(subrec.toArrayDouble("pc"));
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
                                Double worldValue,
                                uInt worldAxis,
                                Bool, Bool, Int, Bool) const
//
// world  abs=rel for Stokes
//
{
   units = worldAxisUnits()(worldAxis);
   return Stokes::name(StokesCoordinate::toWorld (worldValue));
}



void StokesCoordinate::makePixelRelative (Vector<Double>& pixel) const
//       
// rel = abs - ref
//
{ 
   DebugAssert(pixel.nelements()==1, AipsError);
//
   Int index = Int(pixel(0) + 0.5);
   if (index >= 0 && index < nValues_p) {
      pixel -= referencePixel();
   } else {
      ostringstream os;
      os << "Absolute pixel " << index << " is out of range [0.." << nValues_p-1 << "]";
      String s(os);
      throw(AipsError(s));
   }
}
   
   
 
void StokesCoordinate::makePixelAbsolute (Vector<Double>& pixel) const
//
// abs = rel + ref
//
{ 
   DebugAssert(pixel.nelements()==1, AipsError);
   pixel += referencePixel();
//
   Int index = Int(pixel(0) + 0.5);
   if (index < 0 ||  index >= nValues_p) {
      ostringstream os;
      os << "Absolute pixel " << index << " is out of range [0.." << nValues_p-1 << "]";
      String s(os);
      throw(AipsError(s));
   }
} 
   

void StokesCoordinate::makeWorldRelative (Vector<Double>&) const
//
// By definition, for StokesCoordinate, world abs = rel
//
{
}


void StokesCoordinate::makeWorldAbsolute (Vector<Double>&) const
//
// By definition, for StokesCoordinate, world abs = rel
//
{}



// Private functions


Bool StokesCoordinate::toWorld(Double& world, const Double pixel) const
{
    Int index = Int(pixel + 0.5);
    if (index >= 0 && index < nValues_p) {
	world = values_p[index];
	return True;
    } else {
	ostringstream os;
	os << "Pixel " << index << " is out of range [0.." << nValues_p-1 << "]";
	set_error(os);
	return False;
    }
}


Bool StokesCoordinate::toPixel(Double& pixel,  const Double world) const
{
    Bool found = False;
    Int index;
    for (index=0; index<nValues_p; index++) {
	found = casacore::near(world, Double(values_p[index]));
	if (found) break;
    }
    if (!found) {
	ostringstream os;
        Stokes::StokesTypes t0 = toWorld(world);
        String t = Stokes::name(t0);
	os << "Stokes value " << t << " is not contained in this StokesCoordinate";
	set_error(os);
	return False;
    }
//
    pixel = Double(index);
    return True;
}

Bool StokesCoordinate::setWorldMixRanges (const IPosition&)
{
   setDefaultWorldMixRanges();
   return True;
}


void StokesCoordinate::setDefaultWorldMixRanges ()
{
   Vector<Double> pixel(nPixelAxes());
   pixel(0) = 0;
   toWorld(worldMin_p, pixel);
   pixel(0) = nValues_p - 1;
   toWorld(worldMax_p, pixel);
}


} //# NAMESPACE CASACORE - END

