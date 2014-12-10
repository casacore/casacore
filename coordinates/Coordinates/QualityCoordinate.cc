//# QualityCoordinate.cc: this defines QualityCoordinate which shoe-horns Quality axes into a Coordinate
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


#include <casacore/coordinates/Coordinates/QualityCoordinate.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>

#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/LinearSearch.h>
#include <casacore/casa/BasicMath/Math.h>

#include <casacore/casa/sstream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


QualityCoordinate::QualityCoordinate(const Vector<Int> &whichQuality)
: Coordinate(),
  values_p(whichQuality.nelements()),
  crval_p(0), 
  crpix_p(0), 
  matrix_p(1),
  cdelt_p(1), 
  name_p("Quality"),
  unit_p("")
{
	// tested: tQualityCoordinate: 122
    setQuality(whichQuality);
    nValues_p = values_p.nelements();
    setDefaultWorldMixRanges();
}


QualityCoordinate::QualityCoordinate(const QualityCoordinate &other)
: Coordinate(other),
  values_p (other.values_p),
  crval_p  (other.crval_p),
  crpix_p  (other.crpix_p),
  matrix_p (other.matrix_p),
  cdelt_p  (other.cdelt_p),
  name_p   (other.name_p),
  unit_p   (other.unit_p),
  nValues_p(other.nValues_p)
{  
	// tested: tQualityCoordinate: 146
    setDefaultWorldMixRanges();
}

QualityCoordinate &QualityCoordinate::operator=(const QualityCoordinate &other)
{
	// tested: tQualityCoordinate: 175
	if (this != &other) {
		Coordinate::operator=(other);
		values_p  = other.values_p;
		crval_p   = other.crval_p;
		crpix_p   = other.crpix_p;
		matrix_p  = other.matrix_p;
		cdelt_p   = other.cdelt_p;
		name_p    = other.name_p;
		unit_p    = other.unit_p;
        nValues_p = other.nValues_p;
    }
    return *this;
}

QualityCoordinate::~QualityCoordinate()
{}

Coordinate::Type QualityCoordinate::type() const
{
	// tested: tQualityCoordinate: 168
    return Coordinate::QUALITY;
}

String QualityCoordinate::showType() const
{
	// tested: tQualityCoordinate: 174
    return String("Quality");
}

uInt QualityCoordinate::nPixelAxes() const
{
	// tested: tQualityCoordinate: 181
    return 1;
}

uInt QualityCoordinate::nWorldAxes() const
{
	// tested: tQualityCoordinate: 188
	return 1;
}

Bool QualityCoordinate::toWorld(Quality::QualityTypes &quality, Int pixel) const
{
	// tested: tQualityCoordinate: 443
    Double world;
    if (toWorld (world, static_cast<Double>(pixel))) {
       quality = Quality::type(values_p[pixel]);
       return True;
    }
    return False;
}


Bool QualityCoordinate::toPixel(Int& pixel, Quality::QualityTypes quality) const
{
	// tested: tQualityCoordinate: 437
    Double tmp;
    if (toPixel(tmp, static_cast<Double>(quality))) {
       pixel = Int(tmp + 0.5);    
       return True;
    }
    return False;
}


Bool QualityCoordinate::toWorld(Vector<Double>& world,
			       const Vector<Double>& pixel) const
{
	// tested: tQualityCoordinate: 403
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


Bool QualityCoordinate::toPixel(Vector<Double> &pixel,
    	                       const Vector<Double> &world) const
{
	// tested: tQualityCoordinate: 411
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

Double QualityCoordinate::toWorld (Quality::QualityTypes quality)
{
	// tested: tQualityCoordinate: 456
    return static_cast<Double>(quality);
}


Quality::QualityTypes QualityCoordinate::toWorld (Double world)
{
	// tested: tQualityCoordinate: 456
    Int i = Int(world + 0.5);
    if (i < 0 ||  i>=Quality::NumberOfTypes) {
       return Quality::Undefined;
    }
//
    return static_cast<Quality::QualityTypes>(i);
}


Vector<Int> QualityCoordinate::quality() const
{
	// tested: tQualityCoordinate: 257, 435
    return Vector<Int>(values_p);
}


void QualityCoordinate::setQuality (const Vector<Int> &whichQuality)
{
	// implicitly tested via the constructor
    AlwaysAssert(whichQuality.nelements()>0, AipsError);

    // Make sure the quality occur at most once
    Block<Bool> alreadyUsed(Quality::NumberOfTypes);
    alreadyUsed = False;
    for (uInt i=0; i<whichQuality.nelements(); i++) {
    	if (alreadyUsed[whichQuality(i)]) {
    		throw(AipsError("You have specified the same Quality more than once"));
    	}
    	alreadyUsed[whichQuality(i)] = True;
    }

    //
    values_p.resize(whichQuality.nelements());
    whichQuality.toBlock(values_p);
    nValues_p = values_p.nelements();

    //
    crval_p  = whichQuality(0);
    crpix_p  = 0;
    matrix_p = 1.0;
    cdelt_p  = 1.0;
}



Vector<String> QualityCoordinate::worldAxisNames() const
{
	// tested: tQualityCoordinate: 195
    Vector<String> names(1);
    names = name_p;
    return names;
}

Vector<String> QualityCoordinate::worldAxisUnits() const
{
	// tested: tQualityCoordinate: 221
   Vector<String> units(1);
    units = unit_p;
    return units;
}

Vector<Double> QualityCoordinate::referencePixel() const
{
	// tested: tQualityCoordinate: 315
    Vector<Double> crpix(1);
    crpix = crpix_p;
    return crpix;
}

Matrix<Double> QualityCoordinate::linearTransform() const
{
	// tested: tQualityCoordinate: 307
    Matrix<Double> matrix(1,1);
    matrix(0,0) = matrix_p;
    return matrix;
}

Vector<Double> QualityCoordinate::increment() const
{
	// tested: tQualityCoordinate: 307
    Vector<Double> cdelt(1);
    cdelt = cdelt_p;
    return cdelt;
}

Vector<Double> QualityCoordinate::referenceValue() const
{
	// tested: tQualityCoordinate: 299
    Vector<Double> crval(1);
    crval = crval_p;
    return crval;
}

Bool QualityCoordinate::setWorldAxisNames(const Vector<String> &names)
{
	// tested: tQualityCoordinate: 204
    Bool ok = names.nelements()==1;
    if (!ok) {
       set_error ("names vector must be of length 1");
    } else {
       name_p = names(0);
    }
    return ok;
}

Bool QualityCoordinate::setWorldAxisUnits(const Vector<String> &)
{
	// tested: tQualityCoordinate: 227, 242
    return True;
}

Bool QualityCoordinate::setReferencePixel(const Vector<Double> &)
{
	// tested: tQualityCoordinate: 365
	return True;
}


Bool QualityCoordinate::setLinearTransform(const Matrix<Double> &)
{
	// tested: tQualityCoordinate: 380
	return True;
}

Bool QualityCoordinate::setIncrement(const Vector<Double> &)
{
	// tested: tQualityCoordinate: 350
   return True;
}

Bool QualityCoordinate::setReferenceValue(const Vector<Double> &)
{
	// tested: tQualityCoordinate: 336
	return True;
}


Bool QualityCoordinate::near(const Coordinate& other,
                            Double tol) const
{
	// tested: basic test criteria in many
	// tests in tQualityCoordinate
	Vector<Int> excludeAxes;
   return near(other, excludeAxes, tol);
}

Bool QualityCoordinate::near(const Coordinate& other,
                            const Vector<Int>& excludeAxes,
                            Double) const
{
	// tested: basic test criteria in many
	// tests in tQualityCoordinate
	if (other.type() != this->type()) {
		set_error("Comparison is not with another QualityCoordinate");
		return False;
	}

	// Check name
	const QualityCoordinate& sCoord = dynamic_cast<const QualityCoordinate&>(other);
	if (name_p != sCoord.name_p) {
		set_error("The QualityCoordinates have differing world axis names");
		return False;
	}

	// Number of pixel and world axes is the same for a QualityCoordinate
	// and it always 1. So if excludeAxes contains "0" we are done.
	// Add an assertion check should this change
	Bool found;
	if (linearSearch(found, excludeAxes, 0, excludeAxes.nelements()) >= 0)
		return True;


	// The only other thing that really matters in the QualityCoordinate
	// is the values along the axis.    Nothing else (e.g. crval_p etc)
	// is ever actually used.
	if (nValues_p != sCoord.nValues_p) {
		set_error("The QualityCoordinates have different numbers of Quality values");
		return False;
	}

	return True;
}

Bool QualityCoordinate::doNearPixel (const Coordinate& other,
                                    const Vector<Bool>&,
                                    const Vector<Bool>&,
                                    Double) const
{
	// tested: tQualityCoordinate: 568
	if (other.type() != Coordinate::QUALITY) {
		set_error("Other Coordinate type is not Quality");
		return False;
	}

	//
	// The only other thing that really matters in the QualityCoordinate
	// is the values along the axis.    Nothing else (e.g. crval_p etc)
	// is ever actually used.
	// Here we simply test that the number of quality is the same and that
	// is that
	//
	const QualityCoordinate& sCoord = dynamic_cast<const QualityCoordinate&>(other);
	if (nValues_p != sCoord.nValues_p) {
		set_error("The QualityCoordinates have different numbers of Quality values");
		return False;
	}

	//
	return True;
}



Bool QualityCoordinate::save(RecordInterface &container,
			    const String &fieldName) const

{
	// tested: tQualityCoordinate: 267
    Bool ok = !container.isDefined(fieldName);
    if (ok) {
		Record subrec;
		subrec.define("axes", worldAxisNames());
		//
		Vector<String> quality(nValues_p);
		for (Int i=0; i<nValues_p; i++) {
			quality(i) = Quality::name(Quality::type(values_p[i]));
		}
		subrec.define("quality", quality);
		container.defineRecord(fieldName, subrec);

		//
		// Increment, refpix, refval, pc are meaningless to
		// QualityCoordinate. But we save them anyway as maybe one
		// day I will make use of them again, so keep them for
		// compatibility.
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


QualityCoordinate *QualityCoordinate::restore(const RecordInterface &container,
                                            const String &fieldName)
{
	// tested: tQualityCoordinate: 275
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
    if (!subrec.isDefined("quality")) {
	return 0;
    }
    Vector<String> quality;
    subrec.get("quality", quality);
    Vector<Int> iquality(quality.nelements());
    for (uInt i=0; i<iquality.nelements(); i++) {
	iquality(i) = Quality::type(quality(i));
    }

// refpix,refval, increment, pc are all meaningless to QualityCoordinate.  So
// we don't bother reading them now.  Can be read if need be in the future if
// they become useful again

    QualityCoordinate* retval = new QualityCoordinate(iquality);
    AlwaysAssert(retval, AipsError);
    retval->setWorldAxisNames(axes);
    AlwaysAssert(retval, AipsError);

    return retval;
}

Coordinate *QualityCoordinate::clone() const
{
	// tested: tQualityCoordinate: 285
    return new QualityCoordinate(*this);
}

String QualityCoordinate::format(String& units,
                                Coordinate::formatType,
                                Double worldValue,
                                uInt worldAxis,
                                Bool, Bool, Int, Bool) const
//
// world  abs=rel for Quality
//
{
	// tested: tQualityCoordinate: 478
	units = worldAxisUnits()(worldAxis);
	return Quality::name(QualityCoordinate::toWorld (worldValue));
}



void QualityCoordinate::makePixelRelative (Vector<Double>& pixel) const
//       
// rel = abs - ref
//
{ 
	// tested: tQualityCoordinate: 547
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
   
 
void QualityCoordinate::makePixelAbsolute (Vector<Double>& pixel) const
//
// abs = rel + ref
//
{ 
	// tested: tQualityCoordinate: 558
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
   

void QualityCoordinate::makeWorldRelative (Vector<Double>&) const
//
// By definition, for QualityCoordinate, world abs = rel
//
// there is nothing to test.
{
}


void QualityCoordinate::makeWorldAbsolute (Vector<Double>&) const
//
// By definition, for QualityCoordinate, world abs = rel
//
// there is nothing to test.
{
}



// Private functions
Bool QualityCoordinate::toWorld(Double& world, const Double pixel) const
{
	// implicitly tested via the public method
	// toWorld()
	Int index = Int(pixel + 0.5);
    if (index >= 0 && index < nValues_p) {
    	world = values_p[index];
    	return True;
    }
    else {
    	ostringstream os;
    	os << "Pixel " << index << " is out of range [0.." << nValues_p-1 << "]";
    	set_error(os);
    	return False;
    }
}


Bool QualityCoordinate::toPixel(Double& pixel,  const Double world) const
{
	// implicitly tested via the public method
	// toWorld()
    Bool found = False;
    Int index;
    for (index=0; index<nValues_p; index++) {
    	found = casacore::near(world, Double(values_p[index]));
    	if (found) break;
    }
    if (!found) {
    	ostringstream os;
        Quality::QualityTypes t0 = toWorld(world);
        String t = Quality::name(t0);
        os << "Quality value " << t << " is not contained in this QualityCoordinate";
        set_error(os);
        return False;
    }
    pixel = Double(index);
    return True;
}

Bool QualityCoordinate::setWorldMixRanges (const IPosition&)
{
	// is identical to setDefaultWorldMixRanges()
	setDefaultWorldMixRanges();
	return True;
}


void QualityCoordinate::setDefaultWorldMixRanges ()
{
	// implicitly tested via the constructor
	Vector<Double> pixel(nPixelAxes());
	pixel(0) = 0;
	toWorld(worldMin_p, pixel);
	pixel(0) = nValues_p - 1;
	toWorld(worldMax_p, pixel);
}

} //# NAMESPACE CASACORE - END

