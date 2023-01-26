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


#include <casacore/coordinates/Coordinates/QualityCoordinate.h>
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


QualityCoordinate::QualityCoordinate(const Vector<int32_t> &whichQuality)
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

uint32_t QualityCoordinate::nPixelAxes() const
{
	// tested: tQualityCoordinate: 181
    return 1;
}

uint32_t QualityCoordinate::nWorldAxes() const
{
	// tested: tQualityCoordinate: 188
	return 1;
}

bool QualityCoordinate::toWorld(Quality::QualityTypes &quality, int32_t pixel) const
{
	// tested: tQualityCoordinate: 443
    double world;
    if (toWorld (world, static_cast<double>(pixel))) {
       quality = Quality::type(values_p[pixel]);
       return true;
    }
    return false;
}


bool QualityCoordinate::toPixel(int32_t& pixel, Quality::QualityTypes quality) const
{
	// tested: tQualityCoordinate: 437
    double tmp;
    if (toPixel(tmp, static_cast<double>(quality))) {
       pixel = int32_t(tmp + 0.5);    
       return true;
    }
    return false;
}


bool QualityCoordinate::toWorld(Vector<double>& world,
			       const Vector<double>& pixel, bool) const
{
	// tested: tQualityCoordinate: 403
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


bool QualityCoordinate::toPixel(Vector<double> &pixel,
    	                       const Vector<double> &world) const
{
	// tested: tQualityCoordinate: 411
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

double QualityCoordinate::toWorld (Quality::QualityTypes quality)
{
	// tested: tQualityCoordinate: 456
    return static_cast<double>(quality);
}


Quality::QualityTypes QualityCoordinate::toWorld (double world)
{
	// tested: tQualityCoordinate: 456
    int32_t i = int32_t(world + 0.5);
    if (i < 0 ||  i>=Quality::NumberOfTypes) {
       return Quality::Undefined;
    }
//
    return static_cast<Quality::QualityTypes>(i);
}


Vector<int32_t> QualityCoordinate::quality() const
{
	// tested: tQualityCoordinate: 257, 435
    return Vector<int32_t>(values_p.begin(), values_p.end());
}


void QualityCoordinate::setQuality (const Vector<int32_t> &whichQuality)
{
	// implicitly tested via the constructor
    AlwaysAssert(whichQuality.nelements()>0, AipsError);

    // Make sure the quality occur at most once
    Block<bool> alreadyUsed(Quality::NumberOfTypes);
    alreadyUsed = false;
    for (uint32_t i=0; i<whichQuality.nelements(); i++) {
    	if (alreadyUsed[whichQuality(i)]) {
    		throw(AipsError("You have specified the same Quality more than once"));
    	}
    	alreadyUsed[whichQuality(i)] = true;
    }

    //
    values_p.resize(whichQuality.nelements());
    values_p = makeBlock(whichQuality);
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

Vector<double> QualityCoordinate::referencePixel() const
{
	// tested: tQualityCoordinate: 315
    Vector<double> crpix(1);
    crpix = crpix_p;
    return crpix;
}

Matrix<double> QualityCoordinate::linearTransform() const
{
	// tested: tQualityCoordinate: 307
    Matrix<double> matrix(1,1);
    matrix(0,0) = matrix_p;
    return matrix;
}

Vector<double> QualityCoordinate::increment() const
{
	// tested: tQualityCoordinate: 307
    Vector<double> cdelt(1);
    cdelt = cdelt_p;
    return cdelt;
}

Vector<double> QualityCoordinate::referenceValue() const
{
	// tested: tQualityCoordinate: 299
    Vector<double> crval(1);
    crval = crval_p;
    return crval;
}

bool QualityCoordinate::setWorldAxisNames(const Vector<String> &names)
{
	// tested: tQualityCoordinate: 204
    bool ok = names.nelements()==1;
    if (!ok) {
       set_error ("names vector must be of length 1");
    } else {
       name_p = names(0);
    }
    return ok;
}

bool QualityCoordinate::setWorldAxisUnits(const Vector<String> &)
{
	// tested: tQualityCoordinate: 227, 242
    return true;
}

bool QualityCoordinate::setReferencePixel(const Vector<double> &)
{
	// tested: tQualityCoordinate: 365
	return true;
}


bool QualityCoordinate::setLinearTransform(const Matrix<double> &)
{
	// tested: tQualityCoordinate: 380
	return true;
}

bool QualityCoordinate::setIncrement(const Vector<double> &)
{
	// tested: tQualityCoordinate: 350
   return true;
}

bool QualityCoordinate::setReferenceValue(const Vector<double> &)
{
	// tested: tQualityCoordinate: 336
	return true;
}


bool QualityCoordinate::near(const Coordinate& other,
                            double tol) const
{
	// tested: basic test criteria in many
	// tests in tQualityCoordinate
	Vector<int32_t> excludeAxes;
   return near(other, excludeAxes, tol);
}

bool QualityCoordinate::near(const Coordinate& other,
                            const Vector<int32_t>& excludeAxes,
                            double) const
{
	// tested: basic test criteria in many
	// tests in tQualityCoordinate
	if (other.type() != this->type()) {
		set_error("Comparison is not with another QualityCoordinate");
		return false;
	}

	// Check name
	const QualityCoordinate& sCoord = dynamic_cast<const QualityCoordinate&>(other);
	if (name_p != sCoord.name_p) {
		set_error("The QualityCoordinates have differing world axis names");
		return false;
	}

	// Number of pixel and world axes is the same for a QualityCoordinate
	// and it always 1. So if excludeAxes contains "0" we are done.
	// Add an assertion check should this change
	bool found;
	if (linearSearch(found, excludeAxes, 0, excludeAxes.nelements()) >= 0)
		return true;


	// The only other thing that really matters in the QualityCoordinate
	// is the values along the axis.    Nothing else (e.g. crval_p etc)
	// is ever actually used.
	if (nValues_p != sCoord.nValues_p) {
		set_error("The QualityCoordinates have different numbers of Quality values");
		return false;
	}

	return true;
}

bool QualityCoordinate::doNearPixel (const Coordinate& other,
                                    const Vector<bool>&,
                                    const Vector<bool>&,
                                    double) const
{
	// tested: tQualityCoordinate: 568
	if (other.type() != Coordinate::QUALITY) {
		set_error("Other Coordinate type is not Quality");
		return false;
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
		return false;
	}

	//
	return true;
}



bool QualityCoordinate::save(RecordInterface &container,
			    const String &fieldName) const

{
	// tested: tQualityCoordinate: 267
    bool ok = !container.isDefined(fieldName);
    if (ok) {
		Record subrec;
		subrec.define("axes", worldAxisNames());
		//
		Vector<String> quality(nValues_p);
		for (int32_t i=0; i<nValues_p; i++) {
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
    Vector<int32_t> iquality(quality.nelements());
    for (uint32_t i=0; i<iquality.nelements(); i++) {
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
                                double worldValue,
                                uint32_t worldAxis,
                                bool, bool, int32_t, bool) const
//
// world  abs=rel for Quality
//
{
	// tested: tQualityCoordinate: 478
	units = worldAxisUnits()(worldAxis);
	return Quality::name(QualityCoordinate::toWorld (worldValue));
}



void QualityCoordinate::makePixelRelative (Vector<double>& pixel) const
//       
// rel = abs - ref
//
{ 
	// tested: tQualityCoordinate: 547
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
   
 
void QualityCoordinate::makePixelAbsolute (Vector<double>& pixel) const
//
// abs = rel + ref
//
{ 
	// tested: tQualityCoordinate: 558
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
   

void QualityCoordinate::makeWorldRelative (Vector<double>&) const
//
// By definition, for QualityCoordinate, world abs = rel
//
// there is nothing to test.
{
}


void QualityCoordinate::makeWorldAbsolute (Vector<double>&) const
//
// By definition, for QualityCoordinate, world abs = rel
//
// there is nothing to test.
{
}



// Private functions
bool QualityCoordinate::toWorld(double& world, const double pixel) const
{
	// implicitly tested via the public method
	// toWorld()
	int32_t index = int32_t(pixel + 0.5);
    if (index >= 0 && index < nValues_p) {
    	world = values_p[index];
    	return true;
    }
    else {
    	ostringstream os;
    	os << "Pixel " << index << " is out of range [0.." << nValues_p-1 << "]";
    	set_error(os);
    	return false;
    }
}


bool QualityCoordinate::toPixel(double& pixel,  const double world) const
{
	// implicitly tested via the public method
	// toWorld()
    bool found = false;
    int32_t index;
    for (index=0; index<nValues_p; index++) {
    	found = casacore::near(world, double(values_p[index]));
    	if (found) break;
    }
    if (!found) {
    	ostringstream os;
        Quality::QualityTypes t0 = toWorld(world);
        String t = Quality::name(t0);
        os << "Quality value " << t << " is not contained in this QualityCoordinate";
        set_error(os);
        return false;
    }
    pixel = double(index);
    return true;
}

bool QualityCoordinate::setWorldMixRanges (const IPosition&)
{
	// is identical to setDefaultWorldMixRanges()
	setDefaultWorldMixRanges();
	return true;
}


void QualityCoordinate::setDefaultWorldMixRanges ()
{
	// implicitly tested via the constructor
	Vector<double> pixel(nPixelAxes());
	pixel(0) = 0;
	toWorld(worldMin_p, pixel);
	pixel(0) = nValues_p - 1;
	toWorld(worldMax_p, pixel);
}

} //# NAMESPACE CASACORE - END

