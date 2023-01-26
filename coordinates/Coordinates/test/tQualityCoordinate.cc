//# tQualityCoordinate.cc: Test program for QualityCoordinate
//# Copyright (C) 1998,1999,2000,2001,2003,2004
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

 
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/coordinates/Coordinates/QualityCoordinate.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/Assert.h>


#include <casacore/casa/iostream.h>
#include <casacore/casa/namespace.h>

QualityCoordinate makeCoordinate(Vector<int32_t>& whichQuality,
                                Vector<String>& qualityStrings);

void doit (QualityCoordinate& lc,
		const Vector<int32_t>& whichQuality,
		bool verbose=true);

void doit2 (QualityCoordinate& lc,
		   const Vector<int32_t>& whichQuality,
           bool verbose=true);

void doit3 (QualityCoordinate& lc,
		   const Vector<int32_t>& whichQuality,
           const Vector<String>& qualityStrings,
           bool verbose);

void doit4(QualityCoordinate& lc,
		   bool verbose);
void doit5(bool verbose);
void doit6(QualityCoordinate& lc, bool verbose);

int main()
{
   try {

      Vector<int32_t> whichQuality;
      Vector<String> qualityStrings;
      bool verbose=false;

      // Constructors
      {
         QualityCoordinate lc = makeCoordinate(whichQuality, qualityStrings);
      }

      // Test near function
      {
    	  QualityCoordinate lc  = makeCoordinate(whichQuality, qualityStrings);
    	  QualityCoordinate lc2 = makeCoordinate(whichQuality, qualityStrings);
    	  if (!lc.near(lc2)) {
    		  throw(AipsError("Failed near test 1"));
    	  }
    	  Vector<int32_t> excludeAxes(1, 0);
    	  if (!lc.near(lc2, excludeAxes)) {
    		  throw(AipsError("Failed near test 2"));
    	  }
      }

      // Test the rest
      {
         QualityCoordinate lc  = makeCoordinate(whichQuality, qualityStrings);
         doit(lc, whichQuality, verbose);
         doit2(lc, whichQuality, verbose);
      }
      {
         QualityCoordinate lc  = makeCoordinate(whichQuality, qualityStrings);
         doit3(lc, whichQuality, qualityStrings, verbose);
      }
      {
         QualityCoordinate lc  = makeCoordinate(whichQuality, qualityStrings);
         doit4(lc, verbose);
      }
      {
         doit5(verbose);
      }
      {
         QualityCoordinate lc  = makeCoordinate(whichQuality, qualityStrings);
         doit6(lc, verbose);
      }
   } catch (std::exception& x) {
      cerr << "aipserror: error " << x.what() << endl;
      return (1);
   }

   cout << "ok" << endl; 
   return (0);
}


QualityCoordinate makeCoordinate(Vector<int32_t>& whichQuality,
                                Vector<String>& qualityStrings)
{
	// choose all quality types
	whichQuality.resize(2);
	whichQuality(0) = Quality::DATA;
	whichQuality(1) = Quality::ERROR;

	qualityStrings.resize(2);
	qualityStrings(0) = "DATA";
	qualityStrings(1) = "ERROR";

	// generate the coosys
	return QualityCoordinate(whichQuality);
}
 


void doit (QualityCoordinate& lc,
           const Vector<int32_t>& whichQuality, bool verbose)
{

	// Test copy constructor
	{
		QualityCoordinate lc2(lc);
		if (!lc.near(lc2)) {
			throw(AipsError("Failed copy constructor test"));
		}
		if (verbose)
			cout << "Passed copy constructor test!" << endl;
	}

	// Test assignment
	{
		Vector<int32_t> whichQuality2(1); whichQuality2(0) = Quality::DATA;
		QualityCoordinate lc2 = QualityCoordinate(whichQuality2);
		lc2 = lc;
		if (!lc.near(lc2)) {
			throw(AipsError("Failed assignment test"));
		}
		if (verbose)
			cout << "Passed assignment test!" << endl;
	}


	// Test member functions
	if (lc.type() != Coordinate::QUALITY) {
		throw(AipsError("Failed type test"));
	}
	if (verbose)
		cout << "Passed type test!" << endl;

	//
	if (lc.showType() != "Quality") {
		throw(AipsError("Failed showType test"));
	}
	if (verbose)
		cout << "Passed showType test!" << endl;

	//
	if (lc.nPixelAxes() != 1) {
		throw(AipsError("Failed nPixelAxes test"));
	}
	if (verbose)
		cout << "Passed nPixelAxes test!" << endl;

	//
	if (lc.nWorldAxes() != 1) {
		throw(AipsError("Failed nWorldAxes test"));
	}
	if (verbose)
		cout << "Passed nWorldAxes test!" << endl;

	//
	Vector<String> axisNames(1); axisNames(0) = "Quality";
	if (!allEQ(axisNames, lc.worldAxisNames())) {
		throw(AipsError("Failed world axis name recovery test"));
	}
	if (verbose)
		cout << "Passed world axis name recovery test!" << endl;

	//
	axisNames(0) = "Horsies";
	if (!lc.setWorldAxisNames(axisNames)) {
		throw(AipsError(String("Failed to set world axis name because") + lc.errorMessage()));
	}
	if (verbose)
		cout << "Passed to set world axis name!" << endl;

	//
	if (!allEQ(axisNames, lc.worldAxisNames())) {
		throw(AipsError("Failed axis name set/recovery test"));
	}
	if (verbose)
		cout << "Passed axis name set/recovery test!" << endl;

	//
	// There is no unit we can set
	//
	Vector<String> axisUnits(1); axisUnits(0) = "";
	if (!allEQ(axisUnits, lc.worldAxisUnits())) {
		throw(AipsError("Failed world axis units recovery test"));
	}
	if (verbose)
		cout << "Passed world axis units test!" << endl;
	//
	if (!lc.setWorldAxisUnits(axisUnits)) {
		throw(AipsError(String("Failed to set world axis units because ") + lc.errorMessage()));
	}
	if (verbose)
		cout << "Passed to set world axis units!" << endl;

	//
	if (!allEQ(axisUnits, lc.worldAxisUnits())) {
		throw(AipsError("Failed world axis units set/recovery test"));
	}
	if (verbose)
		cout << "Passed world axis units set/recovery test!" << endl;

	//
	axisUnits(0) = "Mulies";
	if (!lc.setWorldAxisUnits(axisUnits)) {
		throw(AipsError(String("Failed to set world axis units because ") + lc.errorMessage()));
	}
	if (verbose)
		cout << "Passed to set world axis units!" << endl;

	//
	if (allEQ(axisUnits, lc.worldAxisUnits())) {
		throw(AipsError(String("World axis units set/recovery succeeded unexpectedly!")));
	}
	if (verbose)
		cout << "Failed as expected as world axis units can not be set/recovered!" << endl;

	//
	if (!allEQ(whichQuality, lc.quality())) {
		throw(AipsError("Failed Quality recovery test"));
	}
	if (verbose)
		cout << "Passed quality recovery test!" << endl;

	//
	//
	// Test record saving
	//
	TableRecord rec;
	if (!lc.save(rec, "Quality")) {
		throw(AipsError("Coordinate saving to Record failed"));
	}
	if (verbose)
		cout << "Passed coordinate saving to Record !" << endl;

	//
	QualityCoordinate* plc = QualityCoordinate::restore(rec, "Quality");
	if (!plc->near(lc, 1e-6)) {
		throw(AipsError("Coordinate reflection through record interface failed"));
	}
	if (verbose)
		cout << "Passed coordinate reflection through record interface!" << endl;
	delete plc;

	// Test clone
	//
	Coordinate* plc2 = lc.clone();
	if (!plc2->near(lc, 1e-6)) {
		throw(AipsError("Clone function failed"));
	}
	if (verbose)
		cout << "Passed clone function!" << endl;
	delete plc2;
}


void doit2 (QualityCoordinate& lc,
            const Vector<int32_t>& whichQuality, bool verbose)
{
	Vector<double> crval(1); crval(0) = double(whichQuality(0));
	if (!allEQ(crval, lc.referenceValue())) {
		throw(AipsError("Failed reference value recovery test"));
	}
	if (verbose)
		cout << "Passed reference value recovery test!" << endl;

	//
	Vector<double> cdelt(1); cdelt(0) = 1.0;
	if (!allEQ(cdelt, lc.increment())) {
		throw(AipsError("Failed increment recovery test"));
	}
	if (verbose)
		cout << "Passed increment recovery test!" << endl;

	//
	Vector<double> crpix(1); crpix(0) = 0.0;
	if (!allEQ(crpix, lc.referencePixel())) {
		throw(AipsError("Failed reference pixel recovery test"));
	}
	if (verbose)
		cout << "Passed reference pixel recovery test!" << endl;

	//
	Matrix<double> xform(1,1); xform(0,0) = 1.0;
	if (!allEQ(xform, lc.linearTransform())) {
		throw(AipsError("Failed Quality transform recovery test"));
	}
	if (verbose)
		cout << "Passed quality transform recovery test!" << endl;

	//
	Vector<double> oldRefVal = lc.referenceValue();
	Vector<double> oldIncr = lc.increment();
	Vector<double> oldRefPix = lc.referencePixel();
	Matrix<double> oldLinTr = lc.linearTransform();

	crval(0) = 111.1;
	if (!lc.setReferenceValue(crval)) {
		throw(AipsError(String("Failed to set reference value because") + lc.errorMessage()));
	}
	if (verbose)
		cout << "Passed to set reference value!" << endl;

	if (!allEQ(oldRefVal, lc.referenceValue())) {
      throw(AipsError("Failed reference value set/recovery test"));
	}
	if (verbose)
		cout << "Passed reference value set/recovery test!" << endl;

	//
	cdelt(0) = -10.3;
	if (!lc.setIncrement(cdelt)) {
		throw(AipsError(String("Failed to set increment because") + lc.errorMessage()));
	}
	if (verbose)
		cout << "Passed to set increment!" << endl;

	//
	if (!allEQ(oldIncr, lc.increment())) {
      throw(AipsError("Failed increment set/recovery test"));
	}
	if (verbose)
		cout << "Passed increment set/recovery test!" << endl;

	//
	crpix(0) = 23.0;
	if (!lc.setReferencePixel(crpix)) {
		throw(AipsError(String("Failed to set reference pixel because") + lc.errorMessage()));
	}
	if (verbose)
		cout << "Passed to set reference pixel!" << endl;

	//
	if (!allEQ(oldRefPix, lc.referencePixel())) {
		throw(AipsError("Failed reference pixel set/recovery test"));
	}
	if (verbose)
		cout << "Passed reference pixel set/recovery test!" << endl;

	//
	xform.diagonal() = -2.0;
	if (!lc.setLinearTransform(xform)) {
		throw(AipsError(String("Failed to set linear transform because") + lc.errorMessage()));
	}
	if (verbose)
		cout << "Passed to set linear transform!" << endl;

	if (!allEQ(oldLinTr, lc.linearTransform())) {
		throw(AipsError("Failed linear transform set/recovery test"));
	}
	if (verbose)
		cout << "Passed linear transform set/recovery test!" << endl;
}


void doit3 (QualityCoordinate& lc,
            const Vector<int32_t>& whichQuality,
            const Vector<String>& qualityStrings, bool verbose)
{
	//
	// Test conversion
	//
	Vector<double> pixel(1), world;
	pixel(0) = lc.referencePixel()(0);
	if (!lc.toWorld(world, pixel)) {
		throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
	}
	if (verbose)
		cout << "Passed toWorld conversion!" << endl;

	//
	Vector<double> pixel2(1);
	if (!lc.toPixel(pixel2, world)) {
		throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
	}
	if (verbose)
		cout << "Passed toPixel conversion!" << endl;

	if (!allNear(pixel2, pixel, 1e-6)) {
		throw(AipsError("Coordinate conversion reflection 1 failed"));
	}
	if (verbose)
		cout << "Passed toPixel conversion!" << endl;

	//
	world(0) = -10000.0;
	if (lc.toPixel(pixel2, world)) {
		throw(AipsError(String("toPixel succeeded unexpectedly")));
	} else {
		if (verbose)
			cout << "Failed as expected with" << lc.errorMessage() << endl;
	}

	//
	int32_t pixel3;
	for (int32_t i=0; i<int32_t(whichQuality.nelements()); i++) {
		Quality::QualityTypes sType = Quality::type(lc.quality()(i));
		Quality::QualityTypes sType2;
		if (!lc.toPixel(pixel3, sType)) {
			throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
		}
		if (verbose)
			cout << "Passed toPixel conversion!" << endl;

		if (!lc.toWorld(sType2, pixel3)) {
			throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
		}
		if (verbose)
			cout << "Passed toWorld conversion!" << endl;

		if (sType != sType2) {
			throw(AipsError(String("Coordinate conversion and reflection failed because ") + lc.errorMessage()));
		}
		if (verbose)
			cout << "Passed coordinate conversion and reflection!" << endl;

		//
		double w = QualityCoordinate::toWorld(sType);
		sType2 = QualityCoordinate::toWorld(w);
		if (sType != sType2) {
			throw(AipsError(String("Coordinate conversion and reflection failed because ") + lc.errorMessage()));
		}
		if (verbose)
			cout << "Passed coordinate conversion and reflection!" << endl;
	}

	//
	// Formatting
	//
	String unit;
	for (uint32_t i=0; i<whichQuality.nelements(); i++) {
		pixel(0) = i;
		if (!lc.toWorld(world, pixel)) {
			throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
		}
		if (verbose)
			cout << "Passed toWorld conversion!" << endl;

		//
		String str = lc.format(unit, Coordinate::FIXED, world(0), 0, true,
				true, 4);
		if (str != qualityStrings(i)) {
			throw(AipsError(String("formatting failed")));
		}
		if (verbose)
			cout << "Passed formatting!" << endl;
	}
}   


void doit4(QualityCoordinate& lc, bool verbose)
{
	Vector<bool> axes(lc.nWorldAxes(), true);
	Vector<int32_t> shape(lc.nPixelAxes(), 10);
	bool failed = false;
	Coordinate* pC = 0;
	try {
		pC = lc.makeFourierCoordinate (axes, shape);
	} catch (std::exception& x) {
		failed = true;
	}
	if (!failed) {
		throw(AipsError("Failed to induce forced error (1) in makeFourierCoordinate"));
	}
	else{
		if (verbose)
			cout << "Succeeded to induce forced error (1) in makeFourierCoordinate!" << endl;
	}
	delete pC;
}
   
void doit5(bool verbose)
{

	// Test setQuality
	{
		Vector<int32_t> quality(1);
		quality(0) = Quality::DATA;
		Vector<String> qualityStrings(1);
		qualityStrings(0) = String("DATA");
		QualityCoordinate lc(quality);

		//
		quality.resize(2);
		qualityStrings.resize(2);
		quality(0) = Quality::DATA;
		quality(1) = Quality::ERROR;
		qualityStrings(0) = String("DATA");
		qualityStrings(1) = String("ERROR");
		lc.setQuality(quality);

		//
		Vector<int32_t> quality2 = lc.quality();
		AlwaysAssert(quality2.nelements()==2, AipsError);
		AlwaysAssert(Quality::type(quality2(0))==Quality::DATA, AipsError);
		AlwaysAssert(Quality::type(quality2(1))==Quality::ERROR, AipsError);

		//
		doit(lc, quality, verbose);
		doit3(lc, quality, qualityStrings, verbose);
   }
}

void doit6(QualityCoordinate& lc, bool verbose)
{
	{
		Vector<double> absPix(1);
		absPix(0) = 0.0;
		lc.makePixelRelative(absPix);
		if (!allNear(absPix, 0.0, 1.0e-05))
			throw(AipsError("Failed to convert value to relative!"));
		else
			if (verbose)
				cout << "Succeeded to convert value to relative!" << endl;

	}
	{
		Vector<double> relPix(1);
		relPix(0) = 0.0;
		lc.makePixelAbsolute(relPix);
		if (!allNear(relPix, 0.0, 1.0e-05))
			throw(AipsError("Failed to convert value to absolute!"));
		else
			if (verbose)
				cout << "Succeeded to convert value to absolute!" << endl;
	}
	{
		Coordinate *lc2 = lc.clone();
		Vector<bool> b1(1), b2(1);
		if (!lc.doNearPixel(*lc2, b1, b2))
			throw(AipsError("Failed to find doNear values!"));
		else
			if (verbose)
				cout << "Succeeded to find doNear values!" << endl;
                delete lc2;

		Vector<int32_t> newQuality(1);
		newQuality.resize(1);
		newQuality(0) = Quality::ERROR;
		Coordinate *lc3 = new QualityCoordinate(newQuality);
		if (lc.doNearPixel(*lc3, b1, b2)) 
			throw(AipsError("Unexpectedly succeeded to find doNear values!"));
		else
			if (verbose)
				cout << "Failed as expected to find doNear values!" << endl;
                delete lc3;
	}
}
