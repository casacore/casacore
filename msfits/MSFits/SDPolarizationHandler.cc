//# SDPolarizationHandler.cc: an POLARIZATION handler for SDFITS data  
//# Copyright (C) 2000
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
//# $Id$

//# Includes
#include <casacore/msfits/MSFits/SDPolarizationHandler.h>

#include <casacore/tables/Tables/ColumnsIndex.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSPolColumns.h>
#include <casacore/ms/MeasurementSets/MSPolarization.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/measures/Measures/Stokes.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/Arrays/ArrayLogical.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

SDPolarizationHandler::SDPolarizationHandler() 
    : index_p(0), msPol_p(0), msPolCols_p(0), rownr_p(-1)
{;}

SDPolarizationHandler::SDPolarizationHandler(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row) 
    : index_p(0), msPol_p(0), msPolCols_p(0), rownr_p(-1)
{
    initAll(ms, handledCols, row);
}

SDPolarizationHandler::SDPolarizationHandler(const SDPolarizationHandler &other) 
    : index_p(0), msPol_p(0), msPolCols_p(0), rownr_p(-1)
{
    *this = other;
}

SDPolarizationHandler &SDPolarizationHandler::operator=(const SDPolarizationHandler &other)
{
    if (this != &other) {
	clearAll();
	index_p = new ColumnsIndex(*(other.index_p));
	AlwaysAssert(index_p, AipsError);
	// need to avoid the assignment operator here because we want
	// this to point to the field in index_p, not in other.index_p
	numCorrKey_p.attachToRecord(index_p->accessKey(),
				    MSPolarization::columnName(MSPolarization::NUM_CORR));
	msPol_p = new MSPolarization(*(other.msPol_p));
	AlwaysAssert(msPol_p, AipsError);
	msPolCols_p = new MSPolarizationColumns(*msPol_p);
	AlwaysAssert(msPolCols_p, AipsError);
	rownr_p = other.rownr_p;
	numCorrField_p = other.numCorrField_p;
	corrTypeField_p = other.corrTypeField_p;
	corrProductField_p = other.corrProductField_p;
	flagRowField_p = other.flagRowField_p;
    }
    return *this;
}

void SDPolarizationHandler::attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
{
    clearAll();
    initAll(ms, handledCols, row);
}

void SDPolarizationHandler::resetRow(const Record &row) 
{
    clearRow();
    Vector<Bool> dummyCols(row.nfields());
    initRow(dummyCols, row);
}

void SDPolarizationHandler::fill(const Record &, const Vector<Int> &stokes)
{
    // don't bother unless there is something there
    if (msPol_p) {
	*numCorrKey_p = stokes.nelements();
	Bool found = False;
	Vector<uInt> foundRows = index_p->getRowNumbers();
	uInt whichOne = 0;
	while (!found && whichOne<foundRows.nelements()) {
	    if (allEQ(stokes, msPolCols_p->corrType()(foundRows(whichOne))) &&
		(!flagRowField_p.isAttached() || *flagRowField_p == msPolCols_p->flagRow()(foundRows(whichOne)))) {
		// we have a winner
		found = True;
	    } else {
		whichOne++;
	    }
	}
	if (found) {
	    rownr_p = foundRows(whichOne);
	} else {
	    // we need to add one
	    rownr_p = msPol_p->nrow();
	    msPol_p->addRow();
	    msPolCols_p->numCorr().put(rownr_p, *numCorrKey_p);
	    msPolCols_p->corrType().put(rownr_p, stokes);
	    Matrix<Int> corrProduct(2,*numCorrKey_p);
	    // can we reuse whats alread in the row from when this was a MS
	    if (numCorrField_p.isAttached() && *numCorrField_p == *numCorrKey_p &&
		corrTypeField_p.isAttached() && allEQ(*corrTypeField_p, stokes) &&
		corrProductField_p.isAttached()) {
		// apparently so
		corrProduct = *corrProductField_p;
	    } else {
		// construct the corrProduct given the stokes values
		// first, we need to determine the decomposition of the stokes values
		SimpleOrderedMap<Int, Int> polTypeMap(-1);
		for (uInt i=0;i<stokes.nelements();i++) {
		    Int key1, key2;
		    stokesKeys(stokes(i), key1, key2);
		    if (!polTypeMap.isDefined(key1)) {
			polTypeMap.define(key1, polTypeMap.ndefined());
		    }
		    if (key2 != key1 && !polTypeMap.isDefined(key2)) {
			polTypeMap.define(key2, polTypeMap.ndefined());
		    }
		}
		// now re-assemble these into the corr product
		for (uInt i=0;i<stokes.nelements();i++) {
		    Int key1, key2;
		    stokesKeys(stokes(i), key1, key2);
		    corrProduct(0,i) = polTypeMap(key1);
		    corrProduct(1,i) = polTypeMap(key2);
		}
	    }
	    // and insert it into the table
	    msPolCols_p->corrProduct().put(rownr_p, corrProduct);
	    if (flagRowField_p.isAttached()) {
		msPolCols_p->flagRow().put(rownr_p, *flagRowField_p);
	    } else {
		msPolCols_p->flagRow().put(rownr_p, False);
	    }
	}
    }
}

void SDPolarizationHandler::clearAll()
{
    delete index_p;
    index_p = 0;

    delete msPol_p;
    msPol_p = 0;

    delete msPolCols_p;
    msPolCols_p = 0;

    clearRow();
}

void SDPolarizationHandler::clearRow()
{
    rownr_p = -1;
    numCorrField_p.detach();
    corrTypeField_p.detach();
    corrProductField_p.detach();
}

void SDPolarizationHandler::initAll(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
{
    msPol_p = new MSPolarization(ms.polarization());
    AlwaysAssert(msPol_p, AipsError);

    msPolCols_p = new MSPolarizationColumns(*msPol_p);
    AlwaysAssert(msPolCols_p, AipsError);

    index_p = new ColumnsIndex(*msPol_p, 
			       MSPolarization::columnName(MSPolarization::NUM_CORR));
    AlwaysAssert(index_p, AipsError);
    
    numCorrKey_p.attachToRecord(index_p->accessKey(),
				MSPolarization::columnName(MSPolarization::NUM_CORR));

    initRow(handledCols, row);
}

void SDPolarizationHandler::initRow(Vector<Bool> &handledCols, const Record &row)
{
    rownr_p = -1;
    // try MS 2 version first, then MS 1
    Int ncorrId = row.fieldNumber("POLARIZATION_NUM_CORR");
    if (ncorrId < 0) ncorrId = row.fieldNumber("SPECTRAL_WINDOW_NUM_CORR");
    if (ncorrId >= 0) {
	numCorrField_p.attachToRecord(row, ncorrId);
	handledCols(ncorrId) = True;
    }
    Int corrTypeId = row.fieldNumber("POLARIZATION_CORR_TYPE");
    if (corrTypeId < 0) corrTypeId = row.fieldNumber("SPECTRAL_WINDOW_CORR_TYPE");
    if (corrTypeId >= 0) {
	corrTypeField_p.attachToRecord(row, corrTypeId);
	handledCols(corrTypeId) = True;
    }
    Int corrProductId = row.fieldNumber("POLARIZATION_CORR_PRODUCT");
    if (corrProductId < 0) corrProductId = row.fieldNumber("SPECTRAL_WINDOW_CORR_PRODUCT");
    if (corrProductId >= 0) {
	corrProductField_p.attachToRecord(row, corrProductId);
	handledCols(corrProductId) = True;
    }
    Int flagRowId = row.fieldNumber("POLARIZATION_FLAG_ROW");
    if (flagRowId >= 0) {
	flagRowField_p.attachToRecord(row, flagRowId);
	handledCols(flagRowId) = True;
    }	
}

void SDPolarizationHandler::stokesKeys(Int stokesValue, Int &key1, Int &key2)
{
    switch (Stokes::type(stokesValue)) {
	// the cases which are tricky are the cross products
	// we need to set the keys to something which remembers
	// that XX is a product of just a receptors while
	// XY and YX are procucts of the same two receptors and one
	// of theme is also the one used in XX.  So, use the non-cross
	// procucts to be synonymous with their receptors when deconstructing
	// the cross product values.
    case Stokes::RL:
	key1 = Stokes::RR;
	key2 = Stokes::LL;
	break;
    case Stokes::LR:
	key1 = Stokes::LL;
	key2 = Stokes::RR;
	break;
    case Stokes::XY:
	key1 = Stokes::XX;
	key2 = Stokes::YY;
	break;
    case Stokes::YX:
	key1 = Stokes::YY;
	key2 = Stokes::XX;
	break;
    case Stokes::RX:
	key1 = Stokes::RR;
	key2 = Stokes::XX;
	break;
    case Stokes::RY:
	key1 = Stokes::RR;
	key2 = Stokes::YY;
	break;
    case Stokes::LX:
	key1 = Stokes::LL;
	key2 = Stokes::XX;
	break;
    case Stokes::LY:
	key1 = Stokes::LL;
	key2 = Stokes::YY;
	break;
    case Stokes::XR:
	key1 = Stokes::XX;
	key2 = Stokes::RR;
	break;
    case Stokes::YR:
	key1 = Stokes::YY;
	key2 = Stokes::RR;
	break;
    case Stokes::XL:
	key1 = Stokes::XX;
	key2 = Stokes::LL;
	break;
    case Stokes::YL:
	key1 = Stokes::YY;
	key2 = Stokes::LL;
	break;
    case Stokes::PQ:
	key1 = Stokes::PP;
	key2 = Stokes::QQ;
	break;
    case Stokes::QP:
	key1 = Stokes::QQ;
	key2 = Stokes::PP;
    default:
	// the two keys are identical to each other and to the stokes type
	key1 = key2 = stokesValue;
	break;
    }
}

} //# NAMESPACE CASACORE - END

