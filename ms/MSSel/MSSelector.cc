//# MSSelector.cc: selection and iteration of an MS
//# Copyright (C) 1997,1998,1999,2000,2001,2002
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

#include <casacore/ms/MSSel/MSSelector.h>

#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/MaskArrMath.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableIter.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/TaQL/TableParse.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/ms/MeasurementSets/MSIter.h>
#include <casacore/ms/MeasurementSets/MSRange.h>
#include <casacore/ms/MSSel/MSSelUtil2.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSSelector::MSSelector():msIter_p(0),initSel_p(False),dataDescId_p(0),
		lastDataDescId_p(1,-1),useSlicer_p(False),
		haveSlicer_p(False),wantedOne_p(-1),convert_p(False),
		useIfrDefault_p(True)
{ }

MSSelector::MSSelector(MeasurementSet& ms):ms_p(ms),
		selms_p(ms),savems_p(ms),
		msIter_p(0),initSel_p(False),
		dataDescId_p(0),
		lastDataDescId_p(1,-1),
		useSlicer_p(False),
		haveSlicer_p(False),
		wantedOne_p(-1),convert_p(False),
		useIfrDefault_p(True)
{ }

MSSelector::MSSelector(const MSSelector& other):msIter_p(0)
{ operator=(other);}

MSSelector& MSSelector::operator=(const MSSelector& other)
{
	if (this==&other) return *this;
	ms_p=other.ms_p;
	selms_p=other.selms_p;
	savems_p=other.savems_p;
	lastDataDescId_p=other.lastDataDescId_p;
	if (msIter_p) delete msIter_p;
	msIter_p = 0;
	if (other.msIter_p) msIter_p=new MSIter(*other.msIter_p);
	initSel_p=other.initSel_p;
	dataDescId_p=other.dataDescId_p;
	useSlicer_p=other.useSlicer_p;
	haveSlicer_p=other.haveSlicer_p;
	slicer_p=other.slicer_p;
	wantedOne_p=other.wantedOne_p;
	convert_p=other.convert_p;
	useIfrDefault_p=other.useIfrDefault_p;
	return *this;
}

MSSelector::~MSSelector() 
{
	if (msIter_p) delete msIter_p;
	msIter_p=0;
}

void MSSelector::setMS(MeasurementSet& ms)
{
	ms_p=ms;
	selms_p=ms;
	savems_p=ms;
	if (msIter_p) delete msIter_p;
	msIter_p=0;
	initSel_p=False;
	dataDescId_p=-1;
	useSlicer_p=False;
	haveSlicer_p=False;
	wantedOne_p=-1;
	convert_p=False;
	useIfrDefault_p=True;
}

Bool MSSelector::initSelection(const Vector<Int>& dataDescId, Bool reset)
{
	LogIO os;
	// first check if we want to throw all selections away & return the pristine
	// MeasurementSet
	if (reset) {
		selms_p=ms_p;
		initSel_p=False;
		dataDescId_p.resize(0);
		lastDataDescId_p.resize(0);
		useSlicer_p=False;
		polSlice_p=Slice();
		chanSlice_p=Slice();
		haveSlicer_p=False;
		wantedOne_p=-1;
		convert_p=False;
		useIfrDefault_p=True;
		return True;
	}

	// check if we can reuse the saved selection
	if (initSel_p && dataDescId.nelements()==lastDataDescId_p.nelements() &&
			allEQ(dataDescId,lastDataDescId_p)) {
		selms_p=savems_p;
		return True;
	} else {
		// undo all previous selections
		selms_p=ms_p;
		ifrSelection_p.resize(0);
		rowIndex_p.resize(0,0);
	}
	// selection on data description is optional
	Bool constantShape=True;
	if (ms_p.dataDescription().nrow()<=1) {
		if (dataDescId.nelements()>1) {
			os << LogIO::NORMAL << "data desc id selection ignored, "
					"there is only one" << LogIO::POST;
		}
	} else {
		if (dataDescId.nelements()>0 && dataDescId(0)!=-1) {
			selms_p=selms_p(selms_p.col(MS::columnName(MS::DATA_DESC_ID))
					.in(dataDescId));
		}
	}
	// check if the data shape is the same for all data
	// if not: select first data desc id only
	ROScalarColumn<Int> dd(selms_p,MS::columnName(MS::DATA_DESC_ID));
	ROMSDataDescColumns ddc(selms_p.dataDescription());
	Vector<Int> ddId=dd.getColumn();
	Int ndd=GenSort<Int>::sort(ddId, Sort::Ascending,
			Sort::HeapSort | Sort::NoDuplicates);
	ddId.resize(ndd,True);
	dataDescId_p.resize(ndd); dataDescId_p=ddId;
	ROMSSpWindowColumns spwc(ms_p.spectralWindow());
	ROMSPolarizationColumns polc(ms_p.polarization());
	spwId_p.resize(ndd);
	polId_p.resize(ndd);
	for (Int i=0; i<ndd; i++) {
		spwId_p(i)=ddc.spectralWindowId()(ddId(i));
		polId_p(i)=ddc.polarizationId()(ddId(i));
	}

	for (Int i=1; i<ndd; i++) {
		if (spwc.numChan()(spwId_p(i)) != spwc.numChan()(spwId_p(i-1)) ||
				polc.numCorr()(polId_p(i)) != polc.numCorr()(polId_p(i-1))) {
			constantShape = False;
			break;
		}
	}
	if (!constantShape) {
		selms_p=selms_p(selms_p.col(MS::columnName(MS::DATA_DESC_ID))
				== ddId(0));
		os<< LogIO::WARN << "Data shape varies, selected first data desc id"
				" only"<< LogIO::POST;
		dataDescId_p.resize(1);dataDescId_p(0)=ddId(0);
		spwId_p.resize(1,True);
		polId_p.resize(1,True);
	}
	lastDataDescId_p.resize(0); lastDataDescId_p=dataDescId_p;

	// if selection is empty, reject it
	initSel_p = (selms_p.nrow()>0);
	if (!initSel_p) {
		os<< LogIO::WARN << "Selected Table has zero rows"<<LogIO::POST;
	} else {
		// save the current selection
		savems_p=selms_p;
		// Set channel selection to entire spectrum
		ROMSSpWindowColumns spwc(selms_p.spectralWindow());
		selectChannel(spwc.numChan()(spwId_p(0)),0,1,1);

		// Set polarization selection/conversion to all present
		ROMSPolarizationColumns polc(selms_p.polarization());
		Vector<Int> pols=polc.corrType()(polId_p(0));
		Vector<String> polSel(pols.nelements());
		for (uInt i=0; i<pols.nelements(); i++) {
			polSel(i)=Stokes::name(Stokes::type(pols(i)));
		}
		// check that other polarization ids have the same polarizations
		Bool polVaries = False;
		for (uInt i=1; i<polId_p.nelements(); i++) {
			if (polId_p(i)!=polId_p(i-1)) {
				Vector<Int> pols2=polc.corrType()(polId_p(i));
				for (uInt j=0; j<pols2.nelements(); j++) {
					if (pols2(j)!=pols(j)) {
						polVaries = True;
						break;
					}
				}
				if (polVaries) break;
			}
		}

		if (polVaries) {
			os<< LogIO::WARN << "Polarization type varies with row - "<<endl<<
					"do not use selectpolarization or results will be incorrect "
					"Assuming (falsely) the following polarizations are present:"
					<< LogIO::POST;
		}
		selectPolarization(polSel);
		os<< LogIO::DEBUG1 << "Selection initialized ok"<< LogIO::POST;
	}
	return constantShape;
}

Bool MSSelector::initSelection(Bool reset) {
	Vector<Int> ddIds;
	return initSelection(ddIds,reset);
}

Bool MSSelector::selectChannel(Int nChan, Int start, Int width, Int incr)
{
	LogIO os;
	if (!checkSelection()) return False;
	if (selms_p.nrow()==0) {
		os << LogIO::WARN << " Selected Table is empty - use selectinit"
				<< LogIO::POST;
		return False;
	}
	Bool ok = (nChan>0 && start>=0 && width>0 && incr>0);
	if (!ok) {
		os << LogIO::SEVERE << "Illegal channel selection"<<LogIO::POST;
		return False;
	}
	ROMSColumns msc(selms_p);
	Int numChan=msc.spectralWindow().numChan()(spwId_p(0));
	Int end=start+(nChan-1)*incr+(width-1);
	ok=(ok && end < numChan);
	if (!ok) {
		os << LogIO::SEVERE << "Illegal channel selection"<<LogIO::POST;
		return False;
	}
	chanSel_p.resize(4);
	chanSel_p(0)=nChan;
	chanSel_p(1)=start;
	chanSel_p(2)=width;
	chanSel_p(3)=incr;
	if (start>0 || end<(numChan-1) || incr>1 ) {
		if (width==1) {
			// width is one, we can use a stride
			chanSlice_p=Slice(start,nChan,incr);
		} else {
			chanSlice_p=Slice(start,1+(nChan-1)*incr+(width-1));
		}
	}
	useSlicer_p=(!polSlice_p.all()||!chanSlice_p.all());
	if (useSlicer_p) slicer_p=Slicer(polSlice_p,chanSlice_p);

	Int nSpW=spwId_p.nelements();
	Matrix<Double> chanFreq =
			msc.spectralWindow().chanFreq().getColumnCells(RefRows(spwId_p));
	Matrix<Double> bandwidth =
			msc.spectralWindow().resolution().getColumnCells(RefRows(spwId_p));
	for (Int i=0; i<nSpW; i++) {
		chanFreq_p.resize(nChan,nSpW); bandwidth_p.resize(nChan,nSpW);
		for (Int j=0; j<nChan; j++) {
			Int n=1;
			chanFreq_p(j,i)=chanFreq(start+incr*j,i);
			for (Int k=1; k<width; k++) {
				chanFreq_p(j,i)+=chanFreq(start+incr*j+k,i);
				n++;
			}
			if (n>1) {
				chanFreq_p(j,i)/=n;
			}
			bandwidth_p(j,i)=bandwidth(start+incr*j,i);
			if (n>1) {
				// Not correct if there are gaps between channels
				bandwidth_p(j,i)=(bandwidth(start,i)+bandwidth(start+incr*j+width-1,i))/2
						+ abs(chanFreq(start+incr*j+width-1,i)-chanFreq(start+incr*j,i));
			}
		}
	}
	os << LogIO::DEBUG1 << "Channel selection: #chan="<<nChan<<
			", start="<<start+1<<", width="<<width<<", incr="<<incr<<LogIO::POST;
	return True;
}

Bool MSSelector::selectPolarization(const Vector<String>& wantedPol)
{
	LogIO os;
	// this selection/conversion assumes that parallactic angle rotation
	// is taken care of elsewhere (i.e., results may only be correct for
	// CORRECTED_DATA and MODEL_DATA conversions, not for the observed DATA)

	if (selms_p.nrow()==0) {
		os << LogIO::WARN << " Selected Table is empty - use selectinit"
				<< LogIO::POST;
		return False;
	}
	// first convert strings to enums
	Int n=wantedPol.nelements();
	Vector<Int> wanted(n);
	for (Int i=0; i<n; i++) wanted(i)=Stokes::type(wantedPol(i));

	// check for duplicates
	for (Int i=0; i<n-1; i++) {
		for (Int j=i+1; j<n; j++) {
			if (wanted(i)==wanted(j)) {
				os << LogIO::WARN << " Duplicate polarizations in input not allowed -"
						<< wantedPol   << LogIO::POST;
				return False;
			}
		}
	}

	// now find out the input polarizations, assuming all selected data is
	// the same
	ROMSPolarizationColumns mspol(selms_p.polarization());
	Int numCorr=mspol.numCorr()(polId_p(0));
	Vector<Int> inputPol=mspol.corrType()(polId_p(0));

	// check if wanted is just a subset or permutation of inputPol
	subSet_p=True;
	for (Int j=0; j<n; j++) {
		Bool found=False;
		for (Int i=0; i<numCorr; i++) {
			if (wanted(j)==inputPol(i)) found=True;
		}
		if (!found) {
			subSet_p=False;
			break;
		}
	}
	if (subSet_p) {
		polIndex_p.resize(0);
		if (n==1) {
			for (Int i=0; i<numCorr; i++) {
				if (wanted(0)==inputPol(i)) {
					polSlice_p=Slice(i,1);
					break;
				}
			}
		} else if (n==2) {
			Int id1=-1,id2=-1;
			for (Int i=0; i<numCorr; i++) {
				if (inputPol(i)==wanted(0)) { id1=i; break;}
			}
			for (Int i=0; i<numCorr; i++) {
				if (inputPol(i)==wanted(1)) { id2=i; break;}
			}
			polSlice_p=Slice(min(id1,id2),2,abs(id1-id2));
			if (id2<=id1) {
				polIndex_p.resize(2);
				polIndex_p(0)=1; polIndex_p(1)=0;
			}
		} else {
			polIndex_p.resize(n);
			for (Int i=0; i<numCorr; i++) {
				for (Int j=0; j<n; j++) {
					if (inputPol(i)==wanted(j)) polIndex_p(j)=i;
				}
			}
			if (n==numCorr) {
				Int j=1;
				for (;j<n;j++) if (polIndex_p(j)<polIndex_p(j-1)) break;
				if (j==n) polIndex_p.resize(0); // want all in correct order
			}
		}
	} else {
		convert_p=True;
		// check validity
		for (Int i=0; i<n; i++) {
			if (wanted(i)==Stokes::Undefined) {
				os << LogIO::SEVERE << "Unrecognized polarization: "<<wantedPol(i)	  << LogIO::POST;
				return False;
			}
		}
		stokesConverter_p.setConversion(wanted,inputPol,True);
	}
	useSlicer_p=(!polSlice_p.all()||!chanSlice_p.all());
	if (useSlicer_p) slicer_p=Slicer(polSlice_p,chanSlice_p);
	polSelection_p.resize(wantedPol.nelements()); polSelection_p=wantedPol;
	os << LogIO::DEBUG1<< "Polarization selection: "<< wantedPol << LogIO::POST;
	return True;
}

Bool MSSelector::select(const Record& items, Bool oneBased)
{
	LogIO os;
	if (!checkSelection()) return False;
	if (selms_p.nrow()==0) {
		os << LogIO::WARN << " Selected Table is empty - use selectinit"
				<< LogIO::POST;
		return False;
	}
	Int n=items.nfields();
	for (Int i=0; i<n; i++) {
		String column=items.name(i);
		MSS::Field fld=MSS::field(column);
		column.upcase();
		switch (fld) {
		case MSS::ANTENNA1:
		case MSS::ANTENNA2:
		case MSS::ARRAY_ID:
		case MSS::DATA_DESC_ID:
		case MSS::FEED1:
		case MSS::FEED2:
		case MSS::FIELD_ID:
		case MSS::SCAN_NUMBER:
		{
			Vector<Int> id = items.asArrayInt(RecordFieldId(i));
			if (id.nelements()>0) {
				if (oneBased) id-=1;
				if (id.nelements()==1) {
					selms_p=selms_p(selms_p.col(column) == id(0));
				} else {
					selms_p=selms_p(selms_p.col(column).in(id));
				}
			} else {
				os<< LogIO::WARN << "Illegal value for item "<<downcase(column)<<
						LogIO::POST;
			}
		}
		break;
		case MSS::IFR_NUMBER:
		{
			Vector<Int> ifrNum = items.asArrayInt(RecordFieldId(i));
			// if (GlishArray(items.get(i)).get(ifrNum)) {
			if (ifrNum.nelements()>0) {
				// check input values for validity, squeeze out illegal values
				Int nAnt=selms_p.antenna().nrow();
				if (oneBased) ifrNum-=1001;
				for (uInt k=0; k<ifrNum.nelements(); k++) {
					if (ifrNum(k)/1000<0 || ifrNum(k)/1000>= nAnt ||
							ifrNum(k)%1000<0 || ifrNum(k)%1000>=nAnt) {
						for (uInt j=k; j<ifrNum.nelements()-1; j++) {
							ifrNum(j)=ifrNum(j+1);
						}
						ifrNum.resize(ifrNum.nelements()-1,True);
					}
				}
				if (ifrNum.nelements()==1) {
					selms_p=selms_p((selms_p.col(MS::columnName(MS::ANTENNA1))*1000+
							selms_p.col(MS::columnName(MS::ANTENNA2))) ==
									ifrNum(0));
				} else {
					selms_p=selms_p((selms_p.col(MS::columnName(MS::ANTENNA1))*1000+
							selms_p.col(MS::columnName(MS::ANTENNA2)))
							.in(ifrNum));
				}
				ifrSelection_p.reference(ifrNum);
				useIfrDefault_p=False;
			} else {
				useIfrDefault_p=True;
				ifrSelection_p.resize(0);
			}
			// } else {
			// os<< LogIO::WARN << "Illegal value for item "<<downcase(column)<<
			// LogIO::POST;
			// }
		}
		break;
		case MSS::ROWS:
		{
			Vector<Int> rows = items.asArrayInt(RecordFieldId(i));
			if (rows.nelements()>0) {
				Int n=rows.nelements();
				if (oneBased) rows-=1;
				Vector<uInt> uRows(n);
				convertArray(uRows,rows);
				// Select rows from the base table.
				// This is consistent with the rownumbers returned by range.
				selms_p=ms_p(uRows);
			}
		}
		break;
		case MSS::TIME:
		{
			Vector<Double> range = items.asArrayDouble(RecordFieldId(i));
			if (range.nelements()==2) {
				TableExprNodeSetElem elem(True,range(0),range(1),True);
				TableExprNodeSet set;
				set.add(elem);
				selms_p=selms_p(selms_p.col(column).in(set));
			} else {
				os << LogIO::WARN << "Illegal value for time range: "<<
						"two element numeric vector required"<<LogIO::POST;
			}
		}
		break;
		case MSS::TIMES:
		{
			column=MS::columnName(MS::TIME);
			Vector<Double> time = items.asArrayDouble(RecordFieldId(i));
			if (time.nelements()>0) {
				if (time.nelements()==1) {
					selms_p=selms_p(selms_p.col(column) == time(0));
				} else {
					selms_p=selms_p(selms_p.col(column).in(time));
				}
			}
		}
		break;
		case MSS::U:
		case MSS::V:
		case MSS::W:
		{
			Int uvwIndex=fld-MSS::U;
			Vector<Double> range = items.asArrayDouble(RecordFieldId(i));
			if (range.nelements()==2) {
				column=MS::columnName(MS::UVW);
				TableExprNodeSet interval;
				interval.add (TableExprNodeSetElem (True, range(0),
						range(1), True));
				IPosition idx(1,uvwIndex);
				selms_p=selms_p(selms_p.col(column)(idx).in(interval));
			} else {
				os << LogIO::WARN << "Illegal value for u, v, w range: "<<
						"two element numeric vector required"<<LogIO::POST;
			}
		}
		break;
		case MSS::UVDIST:
		{
			Vector<Double> range = items.asArrayDouble(RecordFieldId(i));
			if (range.nelements()==2) {
				range*=range; // square
				ROArrayColumn<Double> uvwcol(selms_p,MS::columnName(MS::UVW));
				Int nrow=selms_p.nrow();
				if (nrow>0) {
					Matrix<Double> uvw=uvwcol.getColumn();
					Block<Bool> rowsel(nrow);
					for (Int k=0; k<nrow; k++) {
						Double uvdist=square(uvw(0,k))+square(uvw(1,k));
						//attempt to cope with roundoff error in square
						rowsel[k]=
								(( uvdist >= range(0) || near(uvdist,range(0)) ) &&
										( uvdist <= range(1) || near(uvdist,range(1)) ) );
					}
					selms_p=selms_p(rowsel);
				}
			} else {
				os << LogIO::WARN << "Illegal value for uvdist range: "<<
						"two element numeric vector required"<<LogIO::POST;
			}
		}
		break;
		case MSS::UNDEFINED:
		default:
			os << LogIO::WARN << "Unrecognized field in input ignored: "<<
			downcase(column)<<LogIO::POST;
			break;
		}
	}
	if (selms_p.nrow()==0) {
		os << LogIO::WARN << " Selected Table is now empty - use selectinit"
				<< LogIO::POST;
		return False;
	}
	return True;
}

Bool MSSelector::select(const String& msSelect)
{
	LogIO os;
	if (!checkSelection()) return False;
	if (selms_p.nrow()==0) {
		os << LogIO::WARN << " Selected Table is empty - use selectinit"
				<< LogIO::POST;
		return False;
	}
	// check that a selection was given
	Int len = msSelect.length();
	Int nspace = msSelect.freq(' ');
	if (msSelect.empty() || nspace==len) return False;
	String parseString="select from $1 where " + msSelect;
	selms_p=tableCommand(parseString,selms_p).table();
	if (selms_p.nrow()==0) {
		os << LogIO::WARN << " Selected Table is now empty - use selectinit"
				<< LogIO::POST;
		return False;
	}
	return True;
}

static void averageId(Vector<Int>& vec)
{
	// "average" a vector of integer id's - replace by length 1 vector with
	// common value if all elements are the same, or -1 otherwise.
	// Used below to "average" things like antenna id.
	if (vec.nelements()>1) {
		Int aver=vec(0);
		if (!allEQ(vec,aver)) aver=-1;
		vec.resize(1);
		vec(0)=aver;
	}
}

static void averageDouble(Vector<Double>& vec) 
{
	// average a vector of doubles
	Int n=vec.nelements();
	if (n>1) {
		Double aver=vec(0);
		for (Int i=1; i<n; i++) aver+=vec(i);
		aver/=n;
		vec.resize(1);
		vec(0)=aver;
	}
}

Record MSSelector::getData(const Vector<String>& items, Bool ifrAxis,
		Int ifrAxisGap,
		Int inc, Bool average, Bool oneBased)
{
	LogIO os;
	Record out(RecordInterface::Variable);
	if (!checkSelection()) return out;
	if (selms_p.nrow()==0) {
		os << LogIO::WARN << " Selected Table is empty - use selectinit"
				<< LogIO::POST;
		return out;
	}

	Matrix<Bool> want(nFuncType,nDataType,False);
	Bool wantFlag, wantFlagSum, wantWeight, wantSigma;
	wantFlag=wantFlagSum=wantWeight=wantSigma=False;

	Matrix<Double> uvw;

	Int nItems=items.nelements(),nRows=selms_p.nrow();
	Table tab;
	if (inc>1 && inc<=nRows) {
		Vector<uInt> rows(nRows/inc);
		indgen(rows,uInt(0),uInt(inc));
		tab=selms_p(rows);
	} else {
		tab=selms_p;
	}
	ROMSColumns msc(tab);
	Int nIfr = ifrSelection_p.nelements();
	Int nSlot = 0;
	Int nRow = tab.nrow();
	if (ifrAxis && (nIfr==0 || useIfrDefault_p)) {
		// set default
		MSRange msRange(tab);
		ifrSelection_p.resize();
		ifrSelection_p = msRange.range(MSS::IFR_NUMBER).asArrayInt(0);
		// GlishArray(msRange.range(MSS::IFR_NUMBER).get(0)).get(ifrSelection_p);
		nIfr = ifrSelection_p.nelements();
		useIfrDefault_p=True;
	}

	Vector<Int> ifrIndex; // the index no into the ifrSelection Vector
	Vector<Double> timeSlot; // the time for each slot
	Vector<Int> ddSlot;   // the dataDescId for each slot
	Vector<Int> slot; // the slot for each row
	Bool doIfrAxis = (ifrAxis && nIfr>0);
	if (doIfrAxis) {
		if (ifrAxisGap>=0) {
			// add a small gap before each antenna1 change
			Int gapCount=0;
			for (Int i=1; i<nIfr; i++) {
				if (ifrSelection_p(i)/1000>ifrSelection_p(i-1)/1000) gapCount++;
			}
			ifrAxis_p.resize(nIfr+gapCount*ifrAxisGap);
			ifrAxis_p(0)=ifrSelection_p(0);
			for (Int i=1,j=1; i<nIfr; i++) {
				if (ifrSelection_p(i)/1000>ifrSelection_p(i-1)/1000) {
					for (Int k=0; k<ifrAxisGap; k++) ifrAxis_p(j++)=-1;
					ifrAxis_p(j++)=ifrSelection_p(i);
				} else {
					ifrAxis_p(j++)=ifrSelection_p(i);
				}
			}
			nIfr+=gapCount*ifrAxisGap;
		}

		// figure out which rows go with which slot
		// Assume MS is in time order
		Vector<Double> time=msc.time().getColumn();
		Vector<Int> dd=msc.dataDescId().getColumn();
		nRow=time.nelements();
		timeSlot.resize(nRow);
		ddSlot.resize(nRow);
		slot.resize(nRow);     // the time/datadescid slot
		timeSlot(0)=time(0); ddSlot(0)=dd(0); slot(0)=0;
		for (Int i=1; i<nRow; i++) {
			if (time(i)!=timeSlot(nSlot)) {
				// new time - add new slot
				nSlot++;
				timeSlot(nSlot)=time(i); ddSlot(nSlot)=dd(i);
				slot(i)=nSlot;
			} else if (dd(i)!=ddSlot(nSlot)) {
				// check if we've seen this dd before
				Int j=nSlot-1;
				while (j>=0 && timeSlot(j)==timeSlot(nSlot) &&
						ddSlot(j)!=dd(i)) j--;
				if (j<0 || (j>=0 && timeSlot(j)!=timeSlot(nSlot))) {
					// new data_desc_id for current time - add new slot
					nSlot++;
					timeSlot(nSlot)=time(i); ddSlot(nSlot)=dd(i);
					slot(i)=nSlot;
				} else {
					// we've seen this one before - reuse it
					slot(i)=j;
				}
			} else {
				slot(i)=nSlot;
			}
		}
		nSlot++;
		// resize to true size, copying values
		timeSlot.resize(nSlot,True);
		ddSlot.resize(nSlot,True);

		Vector<Int> ifr=msc.antenna1().getColumn();
		Int maxAnt=max(ifr);
		ifr*=1000;
		{
			Vector<Int> ant2=msc.antenna2().getColumn();
			maxAnt=max(maxAnt,max(ant2));
			ifr+=ant2;
		}
		maxAnt++;

		ifrIndex.resize(nRow);

		Matrix<Int> ifrSlot(maxAnt,maxAnt,-1);
		for (Int i=0; i<nIfr; i++) {
			Int j=ifrAxis_p(i);
			Int ant1=j/1000,ant2=j%1000;
			if (j>=0 && ant1<maxAnt && ant2<maxAnt) ifrSlot(ant1,ant2)=i;
		}

		for (Int i=0; i<nRow; i++) ifrIndex(i)=ifrSlot(ifr(i)/1000,ifr(i)%1000);

		rowIndex_p.resize(nIfr,nSlot); rowIndex_p.set(-1);
		for (Int i=0; i<nRow; i++) rowIndex_p(ifrIndex(i),slot(i))=i;

	} else {
		rowIndex_p.resize(0,0);
	}

	// now get out the data
	for (Int it=0; it<nItems; it++) {
		String item=downcase(items(it));
		MSS::Field fld=MSS::field(item);
		switch (fld) {
		case MSS::AMPLITUDE:
		case MSS::CORRECTED_AMPLITUDE:
		case MSS::MODEL_AMPLITUDE:
		case MSS::RATIO_AMPLITUDE:
		case MSS::RESIDUAL_AMPLITUDE:
		case MSS::OBS_RESIDUAL_AMPLITUDE:
			want(Amp,fld-MSS::AMPLITUDE)=True;
			break;
		case MSS::ANTENNA1:
			if (doIfrAxis) {
				Vector<Int> ant1(nIfr);
				ant1=ifrAxis_p;
				ant1/=1000;
				for (Int i=0; i<nIfr; i++) if (ifrAxis_p(i)<0) ant1(i)=-1;
				if (oneBased) ant1+=1;
				out.define(item,ant1);
			}
			else {
				Vector<Int> ant=msc.antenna1().getColumn();
				if (average) averageId(ant);
				if (oneBased) ant+=1;
				out.define(item,ant);
			}
			break;
		case MSS::ANTENNA2:
			if (doIfrAxis) {
				Vector<Int> ant2(nIfr);
				ant2=ifrAxis_p;
				ant2-=1000*(ifrAxis_p/1000);
				for (Int i=0; i<nIfr; i++) if (ifrAxis_p(i)<0) ant2(i)=-1;
				if (oneBased) ant2+=1;
				out.define(item,ant2);
			}
			else {
				Vector<Int> ant= msc.antenna2().getColumn();
				if (average) averageId(ant);
				if (oneBased) ant+=1;
				out.define(item,ant);
			}
			break;
		case MSS::AXIS_INFO:
		{
			Record axis_info(RecordInterface::Variable);
			// add info for the axes of the data array
			// 1. corr info (polarizations)
			axis_info.define("corr_axis",polSelection_p);
			// 2. freq info
			Record freq_axis(RecordInterface::Variable);
			freq_axis.define("chan_freq",chanFreq_p);
			freq_axis.define("resolution",bandwidth_p);
			axis_info.defineRecord("freq_axis",freq_axis);
			if (doIfrAxis) {
				// 3. ifr info
				Record ifr_axis(RecordInterface::Variable);
				if (oneBased) {
					Vector<Int> ifr;
					ifr=ifrAxis_p;
					ifr+=1001;
					for (Int i=0; i<nIfr; i++) if (ifrAxis_p(i)<0) ifr(i)=0;
					ifr_axis.define("ifr_number",ifr);
				} else {
					ifr_axis.define("ifr_number",ifrSelection_p);
				}
				Vector<String> antName=msc.antenna().name().getColumn();
				Vector<String> ifrName(nIfr,"");
				Vector<String> sName(nIfr,"");
				// get common prefix in antenna names by comparing first and
				// last antenna in table
				String prefix =
						common_prefix(antName(0),antName(antName.nelements()-1));
				Matrix<Double> antPos=msc.antenna().position().getColumn();
				Vector<Double> baseline(nIfr,-1.0);
				// PROBLEM: antName elements have string size extending beyond \0
				// string catenation doesn't work correctly!
				for (Int k=0; k<nIfr; k++) {
					if (ifrAxis_p(k)>=0) {
						Int ant1 = ifrAxis_p(k)/1000;
						Int ant2 = ifrAxis_p(k)%1000;
						ifrName(k)=antName(ant1).before('\0');
						ifrName(k)+="-";
						ifrName(k)+=antName(ant2).before('\0');
						if (prefix.length()>1) {
							sName(k)=String(antName(ant1).after(prefix)).before('\0');
							sName(k)+="-";
							sName(k)+=String(antName(ant2).after(prefix)).before('\0');
						} else {
							sName(k)=ifrName(k);
						}
						baseline(k)=sqrt(square(antPos(0,ant2)-antPos(0,ant1))+
								square(antPos(1,ant2)-antPos(1,ant1))+
								square(antPos(2,ant2)-antPos(2,ant1)));
					}
				}
				ifr_axis.define("ifr_name",ifrName);
				ifr_axis.define("ifr_shortname",sName);
				ifr_axis.define("baseline",baseline);
				axis_info.defineRecord("ifr_axis",ifr_axis);
				// 4. time info
				Record time_axis(RecordInterface::Variable);
				msd_p.setAntennas(msc.antenna());
				Vector<Double> time=msc.time().getColumn();
				Vector<Int> fieldId=msc.fieldId().getColumn();
				Int lastFieldId=-1;
				Double startOfDay=( nSlot>0 ?
						C::day*int(time(0)/C::day) : 0);
				Int nT = (average ? 1 : nSlot);
				Vector<Double> times(nT,0.0),ut(nT),ha(nT),last(nT);
				MEpoch ep=msc.timeMeas()(0);
				Bool doUT=False, doHA=False, doLAST=False;
				for (Int k=0; k<nItems; k++) {
					Int enumval=MSS::field(downcase(items(k)));
					if (enumval == MSS::UT) doUT=True;
					if (enumval == MSS::LAST) doLAST=True;
					if (enumval == MSS::HA) doHA=True;
				}
				// Note: HA conversion assumes there is a single fieldId for
				// each time slot.
				if (average) {
					times(0)=0;
					Int nTimes=0;
					for (Int k=0; k<nSlot; k++) {
						times(0)+=timeSlot(k);
					}
					if (nTimes>0) times(0)/=nTimes;
				} else {
					times=timeSlot;
				}
				for (Int k=0; k<nT; k++) {
					if (doUT) {
						ut(k)=times(k)-startOfDay;
					}
					if (doLAST || doHA) {
						Int curFieldId=fieldId(rowIndex_p(0,k));
						if (curFieldId!=lastFieldId) {
							if (msc.field().numPoly()(curFieldId)==0)
								msd_p.setFieldCenter(msc.field().phaseDirMeas(curFieldId));
							lastFieldId=curFieldId;
						}
						if (msc.field().numPoly()(curFieldId)>0)
							msd_p.setFieldCenter(msc.field().
									phaseDirMeas(curFieldId,times(k)));
						ep.set(MVEpoch(times(k)/C::day));
						msd_p.setEpoch(ep);
						if (doHA) ha(k)=msd_p.hourAngle()/C::_2pi*C::day;
						if (doLAST) last(k)=msd_p.last().getValue().get();
					}
				}
				time_axis.define("MJDseconds",times);
				if (doUT) time_axis.define("UT",ut);
				if (doHA) time_axis.define("HA",ha);
				if (doLAST) time_axis.define("LAST",last);
				axis_info.defineRecord("time_axis",time_axis);
			}
			out.defineRecord("axis_info",axis_info);
		}
		break;
		case MSS::DATA:
		case MSS::CORRECTED_DATA:
		case MSS::MODEL_DATA:
		case MSS::RATIO_DATA:
		case MSS::RESIDUAL_DATA:
		case MSS::OBS_RESIDUAL_DATA:
			want(Data,fld-MSS::DATA)=True;
			break;
		case MSS::DATA_DESC_ID:
			if (doIfrAxis) {
				Vector<Int> id(nSlot); id=ddSlot;
				if (average) averageId(id);
				if (oneBased) id+=1;
				out.define(item,id);
			} else {
				Vector<Int> col=msc.dataDescId().getColumn();
				if (average) averageId(col);
				if (oneBased) col+=1;
				out.define(item,col);
			}
			break;
		case MSS::FIELD_ID:
		case MSS::SCAN_NUMBER:
		{
			Vector<Int> col = (fld == MSS::FIELD_ID ?
					msc.fieldId().getColumn() :
					msc.scanNumber().getColumn());
			if (doIfrAxis) {
				Vector<Int> id(nSlot,-1);
				for (Int k=0; k<nSlot; k++) {
					Int i;
					for (i=0; i<nIfr && rowIndex_p(i,k)<0; i++) {}
					if (i<nIfr) id(k)=col(rowIndex_p(i,k));
				}
				if (average) averageId(id);
				if (oneBased) id+=1;
				out.define(item,id);
			} else {
				if (average) averageId(col);
				if (oneBased) col+=1;
				out.define(item,col);
			}
		}
		break;
		case MSS::FEED1:
		case MSS::FEED2:
		{
			Vector<Int> col = (fld == MSS::FEED1 ?
					msc.feed1().getColumn() :
					msc.feed2().getColumn());
			if (doIfrAxis) {
				Vector<Int> id(nSlot,-1);
				for (Int k=0; k<nSlot; k++) {
					Int i;
					for (i=0; i<nIfr && rowIndex_p(i,k)<0; i++) {}
					if (i<nIfr) id(k)=col(rowIndex_p(i,k));
				}
				if (average) averageId(id);
				if (oneBased) id+=1;
				out.define(item,id);
			} else {
				if (average) averageId(col);
				if (oneBased) col+=1;
				out.define(item,col);
			}
		}
		break;
		case MSS::FLAG:
			wantFlag=True;
			break;
		case MSS::FLAG_ROW:
			if (doIfrAxis) {
				Matrix<Bool> itFlag(nIfr,nSlot);
				itFlag.set(True); // flag unfilled slots
				Vector<Bool> flagRow=msc.flagRow().getColumn();
				for (Int k=0; k<nRow; k++) {
					itFlag(ifrIndex(k),slot(k))=flagRow(k);
				}
				out.define(item,itFlag);
			} else {
				out.define(item,msc.flagRow().getColumn());
			}
			break;
		case MSS::FLAG_SUM:
			wantFlagSum=True;
			break;
		case MSS::IFR_NUMBER:
		{
			if (doIfrAxis) {
				if (oneBased) {
					Vector<Int> ifr;
					ifr=ifrAxis_p;
					ifr+=1001;
					for (Int i=0; i<nIfr; i++) if (ifrAxis_p(i)<0) ifr(i)=0;
					out.define(item,ifr);
				} else {
					out.define(item,ifrAxis_p);
				}
			} else {
				Vector<Int> ant1=msc.antenna1().getColumn();
				Array<Int> ant2=msc.antenna2().getColumn();
				if (oneBased) {
					ant1+=1; ant2+=1;
				}
				ant1*=1000;
				ant1+=ant2;
				if (average) averageId(ant1);
				out.define(item,ant1);
			}
		}
		break;
		case MSS::IMAGINARY:
		case MSS::CORRECTED_IMAGINARY:
		case MSS::MODEL_IMAGINARY:
		case MSS::RATIO_IMAGINARY:
		case MSS::RESIDUAL_IMAGINARY:
		case MSS::OBS_RESIDUAL_IMAGINARY:
			want(Imag,fld-MSS::IMAGINARY)=True;
			break;
		case MSS::FLOAT_DATA:
			want(Data,ObsFloat)=True;
			break;
		case MSS::PHASE:
		case MSS::CORRECTED_PHASE:
		case MSS::MODEL_PHASE:
		case MSS::RATIO_PHASE:
		case MSS::RESIDUAL_PHASE:
		case MSS::OBS_RESIDUAL_PHASE:
			want(Phase,fld-MSS::PHASE)=True;
			break;
		case MSS::REAL:
		case MSS::CORRECTED_REAL:
		case MSS::MODEL_REAL:
		case MSS::RATIO_REAL:
		case MSS::RESIDUAL_REAL:
		case MSS::OBS_RESIDUAL_REAL:
			want(Real,fld-MSS::REAL)=True;
			break;
		case MSS::SIGMA:
		{
			Matrix<Float> sig = getWeight(msc.sigma(),True);
			Int nCorr=sig.shape()(0);
			if (doIfrAxis) {
				IPosition wtsidx(3,nCorr,nIfr,nSlot);
				Cube<Float> wts(wtsidx); wts.set(0);
				for (Int i=0; i<nRow; i++) {
					for (Int j=0; j<nCorr; j++) {
						wts(j, ifrIndex(i),slot(i))=sig(j,i);
					}
				}
				if (average) {
					// averaging sigma's doesn't make sense,
					// return sqrt of sum of squares instead
					IPosition sumwtidx(2,nCorr,nIfr);
					Matrix<Float> sumsig(sumwtidx); sumsig=0;
					for (Int i=0; i<nIfr; i++) {
						for (int j=0; j<nCorr; j++) {
							for (Int k=0; k<nSlot; k++) {sumsig(j,i)+=square(wts(j,i,k));}
							sumsig(j,i)=sqrt(sumsig(j,i));
						}
					}
					out.define("sigma",sumsig);
				} else {
					out.define("sigma",wts);
				}
			} else {
				if (average) {
					// return sqrt of sum of squares
					Vector<Float> sumsig(nCorr); sumsig=0.0;
					for (Int j=0; j<nCorr; j++) {
						for (Int i=0; i<nRow; i++) {sumsig(j)+=square(sig(j,i));}
						sumsig(j)=sqrt(sumsig(j));
					}
					out.define("sigma",sumsig);
				} else {
					out.define("sigma",sig);
				}
			}
		}
		break;
		case MSS::TIME:
		{
			if (doIfrAxis) {
				Vector<Double> times; times=timeSlot;
				if (average) averageDouble(times);
				out.define(item,times);
			} else {
				Vector<Double> time=msc.time().getColumn();
				if (average) averageDouble(time);
				out.define(item,time);
			}
		}
		break;
		case MSS::UVW:
			/* CAS-3211: the following if statement seems to be useless here.
			 * It prevents uvw from being filled when nelements()>0 and writes
			 * garbage to the uvw matrix.
			 */
			//      if (uvw.nelements()==0) uvw=msc.uvw().getColumn();
			uvw=msc.uvw().getColumn();
			if (doIfrAxis) {
				Cube<Double> uvw2(uvw.shape()(0),nIfr,nSlot); uvw2.set(0);
				for (Int k=0; k<nRow; k++) {
					Int ifr=ifrIndex(k);
					Int time=slot(k);
					uvw2(0,ifr,time)=uvw(0,k);
					uvw2(1,ifr,time)=uvw(1,k);
					uvw2(2,ifr,time)=uvw(2,k);
				}
				out.define(item,uvw2);
			} else {
				out.define(item,uvw);
			}
			break;
		case MSS::U:
		case MSS::V:
		case MSS::W:
		{
			Int index=fld-MSS::U;
//			if (uvw.nelements()==0) uvw=msc.uvw().getColumn();
			uvw=msc.uvw().getColumn();
			if (doIfrAxis) {
				Matrix<Double> uvw2(nIfr,nSlot); uvw2.set(0);
				for (Int k=0; k<nRow; k++) {
					uvw2(ifrIndex(k),slot(k))=uvw(index,k);
				}
				out.define(item,uvw2);
			} else {
				out.define(item,uvw.row(index));
			}
		}
		break;
		case MSS::UVDIST:
		{
//			if (uvw.nelements()==0) uvw=msc.uvw().getColumn();
			uvw=msc.uvw().getColumn();
			Vector<Double> u2(uvw.row(0)),v2(uvw.row(1));
			u2*=u2;
			v2*=v2;
			u2+=v2;
			// take square root - could use u2.apply(sqrt) but this can cause
			// link conflicts with fortran
			Bool deleteIt;
			Double* pu2 = u2.getStorage(deleteIt);
			for (Int i=0; i<nRows; i++) pu2[i]=sqrt(pu2[i]);
			if (doIfrAxis) {
				Matrix<Double> uvd(nIfr,nSlot); uvd.set(0);
				for (Int k=0; k<nRow; k++) uvd(ifrIndex(k),slot(k))=pu2[k];
				u2.putStorage(pu2,deleteIt);
				out.define(item,uvd);
			} else {
				u2.putStorage(pu2,deleteIt);
				out.define(item,u2);
			}
		}
		break;
		case MSS::WEIGHT:
			wantWeight=True;
			break;
		case MSS::HA:
		case MSS::LAST:
		case MSS::UT:
			// do nothing, only used within AXIS_INFO
			break;
		case MSS::UNDEFINED:
		default:
			os << LogIO::WARN << "Unrecognized field in input ignored: "<<
			item<<LogIO::POST;
			break;
		}
	}
	uvw.resize(0,0); // reclaim storage before we get the big arrays
	// save the flags and weights if we are averaging data over time/row
	Array<Bool> flags,dataflags;
	Array<Float> weights;

	if (wantWeight || average) {
		Matrix<Float> wt = getWeight(msc.weight());
		Int nCorr=wt.shape()(0);
		if (doIfrAxis) {
			IPosition wtsidx(3,nCorr,nIfr,nSlot);
			Cube<Float> wts(wtsidx); wts.set(0);
			for (Int i=0; i<nRow; i++) {
				for (Int j=0; j<nCorr; j++) {
					wts(j, ifrIndex(i),slot(i))=wt(j,i);
				}
			}
			if (wantWeight) {
				if (average) {
					// averaging weights doesn't make sense,
					// return sum of weights instead
					IPosition sumwtidx(2,nCorr,nIfr);
					Matrix<Float> sumwt(sumwtidx); sumwt=0;
					for (Int i=0; i<nIfr; i++) {
						for (int j=0; j<nCorr; j++) {
							for (Int k=0; k<nSlot; k++) {
								sumwt(j,i)+=wts(j,i,k);
							}
						}
					}
					out.define("weight",sumwt);
				} else {
					out.define("weight",wts);
				}
			}
			weights.reference(wts);
		} else {
			if (wantWeight) {
				if (average) {
					// return sum of weights
					Vector<Float> sumwt(nCorr); sumwt=0.0;
					for (Int i=0; i<nRow; i++) {
						for (Int j=0; j<nCorr; j++) {
							sumwt(j)+=wt(j,i);
						}
					}
					out.define("weight",sumwt);
				} else {
					out.define("weight",wt);
				}
			}
			weights.reference(wt);
		}
	}


	Array<Bool> flag;
	if (wantFlag || wantFlagSum ||
			average ||                                  // time averaging
			(chanSel_p.nelements()>0 && chanSel_p(2)>1) // channel averaging
	) {
		Array<Bool> avFlag;
		flag=getAveragedFlag(avFlag,msc.flag());
		uInt nPol=avFlag.shape()(0), nChan=avFlag.shape()(1), nRow=avFlag.shape()(2);
		if (doIfrAxis) {
			MSSelUtil2<Bool>::reorderData(avFlag,ifrIndex,nIfr,slot,nSlot,True);
		}
		if (average) flags=avFlag;
		if (wantFlag && !average) {
			out.define("flag",avFlag);
		}
		if (wantFlagSum) {
			if (doIfrAxis) {
				Cube<Int> flagSum(nPol,nChan,nIfr);
				flagSum=0;
				IPosition indx(4);
				indx(0)=0;
				for (uInt j=0; j<nPol; j++, indx(0)++) {
					indx(1)=0;
					for (uInt k=0; k<nChan; k++, indx(1)++) {
						indx(2)=0;
						for (Int l=0; l<nIfr; l++, indx(2)++) {
							Int count=0;
							for (indx(3)=0; indx(3)<nSlot; indx(3)++) {
								if (avFlag(indx)) count++;
							}
							flagSum(j,k,l)=count;
						}
					}
				}
				out.define("flag_sum",flagSum);
			} else {
				Matrix<Int> flagSum(nPol,nChan);
				flagSum=0;
				Cube<Bool> flag2(avFlag);
				for (uInt j=0; j<nPol; j++) {
					for (uInt k=0; k<nChan; k++) {
						Int count=0;
						for (uInt l=0; l<nRow; l++) {
							if (flag2(j,k,l)) count++;
						}
						flagSum(j,k)=count;
					}
				}
				out.define("flag_sum",flagSum);
			}
		}
	}

	if (want(Data,ObsFloat)) {
		// get the data
		Array<Float> fdata;
		if (!msc.floatData().isNull()) {
			getAveragedData(fdata,flag,msc.floatData());
			if (doIfrAxis) MSSelUtil2<Float>::
					reorderData(fdata,ifrIndex,nIfr,slot,nSlot,Float());
			if (average) MSSelUtil2<Float>::
					timeAverage(dataflags,fdata,flags,weights);
			out.define("float_data",fdata);
		} else {
			os << LogIO::WARN << "FLOAT_DATA column doesn't exist"<< LogIO::POST;
		}
	}


	Array<Complex> observed_data,corrected_data,model_data;
	Bool keepObs = anyEQ(want.column(ObsResidual),True);
	Bool keepMod = anyEQ(want.column(Residual),True)||
			anyEQ(want.column(Ratio),True)|| keepObs;;
	Bool keepCor = anyEQ(want.column(Residual),True)||
			anyEQ(want.column(Ratio),True);

	for (Int dataType=Observed; dataType<=ObsResidual; dataType++) {
		if (anyEQ(want.column(dataType),True)||
				(dataType==Observed && keepObs) ||
				(dataType==Model && keepMod) ||
				(dataType==Corrected && keepCor)) {
			if (convert_p && !subSet_p && dataType==Observed) {
				os << LogIO::WARN << "Polarization conversion of uncalibrated"
						<< " data may give incorrect results"<< LogIO::POST;
			}
			// get the data if this is a data column
			ROArrayColumn<Complex> colData;
			Array<Complex> data;
			if (dataType<=Model) {
				colData.reference( dataType == Observed ? msc.data() :
						(dataType == Corrected ? msc.correctedData() :
								msc.modelData()));
				if (colData.isNull()) {
					os << LogIO::WARN <<"Requested column doesn't exist"<<LogIO::POST;
				} else {
					getAveragedData(data,flag,colData);
					if (doIfrAxis) MSSelUtil2<Complex>::
							reorderData(data,ifrIndex,nIfr,slot,nSlot,Complex());
				}
			}
			String name;
			switch (dataType) {
			case Observed:
				if (keepObs) observed_data.reference(data);
				name="";
				break;
			case Model:
				if (keepMod) model_data.reference(data);
				name="model_";
				break;
			case Corrected:
				if (keepCor) corrected_data.reference(data);
				name="corrected_";
				break;
			case Ratio:
			{
				LogicalArray mask(model_data!=Complex(0.));
				data = corrected_data;
				data /= model_data(mask);
				data(!mask)=1.0;
				name="ratio_";
			}
			break;
			case Residual:
				data = corrected_data;
				data -= model_data;
				name="residual_";
				break;
			case ObsResidual:
				data = observed_data;
				data -= model_data;
				name="obs_residual_";
				break;
			default:;
			}
			if (average)
				MSSelUtil2<Complex>::timeAverage(dataflags,data,flags,weights);
			if (want(Amp,dataType)) out.define(name+"amplitude",amplitude(data));
			if (want(Phase,dataType)) out.define(name+"phase",phase(data));
			if (want(Real,dataType)) out.define(name+"real",real(data));
			if (want(Imag,dataType)) out.define(name+"imaginary",imag(data));
			if (want(Data,dataType)) out.define(name+"data",data);
		}
	}

	// only have averaged flags if some data item was requested as well
	if (average && wantFlag){
		out.define("flag",dataflags);
		if (dataflags.nelements()==0) {
			os << LogIO::WARN <<"Flags not calculated because no DATA derived "
					"item was specified" << LogIO::POST;
		}
	}
	return out;
}

Bool MSSelector::putData(const Record& items)
{
	LogIO os;
	if (!checkSelection()) return False;
	if (selms_p.nrow()==0) {
		os << LogIO::WARN << " Selected Table is empty - use selectinit"
				<< LogIO::POST;
		return False;
	}
	if (!selms_p.isWritable()) {
		os << LogIO::SEVERE << "MeasurementSet is not writable"<< LogIO::POST;
		return False;
	}

	MSColumns msc(selms_p);
	Int n=items.nfields();
	for (Int i=0; i<n; i++) {
		String item=downcase(items.name(i));
		MSS::Field fld=MSS::field(item);
		switch (fld) {
		case MSS::DATA:
		case MSS::CORRECTED_DATA:
		case MSS::MODEL_DATA:
		{
			ArrayColumn<Complex>& col = (fld==MSS::DATA ? msc.data() :
					(fld==MSS::CORRECTED_DATA ?
							msc.correctedData() :
							msc.modelData()));
			// averaging not supported
			if (chanSel_p(2)>1) {
				os << LogIO::SEVERE << "Averaging not supported when writing data"
						<< LogIO::POST;
				break;
			}
			if (convert_p) {
				os << LogIO::SEVERE <<"Polarization conversion not supported "
						<< "when writing data" << LogIO::POST;
				return False;
			}
			if (polIndex_p.nelements()>0) {
				os << LogIO::SEVERE << "Polarization selection must be 1,2 or "
						<< "all correlations,"<<endl<<
						"in correct order (ie MeasurementSet order), when writing data"
						<< LogIO::POST;
				return False;
			}
			Array<Complex> data = items.toArrayComplex(RecordFieldId(i));
			if (! col.isNull()) {
				if (data.ndim()==4) {
					if (data.shape()(2)==Int(rowIndex_p.nrow()) &&
							data.shape()(3)==Int(rowIndex_p.ncolumn())) {
						MSSelUtil2<Complex>::reorderData(data, rowIndex_p,
								selms_p.nrow());
					} else {
						os << LogIO::SEVERE<<"Data shape inconsistent with "
								"current selection"<< LogIO::POST;
						break;
					}
				}
				if (data.ndim()==3) {
					if (useSlicer_p) col.putColumn(slicer_p, data);
					else col.putColumn(data);
				}
			}
		}
		break;
		case MSS::FLOAT_DATA:
		{
			// averaging not supported
			if (chanSel_p(2)>1) {
				os << LogIO::SEVERE << "Averaging not supported when writing data"
						<< LogIO::POST;
				break;
			}
			if (convert_p) {
				os << LogIO::SEVERE <<"Polarization conversion not supported "
						<< "when writing data" << LogIO::POST;
				return False;
			}
			Array<Float> data = items.toArrayFloat(RecordFieldId(i));
			//if (GlishArray(items.get(i)).get(data)) {
			if (data.ndim()==4) {
				if (data.shape()(2)==Int(rowIndex_p.nrow()) &&
						data.shape()(3)==Int(rowIndex_p.ncolumn())) {
					MSSelUtil2<Float>::reorderData(data, rowIndex_p,
							selms_p.nrow());
				} else {
					os << LogIO::SEVERE<<"Data shape inconsistent with "
							"current selection"<< LogIO::POST;
					break;
				}
			}
			if (data.ndim()==3) {
				if (useSlicer_p) msc.floatData().putColumn(slicer_p, data);
				else msc.floatData().putColumn(data);
			}
			//}
		}
		break;
		case MSS::FLAG:
		{
			Array<Bool> flag = items.toArrayBool(RecordFieldId(i));
			// if (GlishArray(items.get(i)).get(flag)) {
			if (flag.ndim()==4) {
				if (flag.shape()(2)==Int(rowIndex_p.nrow()) &&
						flag.shape()(3)==Int(rowIndex_p.ncolumn())) {
					MSSelUtil2<Bool>::reorderData(flag, rowIndex_p, selms_p.nrow());
				} else {
					os << LogIO::SEVERE<<"Flag shape inconsistent with "
							"current selection"<< LogIO::POST;
					break;
				}
			}
			if (flag.ndim()==3) {
				putAveragedFlag(flag, msc.flag());
			}
			//}
		}
		break;
		case MSS::FLAG_ROW:
		{
			Array<Bool> flagRow = items.toArrayBool(RecordFieldId(i));
			// if (GlishArray(items.get(i)).get(flagRow)) {
			if (flagRow.ndim()==2) {
				reorderFlagRow(flagRow);
			}
			if (flagRow.ndim()==1) {
				msc.flagRow().putColumn(flagRow);
			}
			// }
		}
		break;
		case MSS::SIGMA:
		case MSS::WEIGHT:
		{
			Array<Float> weight = items.toArrayFloat(RecordFieldId(i));
			// if (GlishArray(items.get(i)).get(weight)) {
			if (weight.ndim()==3) {
				reorderWeight(weight);
			}
			if (weight.ndim()==2) {
				if (fld == MSS::SIGMA) msc.sigma().putColumn(weight);
				if (fld == MSS::WEIGHT) msc.weight().putColumn(weight);
			}
			// }
		}
		break;
		case MSS::UNDEFINED:
		default:
			os << LogIO::WARN << "Unrecognized field in input ignored: "<<
			item<<LogIO::POST;
			break;
		}
	}
	return True;
}

Bool MSSelector::iterInit(const Vector<String>& columns,
		Double interval, Int maxRows,
		Bool addDefaultSortColumns)
{
	LogIO os;
	if (!checkSelection()) return False;
	if (selms_p.nrow()==0) {
		os << LogIO::WARN << " Selected Table is empty - use selectinit"
				<< LogIO::POST;
		return False;
	}
	Int n=columns.nelements();
	Block<Int> col(n);
	for (Int i=0; i<n; i++) {
		col[i]=MS::columnType(columns(i));
		if (col[i]==MS::UNDEFINED_COLUMN) {
			os << LogIO::SEVERE << "Iteration initialization failed: unrecognized"
					" column name: "<<columns(i)<<LogIO::POST;
			return False;
		}
	}
	if (msIter_p) delete msIter_p;
	msIter_p=new MSIter(selms_p,col,interval,addDefaultSortColumns);
	maxRow_p = maxRows;
	return True;
}

Bool MSSelector::iterNext()
{
	Bool more=False;
	if (msIter_p) {
		Int nIterRow=msIter_p->table().nrow();
		if (startRow_p==0 || startRow_p> nIterRow) {
			(*msIter_p)++;
			more=msIter_p->more();
			if (more) nIterRow=msIter_p->table().nrow();
			startRow_p = 0;
		}
		if (startRow_p>0 || (more && maxRow_p>0 && nIterRow>maxRow_p)) {
			Int nRow=min(maxRow_p,nIterRow-startRow_p);
			selRows_p.resize(nRow);
			indgen(selRows_p,uInt(startRow_p),uInt(1));
			startRow_p+=maxRow_p;
			selms_p=msIter_p->table()(selRows_p);
			more=True;
		} else {
			if (more) selms_p=msIter_p->table();
			else selms_p=msIter_p->ms(); // put back the original selection at the end
		}
	}
	return more;
}

Bool MSSelector::iterOrigin()
{
	Bool ok=False;
	if (msIter_p) {
		startRow_p=0;
		msIter_p->origin();
		Int nIterRow=msIter_p->table().nrow();
		if (maxRow_p==0 || nIterRow<=maxRow_p) {
			selms_p=msIter_p->table();
		} else {
			selRows_p.resize(maxRow_p);
			indgen(selRows_p,uInt(0),uInt(1));
			selms_p=msIter_p->table()(selRows_p);
			startRow_p=maxRow_p;
		}
		ok=True;
	}
	return ok;
}

Bool MSSelector::iterEnd()
{
	if (!msIter_p) return False;
	selms_p=msIter_p->ms();
	return True;
}
void MSSelector::getAveragedData(Array<Complex>& avData, const Array<Bool>& flag,
		const ROArrayColumn<Complex>& col) const
{
	getAveragedData(avData,flag,col,Slicer(Slice()));
}


void MSSelector::getAveragedData(Array<Complex>& avData, const Array<Bool>& flag,
		const ROArrayColumn<Complex>& col,
		const Slicer & rowSlicer) const
{
	Array<Complex> data;
	if (useSlicer_p) {
		data=col.getColumnRange(rowSlicer,slicer_p);
	} else {
		data=col.getColumnRange(rowSlicer);
	}
	Int nPol=data.shape()(0);
	Vector<Int> chanSel(chanSel_p);
	if (chanSel.nelements()==0) {
		// not yet initialized, set to default
		chanSel.resize(4);
		chanSel(0)=data.shape()(1); chanSel(1)=0; chanSel(2)=1; chanSel(3)=1;
	}
	Int nChan=chanSel(0);
	Int nRow=data.shape()(2);
	avData.resize(IPosition(3,nPol,nChan,nRow));
	if (chanSel(2)==1) {
		// no averaging, just copy the data across
		avData=data;
	} else {
		// Average channel by channel
		Array<Bool> mask(!flag);
		Array<Float> wt(flag.shape(),0.0); wt(mask)=1.0;
		Array<Float> avWt(avData.shape(),0.0);
		for (Int i=0; i<nChan; i++) {
			// if width>1, the slice doesn't have an increment, so we take big steps
			Int chn=i*chanSel(3);
			IPosition is(3,0,i,0),ie(3,nPol-1,i,nRow-1),
					cs(3,0,chn,0),ce(3,nPol-1,chn,nRow-1);
			Array<Complex> ref(avData(is,ie)); ref=Complex(0.0);
			Array<Float> wtref(avWt(is,ie));
			// average over channels
			for (Int j=0; j<chanSel(2); j++,cs(1)++,ce(1)++) {
				MaskedArray<Complex> mdata(data(cs,ce),mask(cs,ce));
				ref+=mdata;
				wtref+=wt(cs,ce);
			}
			ref(wtref>Float(0.0))/=wtref(wtref>Float(0.0));
		}
	}
	// do the polarization conversion or selection
	if (convert_p) {
		Array<Complex> out;
		stokesConverter_p.convert(out,avData);
		avData.reference(out);
	} else if (polIndex_p.nelements()>0) {
		Int n=polIndex_p.nelements();
		Array<Complex> out(IPosition(3,n,nChan,nRow));
		IPosition sp(3,0,0,0),ep(3,0,nChan-1,nRow-1);
		IPosition sav(3,0,0,0),eav(3,0,nChan-1,nRow-1);
		for (Int i=0; i<n; i++,sp(0)++,ep(0)++) {
			sav(0)=polIndex_p(i);eav(0)=polIndex_p(i);
			out(sp,ep)=avData(sav,eav);
		}
		avData.reference(out);
	}
}

void MSSelector::getAveragedData(Array<Float>& avData, const Array<Bool>& flag,
		const ROArrayColumn<Float>& col) const
{
	getAveragedData(avData,flag,col,Slicer(Slice()));
}

void MSSelector::getAveragedData(Array<Float>& avData, const Array<Bool>& flag,
		const ROArrayColumn<Float>& col,
		const Slicer& rowSlicer) const
{
	Array<Float> data;
	if (useSlicer_p) {
		data=col.getColumnRange(rowSlicer,slicer_p);
	} else {
		data=col.getColumnRange(rowSlicer);
	}
	Int nPol=data.shape()(0);
	Vector<Int> chanSel(chanSel_p);
	if (chanSel.nelements()==0) {
		// not yet initialized, set to default
		chanSel.resize(4);
		chanSel(0)=data.shape()(1); chanSel(1)=0; chanSel(2)=1; chanSel(3)=1;
	}
	Int nChan=chanSel(0);
	Int nRow=data.shape()(2);
	avData.resize(IPosition(3,nPol,nChan,nRow));
	if (chanSel(2)==1) {
		// no averaging, just copy the data across
		avData=data;
	} else {
		// Average channel by channel
		Array<Bool> mask(!flag);
		Array<Float> wt(flag.shape(),0.0); wt(mask)=1.0;
		Array<Float> avWt(avData.shape(),0.0);
		for (Int i=0; i<nChan; i++) {
			// if width>1, the slice doesn't have an increment, so we take big steps
			Int chn=i*chanSel(3);
			IPosition is(3,0,i,0),ie(3,nPol-1,i,nRow-1),
					cs(3,0,chn,0),ce(3,nPol-1,chn,nRow-1);
			Array<Float> ref(avData(is,ie));
			Array<Float> wtref(avWt(is,ie));
			// average over channels
			for (Int j=0; j<chanSel(2); j++,cs(1)++,ce(1)++) {
				MaskedArray<Float> mdata(data(cs,ce),mask(cs,ce));
				ref+=mdata;
				wtref+=wt(cs,ce);
			}
			ref(wtref>Float(0.0))/=wtref(wtref>Float(0.0));
		}
	}
	// do the polarization conversion
	if (convert_p) {
		//    Array<Float> out;
		//  stokesConverter_p.convert(out,avData);
		//    avData.reference(out);
		LogIO os;
		os << LogIO::WARN << "Polarization conversion for FLOAT_DATA "
				"not implemented" << LogIO::POST;
	} else if (polIndex_p.nelements()>0) {
		Int n=polIndex_p.nelements();
		Array<Float> out(IPosition(3,n,nChan,nRow));
		IPosition sp(3,0,0,0),ep(3,0,nChan-1,nRow-1);
		IPosition sav(3,0,0,0),eav(3,0,nChan-1,nRow-1);
		for (Int i=0; i<n; i++,sp(0)++,ep(0)++) {
			sav(0)=polIndex_p(i);eav(0)=polIndex_p(i);
			out(sp,ep)=avData(sav,eav);
		}
		avData.reference(out);
	}
}

Array<Bool> MSSelector::getAveragedFlag(Array<Bool>& avFlag, 
		const ROArrayColumn<Bool>& col) const
{
	return getAveragedFlag(avFlag,col,Slicer(Slice()));
}

Array<Bool> MSSelector::getAveragedFlag(Array<Bool>& avFlag, 
		const ROArrayColumn<Bool>& col,
		const Slicer& rowSlicer) const
{
	Array<Bool> flag;
	if (useSlicer_p) {
		flag=col.getColumnRange(rowSlicer,slicer_p);
	} else {
		flag=col.getColumnRange(rowSlicer);
	}
	Int nPol=flag.shape()(0);
	Vector<Int> chanSel(chanSel_p);
	if (chanSel.nelements()==0) {
		// not yet initialized, set to default
		chanSel.resize(4);
		chanSel(0)=flag.shape()(1); chanSel(1)=0; chanSel(2)=1; chanSel(3)=1;
	}
	Int nChan=chanSel(0);
	Int nRow=flag.shape()(2);
	avFlag.resize(IPosition(3,nPol,nChan,nRow));
	if (chanSel(2)==1) {
		// no averaging, just copy flags
		avFlag=flag;
	} else {
		avFlag=True;
		for (Int i=0; i<nChan; i++) {
			Int chn=i*chanSel(3);
			IPosition is(3,0,i,0),ie(3,nPol-1,i,nRow-1),
					cs(3,0,chn,0),ce(3,nPol-1,chn,nRow-1);
			Array<Bool> ref(avFlag(is,ie));
			// average over channels
			for (Int j=0; j<chanSel(2); j++,cs(1)++,ce(1)++) {
				ref*=flag(cs,ce);
			}
		}
	}
	if (convert_p) {
		Array<Bool> out;
		stokesConverter_p.convert(out,avFlag);
		avFlag.reference(out);
	} else if (polIndex_p.nelements()>0) {
		Int n=polIndex_p.nelements();
		Array<Bool> out(IPosition(3,n,nChan,nRow));
		IPosition sp(3,0,0,0),ep(3,0,nChan-1,nRow-1);
		IPosition sav(3,0,0,0),eav(3,0,nChan-1,nRow-1);
		for (Int i=0; i<n; i++,sp(0)++,ep(0)++) {
			sav(0)=polIndex_p(i);eav(0)=polIndex_p(i);
			out(sp,ep)=avFlag(sav,eav);
		}
		avFlag.reference(out);
	}
	return flag; // return the raw flags for use in data averaging
}

void MSSelector::putAveragedFlag(const Array<Bool>& avFlag, 
		ArrayColumn<Bool>& col)
{
	Array<Bool> polFlag=avFlag;
	Array<Bool> out;
	Int n=polIndex_p.nelements();
	Int nRow=avFlag.shape()(2);
	// check if we need to read the data before writing it back
	if (convert_p || (n>2 && n<col.shape(0)(0))||
			(chanSel_p(2)>1 && chanSel_p(3)>chanSel_p(2))) {
		if (useSlicer_p) {
			out=col.getColumn(slicer_p);
		} else {
			out=col.getColumn();
		}
	}
	if (convert_p) {
		stokesConverter_p.invert(out,polFlag);
		polFlag.reference(out);
	}
	if (chanSel_p(2)>1) {
		// we need to undo the averaging and distribute the flags
		IPosition shape=polFlag.shape();
		shape(1)=(chanSel_p(0)-1)*chanSel_p(3)+chanSel_p(2);
		if (chanSel_p(3)<=chanSel_p(2)) {
			if (out.nelements()==0) out.resize(shape);
		}
		Int nChan=chanSel_p(0), st=chanSel_p(1), w=chanSel_p(2), inc=chanSel_p(3),
				nRow=shape(2);
		IPosition st1(3,0,st,0),st2(3,0,0,0),end1(3,shape(0)-1,st,nRow-1),
				end2(3,shape(0)-1,0,nRow-1);
		for (Int i=0; i<nChan; i++) {
			st2(1)=end2(1)=i;
			for (Int j=0; j<w; j++) {
				st1(1)=end1(1)=st+i*inc+j;
				if (n>0) {
					for (Int k=0; k<n; k++) {
						st2(0)=end2(0)=k;
						st1(0)=end1(0)=polIndex_p(k);
						out(st1,end1)=polFlag(st2,end2);
					}
				} else {
					out(st1,end1)=polFlag(st2,end2);
				}
			}
		}
	} else if (n>0) { // need to rearrange polarizations
		Int nChan=chanSel_p(0);
		if (out.nelements()==0) out.resize(IPosition(3,n,nChan,nRow));
		IPosition sp(3,0,0,0),ep(3,0,nChan-1,nRow-1);
		IPosition sav(3,0,0,0),eav(3,0,nChan-1,nRow-1);
		for (Int i=0; i<n; i++,sp(0)++,ep(0)++) {
			sav(0)=polIndex_p(i);eav(0)=polIndex_p(i);
			out(sav,eav)=polFlag(sp,ep);
		}
	} else {
		out.reference(polFlag);
	}
	if (useSlicer_p) col.putColumn(slicer_p, out);
	else col.putColumn(out);
}

Array<Float> MSSelector::getWeight(const ROArrayColumn<Float>& wtCol,
		Bool sigma) const
{
	Array<Float> wt;
	if (wantedOne_p>=0) {
		wt = wtCol.getColumn(Slicer(Slice(wantedOne_p,1)));
	} else {
		wt = wtCol.getColumn();
	}
	// apply the stokes conversion/selection to the weights
	if (convert_p) {
		Matrix<Float> outwt;
		stokesConverter_p.convert(outwt,wt,sigma);
		wt.reference(outwt);
	}
	return wt;
}

// reorder from 2d to 1d (removing ifr axis)
void MSSelector::reorderFlagRow(Array<Bool>& flagRow)
{
	Int nIfr=flagRow.shape()(0), nSlot=flagRow.shape()(1);
	Int nRow=selms_p.nrow();
	Bool deleteFlag, deleteRow;
	const Bool* pFlag=flagRow.getStorage(deleteFlag);
	const Int* pRow=rowIndex_p.getStorage(deleteRow);
	Vector<Bool> rowFlag(nRow);
	Int offset=0;
	for (Int i=0; i<nSlot; i++, offset+=nIfr) {
		for (Int j=0; j<nIfr; j++) {
			Int k=pRow[offset+j];
			if (k>0) {
				rowFlag(k)=pFlag[offset+j];
			}
		}
	}
	flagRow.freeStorage(pFlag,deleteFlag);
	rowIndex_p.freeStorage(pRow,deleteRow);
	flagRow.reference(rowFlag);
}

// reorder from 3d to 2d (removing ifr axis)
void MSSelector::reorderWeight(Array<Float>& weight)
{
	Int nCorr=weight.shape()(0), nIfr=weight.shape()(1), nSlot=weight.shape()(2);
	Int nRow=selms_p.nrow();
	Bool deleteWeight, deleteRow, deleteRowWeight;
	const Float* pWeight=weight.getStorage(deleteWeight);
	const Int* pRow=rowIndex_p.getStorage(deleteRow);
	Matrix<Float> rowWeight(nCorr, nRow);
	Float* pRowWeight=rowWeight.getStorage(deleteRowWeight);
	Int offset=0;
	for (Int i=0; i<nSlot; i++) {
		for (Int j=0; j<nIfr; j++, offset++) {
			Int k=pRow[offset];
			if (k>0) {
				Int wOffset = nCorr*offset;
				Int rwOffset = nCorr*k;
				for (Int c=0; c<nCorr; c++) {
					pRowWeight[rwOffset++] = pWeight[wOffset++];
				}
			}
		}
	}
	weight.freeStorage(pWeight,deleteWeight);
	rowIndex_p.freeStorage(pRow,deleteRow);
	rowWeight.putStorage(pRowWeight,deleteRowWeight);
	weight.reference(rowWeight);
}

Bool MSSelector::checkSelection() {
	if (!initSel_p) {
		LogIO os;
		os << LogIO::NORMAL <<"Initializing with default selection"
				<< LogIO::POST;
		initSelection();
	}
	return initSel_p;
}


} //# NAMESPACE CASACORE - END

