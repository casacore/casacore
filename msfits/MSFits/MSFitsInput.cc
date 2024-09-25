//# MSFitsInput:  uvfits (random group) to MeasurementSet filler
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify
//# it under the terms of the GNU General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or
//# (at your option) any later version.
//#
//# This program is distributed in the hope that it will be useful,
//# but WITHOUT ANY WARRANTY; without even the implied warranty of
//# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//# GNU General Public License for more details.
//# 
//# You should have received a copy of the GNU General Public License
//# along with this program; if not, write to the Free Software
//# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//

#include <casacore/msfits/MSFits/MSFitsInput.h>

#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/MatrixMath.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/fits/FITS/fitsio.h>
#include <casacore/fits/FITS/FITSReader.h>
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/ms/MeasurementSets/MSAntennaColumns.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/ms/MeasurementSets/MSDataDescColumns.h>
#include <casacore/ms/MeasurementSets/MSFeedColumns.h>
#include <casacore/ms/MeasurementSets/MSFieldColumns.h>
#include <casacore/ms/MeasurementSets/MSHistoryColumns.h>
#include <casacore/ms/MeasurementSets/MSObsColumns.h>
#include <casacore/ms/MeasurementSets/MSPolColumns.h>
#include <casacore/ms/MeasurementSets/MSSpWindowColumns.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MeasData.h>
#include <casacore/measures/Measures/Stokes.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/OS/HostInfo.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableUtil.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableInfo.h>
#include <casacore/tables/Tables/TableLock.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/DataMan/TiledColumnStMan.h>
#include <casacore/tables/DataMan/TiledShapeStMan.h>
#include <casacore/tables/DataMan/TiledDataStMan.h>
#include <casacore/tables/DataMan/TiledStManAccessor.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/casa/Utilities/Fallible.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/fits/FITS/FITSDateUtil.h>
#include <casacore/fits/FITS/FITSKeywordUtil.h>
#include <casacore/fits/FITS/FITSSpectralUtil.h>
#include <casacore/fits/FITS/BinTable.h>
#include <casacore/fits/FITS/fits.h>
#include <casacore/tables/LogTables/NewFile.h>
#include <casacore/casa/System/ProgressMeter.h>
#include <casacore/ms/MeasurementSets/MSTileLayout.h>
#include <casacore/ms/MSSel/MSSourceIndex.h>
#include <casacore/ms/MSOper/MSSummary.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>
#include <casacore/casa/OS/Directory.h>
#include <map>

using std::make_pair;

namespace casacore { //# NAMESPACE CASACORE - BEGIN

extern void showBinaryTable(BinaryTableExtension &x);

// Returns the 0-based position of the key string in the map,
// which is a list of strings.  Looks for the "Which" occurrance
// of the key.
static Int getIndex(Vector<String>& map, const String& key, uInt which = 0) {
    uInt count = 0;
    const uInt nMap = map.nelements();
    for (uInt i = 0; i < nMap; i++) {
        if (map(i) == key) {
            if (count == which) {
                return i;
            } else {
                count++;
            }
        }
    }
    return -1;
}

// Like getIndex, but only checks for containment, not exact identity
static Int getIndexContains(Vector<String>& map, const String& key, uInt which = 0) {
    uInt count = 0;
    const uInt nMap = map.nelements();
    for (uInt i = 0; i < nMap; i++) {
        if (map(i).contains(key)) {
            if (count == which) {
                return i;
            } else {
                count++;
            }
        }
    }
    return -1;
}

MSPrimaryGroupHolder::MSPrimaryGroupHolder() :
    hdu_p(0), ps(0), pl(0), pf(0) {
}

MSPrimaryGroupHolder::MSPrimaryGroupHolder(FitsInput& infile) :
    ps(0), pl(0), pf(0) {
    attach(infile);
}

void MSPrimaryGroupHolder::attach(FitsInput& infile) {
    detach();
    switch (infile.datatype()) {
    case FITS::SHORT:
        ps = new PrimaryGroup<Short> (infile);
        hdu_p = ps;
        break;
    case FITS::LONG:
        pl = new PrimaryGroup<FitsLong> (infile);
        hdu_p = pl;
        break;
    case FITS::FLOAT:
        pf = new PrimaryGroup<Float> (infile);
        hdu_p = pf;
        break;
    default:
        throw(AipsError("PrimaryGroupHolder(infile): unhandled FITS datatype"));
    }
}

MSPrimaryGroupHolder::~MSPrimaryGroupHolder() {
    detach();
}

void MSPrimaryGroupHolder::detach() {
    if (ps)
        delete ps;
    if (pl)
        delete pl;
    if (pf)
        delete pf;
    ps = 0;
    pl = 0;
    pf = 0;
}
//----------------------------
MSPrimaryTableHolder::MSPrimaryTableHolder() :
    hdu_p(0), ps(0), pl(0), pf(0), pb(0) {
}

MSPrimaryTableHolder::MSPrimaryTableHolder(FitsInput& infile) :
    ps(0), pl(0), pf(0), pb(0) {
    attach(infile);
}

void MSPrimaryTableHolder::attach(FitsInput& infile) {
    detach();
    switch (infile.datatype()) {
    case FITS::SHORT:
        ps = new PrimaryTable<Short> (infile);
        hdu_p = ps;
        break;
    case FITS::LONG:
        pl = new PrimaryTable<FitsLong> (infile);
        hdu_p = pl;
        break;
    case FITS::FLOAT:
        pf = new PrimaryTable<Float> (infile);
        hdu_p = pf;
        break;
    case FITS::BYTE:
        pb = new PrimaryTable<uChar> (infile);
        hdu_p = pb;
        break;
    default:
        throw(AipsError("PrimaryTableHolder(infile): unhandled FITS datatype"));
    }
}

MSPrimaryTableHolder::~MSPrimaryTableHolder() {
    detach();
}

void MSPrimaryTableHolder::detach() {
    if (ps)
        delete ps;
    if (pl)
        delete pl;
    if (pf)
        delete pf;
    if (pb)
        delete pb;
    ps = 0;
    pl = 0;
    pf = 0;
    pb = 0;
}

//------------------------------------------------------------
MSFitsInput::MSFitsInput(const String& msFile, const String& fitsFile,
        const Bool useNewStyle) :
    _infile(0), _msc(0), _uniqueAnts(), _nAntRow(0), _restfreq(0),
    _addSourceTable(False), _log(LogOrigin("MSFitsInput", "MSFitsInput")),
    _newNameStyle(useNewStyle), _msCreated(False) {
    // First, lets verify that fitsfile exists and that it appears to be a
    // FITS file.
    File f(fitsFile);
    if (! f.exists() || ! f.isReadable()) {
        _log << LogOrigin("MSFitsInput", "MSFitsInput")
               << "File " << fitsFile << " does not exist or is not readable"
                << LogIO::EXCEPTION;
    }
    // First attempt at validating that it's a FITS file
    if (!f.isRegular()) {
        _log << LogOrigin("MSFitsInput", "MSFitsInput")
               << "File " << fitsFile
                << " is not a plain file (maybe a directory?)"
                << LogIO::EXCEPTION;
    }
    // We should probably look for SIMPLE = here

    String errmsg;
    NewFile fileOK(True);
    if (!fileOK.valueOK(msFile, errmsg)) {
        _log << LogOrigin("MSFitsInput", "MSFitsInput")
               << "Error in output file: " << errmsg << LogIO::EXCEPTION;
    }

    _msFile = msFile;

    _log << LogOrigin("MSFitsInput", "MSFitsInput")
           << LogIO::NORMAL << "Converting FITS file '" << fitsFile
            << "' to MeasurementSet '" << msFile << "'" << LogIO::POST;

    // Open the FITS file for reading
    _infile = new FitsInput(fitsFile.chars(), FITS::Disk);
    _obsTime.resize(2);
    MVTime timeVal;
    MEpoch::Types epochRef;
    FITSDateUtil::fromFITS(timeVal, epochRef, "2000-01-01", "UTC");
    _obsTime(0) = timeVal.second();
    _obsTime(1) = timeVal.second();

    if (_infile) {
        if (_infile->err() == FitsIO::IOERR) {
            ThrowCc("Failed to read file " + fitsFile);
        }
        else if (_infile->err()) {
            _log << LogOrigin("MSFitsInput", "MSFitsInput")
                   << "Failed to read initial record -- exiting."
                    << LogIO::EXCEPTION;
        }
        else {

            if (_checkInput(*_infile)) {
                if (_infile->hdutype() == FITS::PrimaryGroupHDU) {
                    _priGroup.attach(*_infile);
                }
                if (_infile->hdutype() == FITS::PrimaryTableHDU) {
                    _priTable.attach(*_infile);
                }
            }
        }
    }
    else {
        _log << LogOrigin("MSFitsInput", "MSFitsInput")
               << "Failed to open fits file " << fitsFile << LogIO::EXCEPTION;
    }
}

void MSFitsInput::readRandomGroupUVFits(Int obsType) {
    _log << LogOrigin("MSFitsInput", __func__)
		        << LogIO::POST;
    Int nField = 0, nSpW = 0;
    _useAltrval = False;
    getPrimaryGroupAxisInfo();

    Bool useTSM = True;

    setupMeasurementSet(_msFile, useTSM, obsType);

    // fill the OBSERVATION table
    fillObsTables();

    Int totMem = HostInfo::memoryTotal();

    // 8 bytes per complex number and the other data like flag, weight is
    // is 1/2 of the total
    Int estMem = _priGroup.gcount() * max(1, _nIF) / 1024
        * _nPixel(getIndex(_coordType, "STOKES"))
        * _nPixel(getIndex(_coordType, "FREQ")) * 8 * 2;
    Int ns = max(1, _nIF);
    Int nc = _nPixel( getIndex(_coordType, "STOKES"));
    Int nf = _nPixel( getIndex(_coordType, "FREQ"));
    Long estStor = _priGroup.gcount() * ns / 1024
        * (7 * 8 + 11 * 4 + (2 * nc + 3 * nc * nf) * 4 + nc * nf + 1);
    Float needS = estStor / 1024. ;
    Directory curD(_msFile);
    Float freeS = curD.freeSpaceInMB();

    _log << LogOrigin("MSFitsInput", __func__)
        << ((needS > freeS) ? LogIO::WARN : LogIO::DEBUG1)
        << "Estimate of Needed Storage Space in MB: "
        << 0.9 * needS << "~" << 1.6 * needS
        << "\n                      Free Space in MB: " << freeS
        << LogIO::POST;

    // In reality it can be twice that number
    // We can remove the estMem limit of 1 Gbyte, below,
    // if we are fully in 64 bits world
    //

    // fill the main table
    if ((estMem < totMem) && (estMem < 1000000)) {
        //fill column wise and keep columns in memory
        try {
            fillMSMainTableColWise(nField, nSpW);
        }
        catch(const AipsError& ex) {
            _log << LogOrigin("MSFitsInput", __func__)
		        << ex.what()
		        << LogIO::EXCEPTION;
        }
    }
    else {
        //else fill row wise
        try {
            fillMSMainTable(nField, nSpW);
        }
        catch(const AipsError& ex) {
            _log << LogOrigin("MSFitsInput", __func__)
               << ex.what()
               << LogIO::EXCEPTION;
        }
    }
    // now handle the BinaryTable extensions for the subtables
    Bool haveAn = False, haveField = False, haveSpW = False, haveSysPower = False;
    while (_infile->rectype() != FITS::EndOfFile && !_infile->err()) {
        if (_infile->hdutype() != FITS::BinaryTableHDU) {
            _log << LogOrigin("MSFitsInput", __func__)
	            << LogIO::NORMAL << "Skipping unhandled extension"
	            << LogIO::POST;
            _infile->skip_hdu();
        }
        else {
            BinaryTable binTab(*_infile);
            // see if we can recognize the type
            String type = binTab.extname();
            _log << LogOrigin("MSFitsInput", __func__)
                << LogIO::DEBUG1 << "Found binary table of type "
                << type << " following data" << LogIO::POST;
            _log << LogOrigin("MSFitsInput", __func__)
                << LogIO::NORMAL
                << "extname=" << type << " nrows=" << binTab.nrows()
                << " ncols=" << binTab.ncols() << " rowsize=" << binTab.rowsize()
                << " pcount=" << binTab.pcount() << " gcount=" << binTab.gcount()
                << LogIO::POST;
            if (type.contains("AN") && !haveAn) {
                haveAn = True;
                fillAntennaTable(binTab);
            }
            else if (type.contains("FQ") && !haveSpW) {
                haveSpW = True;
                fillSpectralWindowTable(binTab, nSpW);
            }
            else if (type.contains("SU") && !haveField) {
                haveField = True;
                fillFieldTable(binTab, nField);
                setFreqFrameVar(binTab);
                //in case spectral window was already filled
                if (haveSpW) {
                    updateSpectralWindowTable();
                }
            }
            else if (type.contains("SY") && ! haveSysPower) {
                haveSysPower = True;
                _fillSysPowerTable(binTab);
            }
            else {
                _log << LogOrigin("MSFitsInput", __func__)
                    << LogIO::NORMAL
                    << "Skipping table, duplicate or unrecognized type: "
                    << type << LogIO::POST;
                binTab.fullTable();
            }
        }
    }
    if (!haveSpW) {
        // single freq. case
        fillSpectralWindowTable();
    }

    if (!haveField) {
        // single source case
        fillFieldTable(nField);
    }

    //this is uselessly slow thus replace it
    fillExtraTables();

    fixEpochReferences();

    if (!haveAn) {
        _log << LogOrigin("MSFitsInput", __func__)
            << "Cannot find an AN Table. This is required."
            << LogIO::EXCEPTION;
    }
    fillFeedTable();
}

void MSFitsInput::readPrimaryTableUVFits(Int obsType) {
    _log << LogOrigin("MSFitsInput", __func__)
           << "_msFile=" << _msFile
           << "obsType=" << obsType
           << LogIO::POST;
           
    _useAltrval = False;

    Bool useTSM = False;

    _epochRef = getDirectionFrame(2000.0);
    setupMeasurementSet(_msFile, useTSM, obsType);
    ConstFitsKeywordList kwlist = _priTable.kwlist();

    const FitsKeyword* kw;
    kwlist.first();

    //this date is not source observation time!
    String date = "2000-01-01";
    date = (kw = kwlist(FITS::DATE)) ? kw->asString() : date;

    MVTime timeVal;
    MEpoch::Types epochRef;
    FITSDateUtil::fromFITS(timeVal, epochRef, date, "UTC");
    _obsTime(0) = timeVal.second();
    _obsTime(1) = timeVal.second();
    fillHistoryTable(kwlist);
    Bool moreToDo = true;
    while (moreToDo &&
           _infile->rectype() != FITS::EndOfFile && !_infile->err()) {
        if (//_infile->rectype() != FITS::HDURecord ||
            _infile->hdutype() != FITS::BinaryTableHDU) {
            _log << LogOrigin("MSFitsInput", __func__)
                   << LogIO::NORMAL << "Skipping unhandled extension"
                   << LogIO::POST;
            _infile->skip_hdu();
        } 
        else {
            _log << LogOrigin("MSFitsInput", __func__)
                   << LogIO::DEBUG1 << "Binary Table HDU ------>>>" << LogIO::POST;

            BinaryTable* fqTab = 0;
            while (moreToDo &&
                   _infile->hdutype() == FITS::BinaryTableHDU) {
                _log << LogOrigin("MSFitsInput", __func__)
                       << LogIO::DEBUG1
                       << "Found binary table of type "
                       << _infile->rectype() << " following data"
                       << LogIO::POST;

                BinaryTable* bt = new BinaryTable(*_infile);
                String type = bt->extname();

                _log << LogOrigin("MSFitsInput", __func__)
                       << LogIO::NORMAL
                       << "extname=" << bt->extname() << " nrows=" << bt->nrows()
                       << " ncols=" << bt->ncols() << " rowsize=" << bt->rowsize() 
                       << LogIO::POST;

                if (type.contains("AN")) {
                    fillAntennaTable(*bt);
                } 
                else if (type.contains("FQ")) {
                    fqTab = &(*bt);
                }
                else if (type.contains("SU")) {
                    fillFieldTable(*bt);
                    setFreqFrameVar(*bt);
                    //in case spectral window was already filled
                    //if (haveSpW) {
                    //    updateSpectralWindowTable();
                    //}
                } 
                else if (type.contains("SY")) {
                    _fillSysPowerTable(*bt);
                }
                else if (type.contains("UV")) {
                    //showBinaryTable(*bt);
                    //fillOtherUVTables(*bt, *fqTab);
                    //TableRecord btKeywords = bt.getKeywords();

                    ConstFitsKeywordList kwl = bt->kwlist();

                    //FitsKeywordList pkw = kwl;
                    fillObservationTable(kwl);
                    fillHistoryTable(kwl);

                    getAxisInfo(kwl);
                    sortPolarizations();
                    fillPolarizationTable();
                    fillSpectralWindowTable(*fqTab);

                    try {
                        fillMSMainTable(*bt);
                    }
                    catch(const AipsError& ex) {
                        _log << LogOrigin("MSFitsInput", __func__)
                               << ex.what() 
                               << LogIO::EXCEPTION;
                    }
                    //fillPointingTable();
                    fillSourceTable();
                    fillFeedTable();

                    moreToDo = false;
                }
                else {
                    _infile->skip_hdu();
                    _log << LogOrigin("MSFitsInput", __func__)
                           << LogIO::NORMAL << "skip " << type << LogIO::POST;
                }
                _log << LogOrigin("MSFitsInput", __func__)
                       << LogIO::DEBUG1 << "<<<------ Binary Table HDU" << LogIO::POST;
            }

            //fill source table
        }
    }
}

void MSFitsInput::readFitsFile(Int obsType) {
    _log << LogOrigin("MSFitsInput", __func__)
           << LogIO::DEBUG2
           << "hdutype=" << _infile->hdutype()
           << LogIO::POST;
    try {
    	if (_infile->hdutype() == FITS::PrimaryGroupHDU) {
            readRandomGroupUVFits(obsType);
        }
    	else if (_infile->hdutype() == FITS::PrimaryTableHDU) {
            readPrimaryTableUVFits(obsType);
    	}
    	else {
    		ThrowCc("Unhandled extension type");
    	}
    }
    catch(const AipsError& ex) {
    	if (_msCreated) {
    		String name = _ms.tableName();
    		_log << LogIO::NORMAL << "Exception while processing UVFITS file. Deleting incomplete MS '"
    			<< name << "'" << LogIO::POST;
    		_ms.closeSubTables();
    		_ms.relinquishAutoLocks(True);
    		// detach to close
    		_ms = MeasurementSet();
    		TableUtil::deleteTable(name, True);
    	}
    	ThrowCc(ex.what());
    }
}

MSFitsInput::~MSFitsInput() {
    delete _infile;
    delete _msc;
}

Bool MSFitsInput::_checkInput(FitsInput& infile) {
    // Check that we have a valid UV fits file
    if (infile.rectype() != FITS::HDURecord) {
        _log << LogOrigin("MSFitsInput", __func__)
               << "file does not start with standard hdu record."
               << LogIO::EXCEPTION;
    }
    _log << LogOrigin("MSFitsInput", __func__)
           << LogIO::DEBUG1
           << "infile.hdutype(): " << infile.hdutype() 
           << LogIO::POST;
    
    //visibilty must be one of these type
    if (infile.hdutype() != FITS::PrimaryGroupHDU &&
        infile.hdutype() != FITS::PrimaryArrayHDU &&
         infile.hdutype() != FITS::PrimaryTableHDU) {
        _log << LogOrigin("MSFitsInput", __func__)
               << "Error, neither primary group nor primary table"
               << LogIO::EXCEPTION;
    }
    FITS::ValueType dataType = infile.datatype();
    if (dataType != FITS::FLOAT &&
         dataType != FITS::SHORT &&
         dataType != FITS::LONG &&
         dataType != FITS::BYTE) {
        _log << LogOrigin("MSFitsInput", __func__)
               << "Error, this class handles only FLOAT, SHORT, LONG and BYTE data "
               << "(BITPIX=-32,16,32,8) at present" << LogIO::EXCEPTION;
    }
    return True;
}

void MSFitsInput::getPrimaryGroupAxisInfo() {
    _log << LogOrigin("MSFitsInput", "getPrimaryGroupAxisInfo");
    // Extracts the axis related info. from the PrimaryGroup object and
    // returns them in the form of arrays.
    const Regex trailing(" *$"); // trailing blanks
    const Int nAxis = _priGroup.dims();
    if (nAxis < 1) {
        _log << "Data has no axes!" << LogIO::EXCEPTION;
    }
    _nPixel.resize(nAxis);
    _refVal.resize(nAxis);
    _refPix.resize(nAxis);
    _delta.resize(nAxis);
    _coordType.resize(nAxis);
    for (Int i = 0; i < nAxis; i++) {
        _nPixel(i) = _priGroup.dim(i);
        if (_nPixel(i) < 0) {
            _log << "Axes " << i << " cannot have a negative value"
                    << LogIO::EXCEPTION;
        }
        _coordType(i) = _priGroup.ctype(i);
        _coordType(i) = _coordType(i).before(trailing);
        _refVal(i) = static_cast<Double> (_priGroup.crval(i));
        _refPix(i) = static_cast<Double> (_priGroup.crpix(i));
        _delta(i) = static_cast<Double> (_priGroup.cdelt(i));
    }
    // Check if required axes are there
    if (getIndex(_coordType, "COMPLEX") < 0) {
        _log << "Data does not have a COMPLEX axis" << LogIO::EXCEPTION;
    }
    if (getIndex(_coordType, "STOKES") < 0) {
        _log << "Data does not have a STOKES axis" << LogIO::EXCEPTION;
    }
    if (getIndex(_coordType, "FREQ") < 0) {
        _log << "Data does not have a FREQ axis" << LogIO::EXCEPTION;
    }
    if ((getIndex(_coordType, "RA") < 0) && (getIndex(_coordType, "RA---SIN")
            < 0) && (getIndex(_coordType, "RA---NCP") < 0) && (getIndex(
            _coordType, "RA---SCP") < 0)) {
        _log << "Data does not have a RA axis" << LogIO::EXCEPTION;
    }
    if ((getIndex(_coordType, "DEC") < 0)
            && (getIndex(_coordType, "DEC--SIN") < 0) && (getIndex(
            _coordType, "DEC--NCP") < 0) && (getIndex(_coordType, "DEC--SCP")
            < 0)) {
        _log << "Data does not have a DEC axis" << LogIO::EXCEPTION;
    }

    // Sort out the order of the polarizations and find the sort indices
    // to put them in 'standard' order: PP,PQ,QP,QQ
    const uInt iPol = getIndex(_coordType, "STOKES");
    const uInt numCorr = _nPixel(iPol);
    _corrType.resize(numCorr);
    for (uInt i = 0; i < numCorr; i++) {
        // note: 1-based ref pix
        _corrType(i) = ifloor(_refVal(iPol) + (i + 1 - _refPix(iPol))
                * _delta(iPol) + 0.5);
        // convert AIPS-convention Stokes description to Casacore enum
        switch (_corrType(i)) {
        case -8:
            _corrType(i) = Stokes::YX;
            break;
        case -7:
            _corrType(i) = Stokes::XY;
            break;
        case -6:
            _corrType(i) = Stokes::YY;
            break;
        case -5:
            _corrType(i) = Stokes::XX;
            break;
        case -4:
            _corrType(i) = Stokes::LR;
            break;
        case -3:
            _corrType(i) = Stokes::RL;
            break;
        case -2:
            _corrType(i) = Stokes::LL;
            break;
        case -1:
            _corrType(i) = Stokes::RR;
            break;
        case 4:
            // _corrType(i) = Stokes::V;
            ThrowCc(
                "Stokes V cannot be decomposed into proper correlation types "
                "without making assumptions. This functionality is not supported"
            );
            break;
        case 3:
            //_corrType(i) = Stokes::U;
            ThrowCc(
                "Stokes U cannot be decomposed into proper correlation types "
                "without making assumptions. This functionality is not supported"
            );
            break;
        case 2:
            //_corrType(i) = Stokes::Q;
            ThrowCc(
                "Stokes Q cannot be decomposed into proper correlation types "
                "without making assumptions. This functionality is not supported"
            );
            break;
        case 1:
            //_corrType(i) = Stokes::I;
            ThrowCc(
                "Stokes I cannot be decomposed into proper correlation types "
                "without making assumptions. This functionality is not supported"
            ); 
            break;
        default:
            if (_corrType(i) < 0) {
                _log << "Unknown Correlation type: " << _corrType(i)
                        << LogIO::EXCEPTION;
            }
        }
    }
    Vector<Int> tmp(_corrType.copy());
    // Sort the polarizations to standard order. Could probably use
    // GenSortIndirect here.
    GenSort<Int>::sort(_corrType);
    _corrIndex.resize(numCorr);
    // Get the sort indices to rearrange the data to standard order
    for (uInt i = 0; i < numCorr; i++) {
        for (uInt j = 0; j < numCorr; j++) {
            if (_corrType(j) == tmp(i))
                _corrIndex[i] = j;
        }
    }

    // Figure out the correlation products from the polarizations
    _corrProduct.resize(2, numCorr);
    _corrProduct = 0;
    for (uInt i = 0; i < numCorr; i++) {
        const Stokes::StokesTypes cType = Stokes::type(_corrType(i));
        Fallible<Int> receptor = Stokes::receptor1(cType);
        Bool warn = False;
        if (receptor.isValid()) {
            _corrProduct(0, i) = receptor;
        } else if (!warn) {
            warn = True;
            _log << LogIO::WARN
                    << "Cannot deduce receptor 1 for correlations of type: "
                    << Stokes::name(cType) << LogIO::POST;
        }
        receptor = Stokes::receptor2(cType);
        if (receptor.isValid()) {
            _corrProduct(1, i) = receptor;
        } else if (!warn) {
            warn = True;
            _log << LogIO::WARN
                    << "Cannot deduce receptor 2 for correlations of type: "
                    << Stokes::name(cType) << LogIO::POST;
        }
    }

    // Save the object name, we may need it (for single source fits)
    const FitsKeyword* kwp;
    _object = (kwp = _priGroup.kw(FITS::OBJECT)) ? kwp->asString()
            : "unknown";
    _object = _object.before(trailing);
    // Save the array name
    _array = (kwp = _priGroup.kw(FITS::TELESCOP)) ? kwp->asString()
            : "unknown";
    _array = _array.before(trailing);
    // Save the RA/DEC epoch (for ss fits)
    if (_priGroup.kw(FITS::EPOCH))
        _epoch = (_priGroup.kw(FITS::EPOCH))->asFloat();
    else if (_priGroup.kw(FITS::EQUINOX))
        _epoch = (_priGroup.kw(FITS::EQUINOX))->asFloat();
    else {
        _epoch = 2000.0;
        _log << LogIO::WARN
                << "Cannot find epoch of data, defaulting to J2000"
                << LogIO::POST;

    }
    //epoch_p = (kwp=priGroup_p.kw(FITS::EPOCH)) ? kwp->asFloat() : 2000.0;
    _epochRef = getDirectionFrame(_epoch);

    // Get the spectral information
    _freqsys = MFrequency::TOPO;
    _restfreq = 0.0;
    Record header;
    Vector<String> ignore;
    Bool ok = FITSKeywordUtil::getKeywords(header, _priGroup.kwlist(), ignore);
    if (ok) {
        Int spectralAxis;
        Double referenceChannel, referenceFrequency, deltaFrequency;
        Vector<Double> frequencies;
        MDoppler::Types velPref;
        // Many of the following aren't used since they have been obtained
        // in other ways.
        ok = FITSSpectralUtil::fromFITSHeader(spectralAxis, referenceChannel,
                referenceFrequency, deltaFrequency, frequencies, _freqsys,
                velPref, _restfreq, _log, header);
        // Override freqsys_p from FITSSpectralUtil, if SPECSYS keyword present
        if (header.isDefined(String("specsys"))) {
            String fframe;
            header.get("specsys", fframe);
            MFrequency::getType(_freqsys, fframe);
        }

        // Be strict about use of ALTREF-derived frequencies:
        //  Only if frame not enforced with SPECSYS keyword,
        //  sufficient info is available to do the back-calculation,
        //  and if that back calculation takes us out of the TOPO
        //  Otherwise, we assume that the header and FQ frequencies are
        //  SPECSYS (or TOPO) and are correct.
        _useAltrval = (!header.isDefined(String("specsys")) && header.isDefined(
                String("altrval")) && header.isDefined(String("restfreq"))
                && _freqsys != MFrequency::TOPO);
        _refFreq = referenceFrequency;
        _chanFreq = frequencies;
    }

}

void MSFitsInput::setupMeasurementSet(const String& MSFileName, Bool useTSM,
        Int obsType) {

    // Make the MS table
    TableDesc td = MS::requiredTableDesc();

    // Even though we know the data is going to be the same shape throughout I'll
    // still create a column that has a variable shape as this will permit MS's
    // with other shapes to be appended.
    MS::addColumnToDesc(td, MS::DATA, 2);

    // add this optional column because random group fits has a
    // weight per visibility
    MS::addColumnToDesc(td, MS::WEIGHT_SPECTRUM, 2);

    if (useTSM) {
        td.defineHypercolumn("TiledData", 3, stringToVector(MS::columnName(
                MS::DATA)));
        td.defineHypercolumn("TiledFlag", 3, stringToVector(MS::columnName(
                MS::FLAG)));
        td.defineHypercolumn("TiledFlagCategory", 4, stringToVector(
                MS::columnName(MS::FLAG_CATEGORY)));
        td.defineHypercolumn("TiledWgtSpectrum", 3, stringToVector(
                MS::columnName(MS::WEIGHT_SPECTRUM)));
        td.defineHypercolumn("TiledUVW", 2, stringToVector(MS::columnName(
                MS::UVW)));
        td.defineHypercolumn("TiledWgt", 2, stringToVector(MS::columnName(
                MS::WEIGHT)));
        td.defineHypercolumn("TiledSigma", 2, stringToVector(MS::columnName(
                MS::SIGMA)));
    }
    SetupNewTable newtab(MSFileName, td, Table::New);
    // Set the default Storage Manager to be the Incr one
    IncrementalStMan incrStMan("ISMData");
    newtab.bindAll(incrStMan, True);

    // Bind ANTENNA1, ANTENNA2 and DATA_DESC_ID to the standardStMan
    // as they may change sufficiently frequently to make the
    // incremental storage manager inefficient for these columns.

    StandardStMan aipsStMan(32768);
    newtab.bindColumn(MS::columnName(MS::ANTENNA1), aipsStMan);
    newtab.bindColumn(MS::columnName(MS::ANTENNA2), aipsStMan);
    newtab.bindColumn(MS::columnName(MS::DATA_DESC_ID), aipsStMan);

    if (useTSM) {
        Int nCorr = _nPixel(getIndex(_coordType, "STOKES"));
        Int nChan = _nPixel(getIndex(_coordType, "FREQ"));
        _nIF = getIndex(_coordType, "IF");
        if (_nIF >= 0) {
            _nIF = _nPixel(_nIF);
        } else {
            _nIF = 1;
        }

        // Choose an appropriate tileshape
        IPosition dataShape(2, nCorr, nChan);
        IPosition tileShape = MSTileLayout::tileShape(dataShape, obsType,
                _array);
        _log << LogOrigin("MSFitsInput", __func__);
        _log << LogIO::NORMAL << "Using tile shape " << tileShape << " for "
                << _array << " with obstype=" << obsType << LogIO::POST;
        TiledShapeStMan tiledStMan1("TiledData", tileShape);
        TiledShapeStMan tiledStMan1f("TiledFlag", tileShape);
        TiledShapeStMan tiledStMan1fc("TiledFlagCategory", IPosition(4,
                tileShape(0), tileShape(1), 1, tileShape(2)));
        TiledShapeStMan tiledStMan2("TiledWgtSpectrum", tileShape);
        TiledColumnStMan tiledStMan3("TiledUVW", IPosition(2, 3, 1024));
        TiledShapeStMan tiledStMan4("TiledWgt", IPosition(2, tileShape(0),
                tileShape(2)));
        TiledShapeStMan tiledStMan5("TiledSigma", IPosition(2, tileShape(0),
                tileShape(2)));

        // Bind the DATA, FLAG & WEIGHT_SPECTRUM columns to the tiled stman
        newtab.bindColumn(MS::columnName(MS::DATA), tiledStMan1);
        newtab.bindColumn(MS::columnName(MS::FLAG), tiledStMan1f);
        newtab.bindColumn(MS::columnName(MS::FLAG_CATEGORY), tiledStMan1fc);
        newtab.bindColumn(MS::columnName(MS::WEIGHT_SPECTRUM), tiledStMan2);
        newtab.bindColumn(MS::columnName(MS::UVW), tiledStMan3);
        newtab.bindColumn(MS::columnName(MS::WEIGHT), tiledStMan4);
        newtab.bindColumn(MS::columnName(MS::SIGMA), tiledStMan5);
    } else {
        newtab.bindColumn(MS::columnName(MS::DATA), aipsStMan);
        newtab.bindColumn(MS::columnName(MS::FLAG), aipsStMan);
        newtab.bindColumn(MS::columnName(MS::WEIGHT_SPECTRUM), aipsStMan);
        newtab.bindColumn(MS::columnName(MS::UVW), aipsStMan);
    }
    // avoid lock overheads by locking the table permanently
    TableLock lock(TableLock::AutoLocking);
    MeasurementSet ms(newtab, lock);
    _msCreated = True;

    // Set up the subtables for the UVFITS MS
    // we make new tables with 0 rows
    Table::TableOption option = Table::New;
    ms.createDefaultSubtables(option);
    // add the optional Source sub table to allow for
    // specification of the rest frequency
    TableDesc sourceTD = MSSource::requiredTableDesc();
    MSSource::addColumnToDesc(sourceTD, MSSource::POSITION);
    MSSource::addColumnToDesc(sourceTD, MSSource::REST_FREQUENCY);
    MSSource::addColumnToDesc(sourceTD, MSSource::SYSVEL);
    MSSource::addColumnToDesc(sourceTD, MSSource::TRANSITION);
    MSSource::addColumnToDesc(sourceTD, MSSource::SOURCE_MODEL);
    SetupNewTable sourceSetup(ms.sourceTableName(), sourceTD, option);
    ms.rwKeywordSet().defineTable(MS::keywordName(MS::SOURCE), Table(
            sourceSetup, 0));
    // update the references to the subtable keywords
    ms.initRefs();

    { // Set the TableInfo
        TableInfo& info(ms.tableInfo());
        info.setType(TableInfo::type(TableInfo::MEASUREMENTSET));
        info.setSubType(String("UVFITS"));
        info.readmeAddLine(
                "This is a measurement set Table holding astronomical observations");
    }

    _ms = ms;
    _msc = new MSColumns(_ms);
    _msc->setDirectionRef(_epochRef); // Does the subtables.

    // UVW is the only Direction type Measures column in the main table.
    _msc->setUVWRef(Muvw::castType(_epochRef));
}

void MSFitsInput::fillObsTables() {
    const Regex trailing(" *$"); // trailing blanks
    const FitsKeyword* kwp;
    _ms.observation().addRow();
    String observer;
    observer = (kwp = _priGroup.kw(FITS::OBSERVER)) ? kwp->asString() : "";
    observer = observer.before(trailing);
    MSObservationColumns msObsCol(_ms.observation());
    msObsCol.observer().put(0, observer);
    String telescope = (kwp = _priGroup.kw(FITS::TELESCOP)) ? kwp->asString()
            : "unknown";
    telescope = telescope.before(trailing);
    if (telescope == "HATCREEK")
        telescope = "BIMA";
    msObsCol.telescopeName().put(0, telescope);
    msObsCol.scheduleType().put(0, "");
    msObsCol.project().put(0, "");

    String date;
    date = (kwp = _priGroup.kw(FITS::DATE_OBS)) ? kwp->asString() : "";
    if (date == "") {
        // try FITS::DATE instead
        //  (but this will find DATE-MAP which may not be correct...)
        date = (kwp = _priGroup.kw(FITS::DATE)) ? kwp->asString() : "";
    }
    if (date == "")
        date = "2000-01-01";
    MVTime timeVal;
    MEpoch::Types epochRef;
    FITSDateUtil::fromFITS(timeVal, epochRef, date, "UTC");
    Vector<Double> times(2);
    times(0) = timeVal.second();
    times(1) = timeVal.second(); // change this to last time in input
    _obsTime(0) = times(0);
    _obsTime(1) = times(1);

    msObsCol.timeRange().put(0, times);
    msObsCol.releaseDate().put(0, times(0)); // just use TIME_RANGE for now
    Double time = timeVal.second();
    msObsCol.flagRow().put(0, False);

    // Store all keywords from the first HISTORY keyword onwards in History table
    String history = (kwp = _priGroup.kw(FITS::HISTORY)) ? kwp->comm() : "";
    history = history.before(trailing);
    MSHistoryColumns msHisCol(_ms.history());
    Int row = -1;
    while (history != "") {
        _ms.history().addRow();
        row++;
        msHisCol.observationId().put(row, 0);
        msHisCol.time().put(row, time);
        msHisCol.priority().put(row, "NORMAL");
        msHisCol.origin().put(row, "MSFitsInput::fillObsTables");
        msHisCol.application().put(row, "ms");
        Vector<String> cliComm(1);
        cliComm[0] = "";
        msHisCol.cliCommand().put(row, cliComm);
        msHisCol.appParams().put(row, cliComm);
        msHisCol.message().put(row, history);
        history = (kwp = _priGroup.nextkw()) ? kwp->comm() : "";
        history = history.before(trailing);
    }
}

//
void MSFitsInput::fillHistoryTable(ConstFitsKeywordList &kwl) {
    kwl.first();
    const FitsKeyword *kw;

    const Regex trailing(" *$");

    String date;
    date = (kw = kwl(FITS::DATE_OBS)) ? kw->asString() : "";
    if (date == "") {
        date = (kw = kwl(FITS::DATE)) ? kw->asString() : "";
    }
    if (date == "")
        date = "2000-01-01";
    MVTime timeVal;
    MEpoch::Types epochRef;
    FITSDateUtil::fromFITS(timeVal, epochRef, date, "UTC");
    Double time = timeVal.second();

    String history;
    MSHistoryColumns msHisCol(_ms.history());
    Int row = _ms.history().nrow() - 1;
    kwl.first();
    while ((kw = kwl.next())) {
        String nm = kw->name();
        if (nm == "HISTORY" || nm == "COMMENT" || nm == "") {
            history = kw->comm();
            history = history.before(trailing);
            _ms.history().addRow();
            row++;
            msHisCol.observationId().put(row, 0);
            msHisCol.time().put(uInt(row), time);
            msHisCol.priority().put(row, "NORMAL");
            msHisCol.origin().put(row, "MSFitsInput::fillHistoryTables");
            msHisCol.application().put(row, history.before(' '));
            Vector<String> cliComm(1);
            cliComm[0] = "";
            msHisCol.cliCommand().put(row, cliComm);
            msHisCol.appParams().put(row, cliComm);
            msHisCol.message().put(row, history.after(' '));
        }
    }

}

// Extract the data from the PrimaryGroup object and stick it into
// the MeasurementSet 
// keep the arrays of data in memory before dumping them in columns
void MSFitsInput::fillMSMainTableColWise(Int& nField, Int& nSpW) {
    _log << LogOrigin("MSFitsInput", "fillMSMainTable");
    // Get access to the MS columns
    MSColumns& msc(*_msc);
    const Regex trailing(" *$"); // trailing blanks

    // get the random group parameter names
    Int nParams;
    Int nGroups;
    nParams = _priGroup.pcount();
    nGroups = _priGroup.gcount();
    Vector<String> pType(nParams);
    for (Int i = 0; i < nParams; i++) {
        pType(i) = _priGroup.ptype(i);
        pType(i) = pType(i).before(trailing);
    }
    Int totRows = nGroups * max(1, _nIF);

    Int nCorr = _nPixel(getIndex(_coordType, "STOKES"));
    Int nChan = _nPixel(getIndex(_coordType, "FREQ"));

    Cube<Complex> vis(nCorr, nChan, totRows);
    Matrix<Float> sigma(nCorr, totRows);
    Cube<Float> weightSpec(nCorr, nChan, totRows);
    Matrix<Float> weight(nCorr, totRows);
    const Int nCat = 3; // three initial categories
    // define the categories
    Vector<String> cat(nCat);
    cat(0) = "FLAG_CMD";
    cat(1) = "ORIGINAL";
    cat(2) = "USER";
    msc.flagCategory().rwKeywordSet().define("CATEGORY", cat);
    Cube<Bool> flagCat(nCorr, nChan, nCat, False);
    //  Matrix<Bool> flag = flagCat.xyPlane(0); // references flagCat's storage
    Cube<Bool> flag(nCorr, nChan, totRows);
    // find out the indices for U, V and W, there are several naming schemes
    Int iU, iV, iW;
    iU = getIndexContains(pType, "UU");
    iV = getIndexContains(pType, "VV");
    iW = getIndexContains(pType, "WW");
    if (iU < 0 || iV < 0 || iW < 0) {
        throw(AipsError("MSFitsInput: Cannot find UVW information"));
    }
    // get index for baseline
    Int iBsln = getIndex(pType, "BASELINE");
    // get indices for subarray, antenna1 and antenna2
    Int iSubarr = getIndex(pType, "SUBARRAY");
    Int iAnt1 = getIndex(pType, "ANTENNA1");
    Int iAnt2 = getIndex(pType, "ANTENNA2");
    // get indices for time
    Int iTime0 = getIndex(pType, "DATE", 0);
    Int iTime1 = getIndex(pType, "DATE", 1);
    // get index for source
    Int iSource = getIndex(pType, "SOURCE");
    if ( iSource == -1 ) {
        _log << LogOrigin("MSFitsInput", __func__)
             << LogIO::NORMAL
             << "SOURCE not found in";
        for ( auto p = pType.begin( ); p != pType.end( ); ++p )
            _log << " " << *p ;
        _log << LogIO::POST;
    }

    // get index for Freq
    Int iFreq = getIndex(pType, "FREQSEL");
    if ( iFreq == -1 ) {
        _log << LogOrigin("MSFitsInput", __func__)
             << LogIO::NORMAL
             << "FREQSEL not found in";
        for ( auto p = pType.begin( ); p != pType.end( ); ++p )
            _log << " " << *p ;
        _log << LogIO::POST;
    }

    // get index for Integration time
    Int iInttim = getIndex(pType, "INTTIM");

    _receptorAngle.resize(1);
    _log << LogIO::NORMAL << "Reading and writing " << nGroups
            << " visibility groups" << LogIO::POST;
    Int row = -1;

    Double interval, exposure;
    interval = 0.0;
    exposure = 0.0;
    Bool discernIntExp(True);
    Double discernedInt(DBL_MAX);

    // ProgressMeter meter(0.0, nGroups*1.0, "UVFITS Filler", "Groups copied", "",//               "", True,  nGroups/100);


    Matrix<Double> uvw(3, totRows);

    // Remember last-filled values for TSM use
    Int lastFillArrayId, lastFillFieldId, lastFillScanNumber;
    lastFillArrayId = -1;
    lastFillFieldId = -1, lastFillScanNumber = 0;
    Double lastFillTime = 0;

    // Keep track of array-specific scanNumbers, FieldIds and FreqIds
    Vector<Int> scanNumber(1);
    Vector<Int> lastFieldId(1), lastFreqId(1);
    scanNumber = 0;
    lastFieldId = -1, lastFreqId = -1;
    // nArray_p was uninitialized so could be HUGE ie causing std::bad_alloc
    // to be thrown as it is used below for dimensioning, using the construction
    //  nArray_p = max(nArray_p, arrayId+1). In order for this to work you'd better
    // initialize nArray_p first...
    _nArray = -1;

    Bool lastRowFlag = False;
    Vector<Int> ant1(totRows);
    Vector<Int> ant2(totRows);
    Vector<Double> interv(totRows);
    Vector<Double> expos(totRows);
    Vector<Int> datDescId(totRows);

    _ms.addRow(totRows);
    Int nif = max(1, _nIF);
    // Loop over groups
    for (Int group = 0; group < nGroups; group++) {

        // Read next group and
        _priGroup.read();

        // Extract time in MJD seconds
        const Double JDofMJD0 = 2400000.5;
        Double time = _priGroup.parm(iTime0);
        time -= JDofMJD0;
        if (iTime1 >= 0)
            time += _priGroup.parm(iTime1);
        time *= C::day;

        // Extract fqid
        Int freqId = iFreq > 0 ? Int(_priGroup.parm(iFreq)) : 1;

        // Extract field Id
        Int fieldId = 0;
        if (iSource >= 0) {
            // make 0-based
            fieldId = (Int) _priGroup.parm(iSource) - 1;
        }
        Int arrayId = 0;
        std::pair<Int, Int> ants;
        if (iBsln >= 0) {
            Float baseline = _priGroup.parm(iBsln);
            ants = _extractAntennas(baseline);
            arrayId = Int(100.0 * (baseline - Int(baseline) + 0.001));
        } else {
            Int antenna1 = _priGroup.parm(iAnt1);
            Int antenna2 = _priGroup.parm(iAnt2);
            ants = _extractAntennas(antenna1, antenna2);
            arrayId = _priGroup.parm(iSubarr);
        }
        _nArray = max(_nArray, arrayId + 1);
        for (Int k = 0; k < nif; ++k) {
            Int index = group * nif + k;
            // Extract uvw
            uvw(0, index) = _priGroup.parm(iU) * C::c;
            uvw(1, index) = _priGroup.parm(iV) * C::c;
            uvw(2, index) = _priGroup.parm(iW) * C::c;
            // Convert from units of seconds to meters
            ant1[index] = ants.first;
            ant2[index] = ants.second;
        }
        // Ensure arrayId-specific params are of correct length:
        if (scanNumber.shape() < _nArray) {
            scanNumber.resize(_nArray, True);
            lastFieldId.resize(_nArray, True);
            lastFreqId.resize(_nArray, True);
            scanNumber(_nArray - 1) = 0;
            lastFieldId(_nArray - 1) = -1;
            lastFreqId(_nArray - 1) = -1;
        }

        // Detect new scan (field or freqid change) for each arrayId
        if (fieldId != lastFieldId(arrayId) || freqId != lastFreqId(arrayId)
                || time - lastFillTime > 300.0) {
            scanNumber(arrayId)++;
            lastFieldId(arrayId) = fieldId;
            lastFreqId(arrayId) = freqId;
        }

        // If integration time is a RP, use it:
        if (iInttim > -1) {
            discernIntExp = False;
            exposure = _priGroup.parm(iInttim);
            interval = exposure;
        } else {
            // keep track of minimum which is the only one
            // (if time step is larger than UVFITS precision (and zero))
            discernIntExp = True;
            Double tempint;
            tempint = time - lastFillTime;
            if (tempint > 0.01) {
                discernedInt = min(discernedInt, tempint);
            }
        }

        // Work out which axis increments fastests, pol or channel
        // The COMPLEX axis is assumed to be first, and the IF axis is assumed
        // to be after STOKES and FREQ.
        Bool polFastest = (getIndex(_coordType, "STOKES") < getIndex(
                _coordType, "FREQ"));
        const Int nx = (polFastest ? nChan : nCorr);
        const Int ny = (polFastest ? nCorr : nChan);

        Int count = 0;
        for (Int ifno = 0; ifno < nif; ifno++) {
            // IFs go to separate rows in the MS
            row++;

            // fill in values for all the unused columns
            if (row == 0) {
                msc.feed1().put(row, 0);
                msc.feed2().put(row, 0);
                msc.flagRow().put(row, False);
                lastRowFlag = False;
                msc.processorId().put(row, -1);
                msc.observationId().put(row, 0);
                msc.stateId().put(row, -1);
            }

            // Fill scanNumber if changed since last row
            if (scanNumber(arrayId) != lastFillScanNumber) {
                msc.scanNumber().put(row, scanNumber(arrayId));
                lastFillScanNumber = scanNumber(arrayId);
            }

            weight.column(row).set(0.0);

            // Loop over chans and corrs:
            for (Int ix = 0; ix < nx; ix++) {
                for (Int iy = 0; iy < ny; iy++) {
                    const Float visReal = _priGroup(count++);
                    const Float visImag = _priGroup(count++);
                    const Float wt = _priGroup(count++);
                    const Int pol = (polFastest ? _corrIndex[iy]
                            : _corrIndex[ix]);
                    const Int chan = (polFastest ? ix : iy);
                    if (wt <= 0.0) {
                        weightSpec(pol, chan, row) = abs(wt);
                        flag(pol, chan, row) = True;
                        weight(pol, row) += abs(wt);
                    } else {
                        weightSpec(pol, chan, row) = wt;
                        flag(pol, chan, row) = False;
                        // weight column is sum of weight_spectrum (each pol):
                        weight(pol, row) += wt;
                    }
                    vis(pol, chan, row) = Complex(visReal, visImag);
                }
            }

            // calculate sigma (weight = inverse variance)
            for (Int nc = 0; nc < nCorr; nc++) {
                if (weight(nc, row) > 0.0) {
                    sigma(nc, row) = sqrt(1.0 / weight(nc, row));
                } else {
                    sigma(nc, row) = 0.0;
                }
            }

            if (!discernIntExp) {
                interv(row) = interval;
                expos(row) = exposure;
            }

            Bool rowFlag = allEQ(flag.xyPlane(row), True);
            if (rowFlag != lastRowFlag) {
                msc.flagRow().put(row, rowFlag);
                lastRowFlag = rowFlag;
            }

            if (arrayId != lastFillArrayId) {
                msc.arrayId().put(row, arrayId);
                lastFillArrayId = arrayId;
            }
            // Always put antenna1 & antenna2 since it is bound to the
            // aipsStMan and is assumed to change every row
            //     msc.antenna1().put(row,ant1);
            //     msc.antenna2().put(row,ant2);
            if (time != lastFillTime) {
                msc.time().put(row, time);
                msc.timeCentroid().put(row, time);
                lastFillTime = time;
            }

            // determine the spectralWindowId
            Int spW = ifno;
            if (iFreq >= 0) {
                spW = (Int) _priGroup.parm(iFreq) - 1; // make 0-based
                if (_nIF > 0) {
                    spW *= _nIF;
                    spW += ifno;
                }
            }
            nSpW = max(nSpW, spW + 1);

            // Always put DDI (SSM) since it might change rapidly
            //   msc.dataDescId().put(row,spW);
            datDescId[row] = spW;
            // store the fieldId
            if (fieldId != lastFillFieldId) {
                msc.fieldId().put(row, fieldId);
                nField = max(nField, fieldId + 1);
                lastFillFieldId = fieldId;
            }
        }
    }
    // If determining interval on-the-fly, fill interval/exposure columns
    //  now:
    if (discernIntExp) {
        discernedInt = floor(100.0 * discernedInt + 0.5) / 100.0;
        msc.interval().fillColumn(discernedInt);
        msc.exposure().fillColumn(discernedInt);
    } else {
        msc.interval().putColumn(interv);
        msc.exposure().putColumn(expos);
    }
    msc.uvw().putColumn(uvw);
    msc.antenna1().putColumn(ant1);
    msc.antenna2().putColumn(ant2);
    msc.dataDescId().putColumn(datDescId);
    msc.data().putColumn(vis);
    msc.weight().putColumn(weight);
    msc.sigma().putColumn(sigma);
    msc.weightSpectrum().putColumn(weightSpec);
    msc.flag().putColumn(flag);
    // fill the receptorAngle with defaults, just in case there is no AN table
    _receptorAngle = 0;
}

// Extract the data from the PrimaryGroup object and stick it into
// the MeasurementSet 
// Doing it row by row
void MSFitsInput::fillMSMainTable(Int& nField, Int& nSpW) {
    _log << LogOrigin("MSFitsInput", "fillMSMainTable");
    // Get access to the MS columns
    MSColumns& msc(*_msc);
    const Regex trailing(" *$"); // trailing blanks

    // get the random group parameter names
    Int nParams;
    Int nGroups;
    nParams = _priGroup.pcount();
    nGroups = _priGroup.gcount();
    Vector<String> pType(nParams);
    for (Int i = 0; i < nParams; i++) {
        pType(i) = _priGroup.ptype(i);
        pType(i) = pType(i).before(trailing);
    }

    Int nCorr = _nPixel(getIndex(_coordType, "STOKES"));
    Int nChan = _nPixel(getIndex(_coordType, "FREQ"));


    Matrix<Complex> vis(nCorr, nChan);
    Vector<Float> sigma(nCorr);
    Matrix<Float> weightSpec(nCorr, nChan);
    Vector<Float> weight(nCorr);
    const Int nCat = 3; // three initial categories
    // define the categories
    Vector<String> cat(nCat);
    cat(0) = "FLAG_CMD";
    cat(1) = "ORIGINAL";
    cat(2) = "USER";
    msc.flagCategory().rwKeywordSet().define("CATEGORY", cat);
    Cube<Bool> flagCat(nCorr, nChan, nCat, False);
    Matrix<Bool> flag = flagCat.xyPlane(0); // references flagCat's storage

    // find out the indices for U, V and W, there are several naming schemes
    Int iU, iV, iW;
    iU = getIndexContains(pType, "UU");
    iV = getIndexContains(pType, "VV");
    iW = getIndexContains(pType, "WW");
    if (iU < 0 || iV < 0 || iW < 0) {
        throw(AipsError("MSFitsInput: Cannot find UVW information"));
    }
    // get index for baseline
    Int iBsln = getIndex(pType, "BASELINE");
    // get indices for subarray, antenna1 and antenna2
    Int iSubarr = getIndex(pType, "SUBARRAY");
    Int iAnt1 = getIndex(pType, "ANTENNA1");
    Int iAnt2 = getIndex(pType, "ANTENNA2");
    // get indices for time
    Int iTime0 = getIndex(pType, "DATE", 0);
    Int iTime1 = getIndex(pType, "DATE", 1);
    // get index for source
    Int iSource = getIndex(pType, "SOURCE");
    if ( iSource == -1 ) {
        _log << LogOrigin("MSFitsInput", __func__)
             << LogIO::NORMAL
             << "SOURCE not found in";
        for ( auto p = pType.begin( ); p != pType.end( ); ++p )
            _log << " " << *p ;
        _log << LogIO::POST;
    }
    // get index for Freq
    Int iFreq = getIndex(pType, "FREQSEL");
    if ( iFreq == -1 ) {
        _log << LogOrigin("MSFitsInput", __func__)
             << LogIO::NORMAL
             << "FREQSEL not found in";
        for ( auto p = pType.begin( ); p != pType.end( ); ++p )
            _log << " " << *p ;
        _log << LogIO::POST;
    }

    // get index for Integration time
    Int iInttim = getIndex(pType, "INTTIM");

    _receptorAngle.resize(1);
    _log << LogIO::NORMAL << "Reading and writing " << nGroups
            << " visibility groups" << LogIO::POST;
    Int row = -1;

    Double interval, exposure;
    interval = 0.0;
    exposure = 0.0;
    Bool discernIntExp(True);
    Double discernedInt(DBL_MAX);

    ProgressMeter meter(0.0, nGroups * 1.0, "UVFITS Filler", "Groups copied",
            "", "", True, nGroups / 100);

    Vector<Double> uvw(3);

    // Remember last-filled values for TSM use
    Int lastFillArrayId, lastFillFieldId, lastFillScanNumber;
    lastFillArrayId = -1;
    lastFillFieldId = -1, lastFillScanNumber = 0;
    Double lastFillTime = 0;

    // Keep track of array-specific scanNumbers, FieldIds and FreqIds
    Vector<Int> scanNumber(1);
    Vector<Int> lastFieldId(1), lastFreqId(1);
    scanNumber = 0;
    lastFieldId = -1, lastFreqId = -1;
    // nArray_p was uninitialized so could be HUGE ie causing std::bad_alloc
    // to be thrown as it is used below for dimensioning, using the construction
    //  nArray_p = max(nArray_p, arrayId+1). In order for this to work you'd better
    // initialize nArray_p first...
    _nArray = -1;

    Bool lastRowFlag = False;

    // Loop over groups
    for (Int group = 0; group < nGroups; group++) {

        // Read next group and
        _priGroup.read();

        // Extract time in MJD seconds
        //  (this has VERY limited precision [~0.01s])
        const Double JDofMJD0 = 2400000.5;
        Double time = _priGroup.parm(iTime0);
        time -= JDofMJD0;
        if (iTime1 >= 0)
            time += _priGroup.parm(iTime1);
        time *= C::day;

        // Extract fqid
        Int freqId = iFreq > 0 ? Int(_priGroup.parm(iFreq)) : 1;

        // Extract field Id
        Int fieldId = 0;
        if (iSource >= 0) {
            // make 0-based
            fieldId = (Int) _priGroup.parm(iSource) - 1;
        }

        // Extract uvw
        uvw(0) = _priGroup.parm(iU);
        uvw(1) = _priGroup.parm(iV);
        uvw(2) = _priGroup.parm(iW);
        // Convert from units of seconds to meters
        uvw *= C::c;

        // Extract array/baseline/antenna info
        Int arrayId = 0;
        std::pair<Int, Int> ants;
        if (iBsln >= 0) {
            Float baseline = _priGroup.parm(iBsln);
            ants = _extractAntennas(baseline);
            arrayId = Int(100.0 * (baseline - Int(baseline) + 0.001));
        } else {
            Int antenna1 = _priGroup.parm(iAnt1);
            Int antenna2 = _priGroup.parm(iAnt2);
            ants = _extractAntennas(antenna1, antenna2);
            arrayId = _priGroup.parm(iSubarr);
        }
        _nArray = max(_nArray, arrayId + 1);
        Int ant1 = ants.first;
        Int ant2 = ants.second;
        // Ensure arrayId-specific params are of correct length:
        if (scanNumber.shape() < _nArray) {
            scanNumber.resize(_nArray, True);
            lastFieldId.resize(_nArray, True);
            lastFreqId.resize(_nArray, True);
            scanNumber(_nArray - 1) = 0;
            lastFieldId(_nArray - 1) = -1;
            lastFreqId(_nArray - 1) = -1;
        }

        // Detect new scan (field or freqid change) for each arrayId
        if (fieldId != lastFieldId(arrayId) || freqId != lastFreqId(arrayId)
                || time - lastFillTime > 300.0) {
            scanNumber(arrayId)++;
            lastFieldId(arrayId) = fieldId;
            lastFreqId(arrayId) = freqId;
        }

        // If integration time is a RP, use it:
        if (iInttim > -1) {
            discernIntExp = False;
            exposure = _priGroup.parm(iInttim);
            interval = exposure;
        } else {
            // keep track of minimum which is the only one
            // (if time step is larger than UVFITS precision (and zero))
            discernIntExp = True;
            Double tempint;
            tempint = time - lastFillTime;
            if (tempint > 0.01) {
                discernedInt = min(discernedInt, tempint);
            }
        }

        // Work out which axis increments fastests, pol or channel
        // The COMPLEX axis is assumed to be first, and the IF axis is assumed
        // to be after STOKES and FREQ.
        Bool polFastest = (getIndex(_coordType, "STOKES") < getIndex(
                _coordType, "FREQ"));
        const Int nx = (polFastest ? nChan : nCorr);
        const Int ny = (polFastest ? nCorr : nChan);

        Int count = 0;
        for (Int ifno = 0; ifno < max(1, _nIF); ifno++) {
            // IFs go to separate rows in the MS
            _ms.addRow();
            row++;

            // fill in values for all the unused columns
            if (row == 0) {
                msc.feed1().put(row, 0);
                msc.feed2().put(row, 0);
                msc.flagRow().put(row, False);
                lastRowFlag = False;
                msc.processorId().put(row, -1);
                msc.observationId().put(row, 0);
                msc.stateId().put(row, -1);
            }

            // Fill scanNumber if changed since last row
            if (scanNumber(arrayId) != lastFillScanNumber) {
                msc.scanNumber().put(row, scanNumber(arrayId));
                lastFillScanNumber = scanNumber(arrayId);
            }

            weight = 0.0;
            // Loop over chans and corrs:
            for (Int ix = 0; ix < nx; ix++) {
                for (Int iy = 0; iy < ny; iy++) {
                    const Float visReal = _priGroup(count++);
                    const Float visImag = _priGroup(count++);
                    const Float wt = _priGroup(count++);
                    const Int pol = (polFastest ? _corrIndex[iy]
                            : _corrIndex[ix]);
                    const Int chan = (polFastest ? ix : iy);
                    if (wt <= 0.0) {
                        weightSpec(pol, chan) = abs(wt);
                        flag(pol, chan) = True;
                        weight(pol) += abs(wt);
                    } else {
                        weightSpec(pol, chan) = wt;
                        flag(pol, chan) = False;
                        // weight column is sum of weight_spectrum (each pol):
                        weight(pol) += wt;
                    }
                    vis(pol, chan) = Complex(visReal, visImag);
                }
            }

            // calculate sigma (weight = inverse variance)
            for (Int nc = 0; nc < nCorr; nc++) {
                if (weight(nc) > 0.0) {
                    sigma(nc) = sqrt(1.0 / weight(nc));
                } else {
                    sigma(nc) = 0.0;
                }
            }

            // If available, store interval/exposure
            if (!discernIntExp) {
                msc.interval().put(row, interval);
                msc.exposure().put(row, exposure);
            }

            msc.data().put(row, vis);

            msc.weight().put(row, weight);
            msc.sigma().put(row, sigma);
            msc.weightSpectrum().put(row, weightSpec);

            msc.flag().put(row, flag);
            msc.flagCategory().put(row, flagCat);
            Bool rowFlag = allEQ(flag, True);
            if (rowFlag != lastRowFlag) {
                msc.flagRow().put(row, rowFlag);
                lastRowFlag = rowFlag;
            }

            if (arrayId != lastFillArrayId) {
                msc.arrayId().put(row, arrayId);
                lastFillArrayId = arrayId;
            }
            // Always put antenna1 & antenna2 since it is bound to the
            // aipsStMan and is assumed to change every row
            msc.antenna1().put(row, ant1);
            msc.antenna2().put(row, ant2);
            if (time != lastFillTime) {
                msc.time().put(row, time);
                msc.timeCentroid().put(row, time);
                lastFillTime = time;
            }
            msc.uvw().put(row, uvw);

            // determine the spectralWindowId
            Int spW = ifno;
            if (iFreq >= 0) {
                spW = (Int) _priGroup.parm(iFreq) - 1; // make 0-based
                if (_nIF > 0) {
                    spW *= _nIF;
                    spW += ifno;
                }
            }
            nSpW = max(nSpW, spW + 1);

            // Always put DDI (SSM) since it might change rapidly
            msc.dataDescId().put(row, spW);

            // store the fieldId
            if (fieldId != lastFillFieldId) {
                msc.fieldId().put(row, fieldId);
                nField = max(nField, fieldId + 1);
                lastFillFieldId = fieldId;
            }
        }
        meter.update((group + 1) * 1.0);
    }
    // If determining interval on-the-fly, fill interval/exposure columns
    //  now:
    if (discernIntExp) {
        discernedInt = floor(100.0 * discernedInt + 0.5) / 100.0;
        msc.interval().fillColumn(discernedInt);
        msc.exposure().fillColumn(discernedInt);
    }

    // fill the receptorAngle with defaults, just in case there is no AN table
    _receptorAngle = 0;
}

void MSFitsInput::_fillSysPowerTable(BinaryTable& bt) {
    static const Regex trailing(" *$"); // trailing blanks
    const TableRecord btKeywords = bt.getKeywords();
    const auto nIF = btKeywords.asInt("NO_IF");
    ThrowIf(nIF > 1, "Currently SYSPOWER tables with only a single IF may be imported");
    ThrowIf(nIF == 0, "Number of IFs in SY table cannot be 0");
    const auto nPol = btKeywords.asInt("NO_POL");
    Table syTab = bt.fullTable();
    //syTab.tableDesc().show();
    const static String name = "SYSPOWER";
    const String casaTableName = _ms.tableName() + "/" + name;
    {
        // table creation code copied from casa ASDM2MSFiller.cc
        TableDesc tableDesc;
        // Key columns.
        tableDesc.comment() = "System calibration from Cal diode demodulation (EVLA).";
        tableDesc.addColumn(ScalarColumnDesc<Int>("ANTENNA_ID", "Antenna identifier."));
        tableDesc.addColumn(ScalarColumnDesc<Int>("FEED_ID", "Feed's index."));
        tableDesc.addColumn(ScalarColumnDesc<Int>("SPECTRAL_WINDOW_ID", "Spectral window identifier."));
        tableDesc.addColumn(ScalarColumnDesc<Double>("TIME", "Midpoint of time measurement."));
        tableDesc.addColumn(ScalarColumnDesc<Float>("INTERVAL", "Interval of measurement."));
        // Data columns.
        tableDesc.addColumn(ArrayColumnDesc<Float>("SWITCHED_DIFF", "Switched power difference (cal on - off)."));
        tableDesc.addColumn(ArrayColumnDesc<Float>("SWITCHED_SUM", "Switched power sum (cal on + off)."));
        tableDesc.addColumn(ArrayColumnDesc<Float>("REQUANTIZER_GAIN", "Requantizer gain."));
        SetupNewTable tableSetup(casaTableName, tableDesc, Table::New);
        _ms.rwKeywordSet().defineTable(name, Table(tableSetup));
        _ms.rwKeywordSet().asTable(name).flush();
    }
    Table casaTable(casaTableName, Table::Update);
    casaTable.addRow(syTab.nrow() * nIF);
    ScalarColumn<Double> timeCol(syTab, "TIME");
    ScalarColumn<Float> timeIntCol(syTab, "TIME INTERVAL");
    ScalarColumn<Int> antNoCol(syTab, "ANTENNA NO.");
    ScalarColumn<Int> freqIDCol(syTab, "FREQ ID");
    if (nIF == 1) {
        ScalarColumn<Float> powerDif1Col(syTab, "POWER DIF1");
        ScalarColumn<Float> powerSum1Col(syTab, "POWER SUM1");
        ScalarColumn<Float> postGain1Col(syTab, "POST GAIN1");
        ScalarColumn<Float> powerDif2Col;
        ScalarColumn<Float> powerSum2Col;
        ScalarColumn<Float> postGain2Col;
        if (nPol == 2) {
            powerDif2Col = ScalarColumn<Float>(syTab, "POWER DIF2");
            powerSum2Col = ScalarColumn<Float>(syTab, "POWER SUM2");
            postGain2Col = ScalarColumn<Float>(syTab, "POST GAIN2");
        }
        _doFillSysPowerSingleIF(
            casaTableName, timeCol, timeIntCol,
            antNoCol, freqIDCol, powerDif1Col,
            powerSum1Col, postGain1Col, powerDif2Col,
            powerSum2Col, postGain2Col 
        );
    }
    else {
        ArrayColumn<Float> powerDif1Col(syTab, "POWER DIF1");
        ArrayColumn<Float> powerSum1Col(syTab, "POWER SUM1");
        ArrayColumn<Float> postGain1Col(syTab, "POST GAIN1");
    }
}

void MSFitsInput::_doFillSysPowerSingleIF(
    const String& casaTableName, const ScalarColumn<Double>& timeCol,
    const ScalarColumn<Float>& intervalCol,
    const ScalarColumn<Int>& antNoCol, const ScalarColumn<Int>& freqIDCol,
    const ScalarColumn<Float>& powerDif1Col,
    const ScalarColumn<Float>& powerSum1Col,
    const ScalarColumn<Float>& postGain1Col,
    const ScalarColumn<Float>& powerDif2Col,
    const ScalarColumn<Float>& powerSum2Col,
    const ScalarColumn<Float>& postGain2Col
) {
    Table casaTable(casaTableName, Table::Update);
    const auto nrow = timeCol.nrow();
    const auto npol = powerDif2Col.nrow() == 0 ? 1 : 2;
    {
        ScalarColumn<Int> sysPowerAnt(casaTable, "ANTENNA_ID");
        const auto antVals = antNoCol.getColumn() - 1;
        sysPowerAnt.putColumn(antVals);
    }
    {
        ScalarColumn<Int> sysPowerFeed(casaTable, "FEED_ID");
        const Vector<Int> feedVals(nrow, 0);
        sysPowerFeed.putColumn(feedVals);
    }
    {
        ScalarColumn<Int> sysPowerSpw(casaTable, "SPECTRAL_WINDOW_ID");
        const auto spwVals = freqIDCol.getColumn() - 1;
        sysPowerSpw.putColumn(spwVals);
    }
    {
        ScalarColumn<Double> sysPowerTime(casaTable, "TIME");
        const auto timeVals = timeCol.getColumn() * C::day;
        sysPowerTime.putColumn(timeVals);
    }
    {
        ScalarColumn<Float> sysPowerInterval(casaTable, "INTERVAL");
        const auto intervalVals = intervalCol.getColumn() * (float)C::day;
        sysPowerInterval.putColumn(intervalVals);
    }
    {
        ArrayColumn<Float> sysPowerDiff(casaTable, "SWITCHED_DIFF");
        Array<Float> diffs(IPosition(2, npol, nrow));
        for (uInt i=0; i<nrow; ++i) {
            diffs(IPosition(2, 0, i)) = powerDif1Col(i);
            if (npol == 2) {
                diffs(IPosition(2, 1, i)) = powerDif2Col(i);
            }
        }
        sysPowerDiff.putColumn(diffs);
    }
    {
        ArrayColumn<Float> sysPowerSum(casaTable, "SWITCHED_SUM");
        Array<Float> sums(IPosition(2, npol, nrow));
        for (uInt i=0; i<nrow; ++i) {
            sums(IPosition(2, 0, i)) = powerSum1Col(i);
            if (npol == 2) {
                sums(IPosition(2, 1, i)) = powerSum2Col(i);
            }
        }
        sysPowerSum.putColumn(sums);
    }
    {
        ArrayColumn<Float> sysPowerGain(casaTable, "REQUANTIZER_GAIN");
        Array<Float> gains(IPosition(2, npol, nrow));
        for (uInt i=0; i<nrow; ++i) {
            gains(IPosition(2, 0, i)) = postGain1Col(i);
            if (npol == 2) {
                gains(IPosition(2, 1, i)) = postGain2Col(i);
            }
        }
        sysPowerGain.putColumn(gains);
    }
}  

void MSFitsInput::fillAntennaTable(BinaryTable& bt) {
    static const Regex trailing(" *$"); // trailing blanks
    TableRecord btKeywords = bt.getKeywords();
    Int nAnt = bt.nrows();
    Table anTab = bt.fullTable();
    ScalarColumn<Int> id(anTab, "NOSTA");
    Vector<Int> ids = id.getColumn();
    std::set<Int> sids(ids.begin(), ids.end());
    _nAntRow = *std::max_element(sids.begin(), sids.end());
    if (_uniqueAnts.empty()) {
        ThrowIf(
            _nAntRow < nAnt,
            "Logic Error: Please submit a defect report and include where we can find your dataset"
        );
        if (_nAntRow > nAnt) {
            _log << LogOrigin("MSFitsInput", __func__)
                << LogIO::WARN << _array
                << " there is at least one gap in the antenna "
                << "sequence found in the FITS AN table. Empty "
                << "rows will be inserted into the ANTENNA table "
                << "representing the gap(s)." << LogIO::POST;
        }
    }
    else {
        Int nAntVis = _uniqueAnts.size();
        ThrowIf(
            nAntVis > nAnt,
            "The number of antennas in the visibilities exceeds "
            "the number of rows in the AN table. Cannot proceed"
        );
        Int maxAntVis = *std::max_element(_uniqueAnts.begin(), _uniqueAnts.end());
        ThrowIf(
            maxAntVis > _nAntRow,
            "This data set has (1-based) antenna number " + String::toString(maxAntVis)
            + " in the visibility data, but there is no corresponding "
            "antenna ID in the FITS AN table. Cannot proceed."
        );
        std::set<Int>::const_iterator iter = _uniqueAnts.begin();
        std::set<Int>::const_iterator end = _uniqueAnts.end();
        for (; iter!=end; ++iter) {
            ThrowIf(
                std::find(sids.begin(), sids.end(), *iter) == sids.end(),
                "(1-based) antenna " + String::toString(*iter)
                + " exists in the visibility data, but there is no "
                " record which references it in the FITS AN table. "
                "Cannot proceed"
            );
        }
    }
    std::set<Int>::const_iterator iter = sids.begin();
    std::set<Int>::const_iterator end = sids.end();
    Int i = 1;
    for (; iter!=end; ++iter, ++i) {
        if (*iter != i) {
            _log << LogOrigin("MSFitsInput", __func__)
                << LogIO::WARN << _array
                << " there is at least one gap in the antenna "
                << "sequence found in the FITS AN table. Empty "
                << "rows will be inserted into the ANTENNA table "
                << "representing the gaps." << LogIO::POST;
            break;
        }
    }
    _receptorAngle.resize(2 * _nAntRow);
    _receptorAngle = 0.0;
    Vector<Double> arrayXYZ(3);
    arrayXYZ = 0.0;
    if (
        !btKeywords.isDefined("ARRAYX") || !btKeywords.isDefined("ARRAYY")
        || !btKeywords.isDefined("ARRAYZ")
    ) {
        throw(AipsError("MSFitsInput: Illegal AN file: no antenna positions"));
    }
    arrayXYZ(0) = bt.getKeywords().asdouble("ARRAYX");
    arrayXYZ(1) = bt.getKeywords().asdouble("ARRAYY");
    arrayXYZ(2) = bt.getKeywords().asdouble("ARRAYZ");

    static const String xyzHand = "XYZHAND";
    String handed;
    if (bt.getKeywords().isDefined(xyzHand)) {
        handed = bt.getKeywords().asString(xyzHand);
    }
    else {
        _log << LogOrigin("MSFitsInput", __func__) << LogIO::WARN
            << xyzHand + " keyword not found in AN table. Will assume "
            << "antenna coordinate system is right handed." << LogIO::POST;
    }
    Bool leftHanded = handed == "LEFT";
    if (leftHanded) {
        _log << LogOrigin("MSFitsInput", __func__)
            << LogIO::NORMAL << "Antenna positions in the uvfits "
            << "AN table are in a left handed coordinate system "
            << "and so will undergo a y -> -y transformation when "
            << "written to the MS" << LogIO::POST;
    }
    // Since we cannot write these quantities, we cannot rely upon
    // their presence in any UVFITS file that we read:
    Double rdate = 0.0;
    String srdate;
    if (btKeywords.isDefined("RDATE")) {
        srdate = btKeywords.asString("RDATE");
    }
    Double gst = 0.0;
    if (btKeywords.isDefined("GSTIA0")) {
        gst = btKeywords.asdouble("GSTIA0") * C::degree;
    }
    Double degpdy = 0.0;
    if (btKeywords.isDefined("DEGPDY")) {
        degpdy = btKeywords.asdouble("DEGPDY");
    }
    String timsys = "TAI";
    if (btKeywords.isDefined("TIMSYS")) {
        timsys = btKeywords.asString("TIMSYS");
        timsys = timsys.before(trailing);
    }
    MVTime timeVal;
    MEpoch::Types epochRef;
    FITSDateUtil::fromFITS(timeVal, epochRef, srdate, timsys);
    // convert to canonical form
    timsys = MEpoch::showType(epochRef);
    rdate = timeVal.second(); // MJD seconds
    // store the time keywords
    _ms.antenna().rwKeywordSet().define(String("RDATE"), rdate);
    _ms.antenna().rwKeywordSet().define(String("GSTIA0"), gst);
    _ms.antenna().rwKeywordSet().define(String("DEGPDY"), degpdy);
    _ms.antenna().rwKeywordSet().define(String("TIMSYS"), timsys);
    //save value to set time reference frame elsewhere
    _timsys = timsys;
    // Fill in some likely values
    Float diameter = 25;
    Bool doSMA = (_array == "SMA");
    if (_array == "ATCA") {
        diameter = 22;
    }
    else if (doSMA) {
        diameter = 6;
    }
    else if (_array == "ATA") {
        diameter = 6.1;
    }
    else if (_array == "HATCREEK" || _array == "BIMA") {
        diameter = 6.1;
    }
    else if (_array == "GMRT") {
        diameter = 45.0;
    }
    else if (_array == "IRAM_PDB" || _array == "IRAM PDB") {
        diameter = 15.0;
    }
    MSAntennaColumns& ant(_msc->antenna());
    ScalarColumn<String> name(anTab, "ANNAME");
    ScalarColumn<Int> mountType(anTab, "MNTSTA");
    ScalarColumn<Float> offset(anTab, "STAXOF");
    ScalarColumn<Float> polangleA(anTab, "POLAA");
    ScalarColumn<Float> polangleB(anTab, "POLAB");
    ArrayColumn<Double> antXYZ(anTab, "STABXYZ");
    Vector<Float> antDiams(nAnt);
    antDiams.set(diameter);

    //If it has a column called DIAMETER ...make use of it if
    // any of the values are valid
    Bool positiveDiamsFound = False;
    if (anTab.tableDesc().isColumn("DIAMETER")) {
        Vector<Float> tmpDiams = ScalarColumn<Float>(anTab, "DIAMETER").getColumn();
        if (anyGT(tmpDiams, 0.0f)) {
            antDiams = tmpDiams;
            positiveDiamsFound = True;
        }
    }
    if (! positiveDiamsFound) {
        if (_array == "OVRO" || _array == "CARMA") {
            for (Int i = 0; i < nAnt; ++i) {
                //Crystal Brogan has guaranteed that it is always this order
                antDiams[i] = id(i) <= 6 ? 10.4 : 6.1;
            }
        }
        else if (_array == "ALMA") {
            // CAS-8875, algorithm from Jen Meyer
            for (Int i = 0; i < nAnt; ++i) {
                const String& myName = name(i);
                if (myName.startsWith("CM")) {
                    antDiams[i] = 7.0;
                }
                else if (
                    myName.startsWith("DA") || myName.startsWith("DV")
                    || myName.startsWith("PM")
                ) {
                    antDiams[i] = 12.0;
                }
                else {
                    antDiams[i] = diameter;
                }
            }
        }
    }
    // Prepare handling of UVFITS Antenna position coord conventions:
    // VLA requires rotation of local coords in some cases
    Bool rotate = False;
    Matrix<Double> posRot = Rot3D(0, 0.0);
    String arrnam = "Unknown";
    if (btKeywords.isDefined("ARRNAM")) {
        arrnam = btKeywords.asString("ARRNAM");
        arrnam.trim();
    }
    // For old VLA archive files, antenna positions are stored in a non-standard
    // frame and so must be rotated. This is necessary for CASA VLA users, see
    // eg CAS-11726
    // The file is an old VLA archive file if either the FRAME keyword is not
    // present, or if the array position is approximately the VLA location 
    // (a smaller magnitude position vector indicates a third part db should
    // be used for the array position, such as the Observatories table).
    // Usually if it is not the VLA position it will be 0, but we allow
    // some slop here by only considering position vectors with magnitudes
    // in excess of 1000km of indicating an old VLA archive file
    if (
        arrnam == "VLA" 
        && (
            ! btKeywords.isDefined("FRAME")
            || norm(arrayXYZ) > 1e6
        )
    ) {
        _log << LogOrigin("MSFitsInput", __FUNCTION__) << LogIO::NORMAL
            << "This looks like an old VLA archive UVFITS file"
            << LogIO::POST;
        // Array position for VLA from aips may be wrong, so use
        //  authoritative position from measures (station positions
        //  are from on-line system and are relative to this)
        MPosition vlaCenter;
        AlwaysAssert(MeasTable::Observatory(vlaCenter, "VLA"), AipsError);
        const auto diff = abs(arrayXYZ - vlaCenter.getValue().getValue());
        const auto diff2 = sqrt(sum(diff*diff));
        _log << LogIO::NORMAL << "UVFITS file telescope position is " << diff2
            << " meters from CASA Observatories table VLA position" << LogIO::POST;
        // give a pretty large tolerance (10km) for uvftis files VLA position
        // to differ from CASA Observatories VLA position
        if (diff2 <= 10000.0) {
            // if the difference between the magnitudes of the array positions
            // is less than or equal to 10km, we can be certain the file
            // is an old VLA archive file.
            arrayXYZ = vlaCenter.getValue().getValue();
            // Form rotation around Z axis by VLA longitude=atan(arrayY/arrayX)
            Double vlaLong = atan2(arrayXYZ(1), arrayXYZ(0));
            posRot = Rot3D(2, vlaLong); // Applied to each ant position below
            rotate = True;
            _log << LogIO::NORMAL << "Performing transformation of antenna "
                << "positions from coordinate frame used by MODCOMPs to ITRF"
                << LogIO::POST;
        }
        else {
            _log << LogIO::WARN << "Array position from UVFITS file is not "
                << "near that of the position from the Observatories table. "
                << "No rotation of antenna positions will be performed."
                << LogIO::POST;
        }
    }
    // add antenna info to table
    ant.setPositionRef(MPosition::ITRF);
    _ms.antenna().addRow(_nAntRow);
    for (Int i = 0; i < _nAntRow; ++i) {
        // This loop initially flags all rows.
        // The good rows will be unflagged in the next loop.
        // Bad rows (representing gaps in the antenna IDs) will remain flagged.
        ant.flagRow().put(i, True);
    }
    for (Int i = 0; i < nAnt; ++i) {
        Int row = id(i) - 1;
        ant.dishDiameter().put(row, antDiams(i));
        String mount;
        switch (mountType(i)) {
        case 0:
            mount = "ALT-AZ";
            break;
        case 1:
            mount = "EQUATORIAL";
            break;
        case 3:
            mount = "X-Y";
            break;
        case 2:
            mount = "ORBITING";
            break;
        case 4:
            mount = "ALT-AZ+NASMYTH-R";
            break;
        case 5:
            mount = "ALT-AZ+NASMYTH-L";
            break;
        case 6:
            mount = "BIZARRE";
            break;
        default:
            mount = "SPACE_HALCA";
            break;
        }
        //overwrite mount type for SMA
        if (doSMA) {
            mount = "ALT-AZ";
        }
        ant.flagRow().put(row, False);
        ant.mount().put(row, mount);
        if (_array == "CARMA" && _newNameStyle) {
            ostringstream oss;
            oss << "CA" << id(i);
            ant.name().put(row, oss.str());
        }
        else if (_array == "EVLA" && _newNameStyle) {
            ostringstream oss;
            oss << "EA" << setw(2) << setfill('0') << id(i);
            ant.name().put(row, oss.str());
        }
        else if (_array == "VLA" && _newNameStyle) {
            ostringstream oss;
            oss << "VA" << setw(2) << setfill('0') << id(i);
            //cerr << name(i) << endl;
            ant.name().put(row, oss.str());
        } 
        else {
            ant.name().put(row, String::toString(id(i)));
        }
        Vector<Double> offsets(3);
        offsets = 0.;
        offsets(0) = offset(i);
        ant.offset().put(row, offsets);
        ant.station().put(row, name(i).before('\0'));
        ant.type().put(row, "GROUND-BASED");

        // Do UVFITS-dependent position corrections:
        // ArrayColumn antXYZ(i) may need coord transform; do it in corXYZ:
        Vector<Double> corXYZ = antXYZ(i);
        if (rotate) {
            corXYZ = product(posRot, corXYZ);
        }
        if (leftHanded) {
            corXYZ(1) = -corXYZ(1);
        }
        ant.position().put(row, arrayXYZ + corXYZ);
        // store the angle for use in the feed table
        _receptorAngle(2 * i + 0) = polangleA(i) * C::degree;
        _receptorAngle(2 * i + 1) = polangleB(i) * C::degree;
    }
    // store these items in non-standard keywords for now
    ant.name().rwKeywordSet().define("ARRAY_NAME", arrnam);
    ant.position().rwKeywordSet().define("ARRAY_POSITION", arrayXYZ);
}

void MSFitsInput::fillSpectralWindowTable(BinaryTable& bt, Int nSpW)
{
  MSSpWindowColumns& msSpW(_msc->spectralWindow());
  MSDataDescColumns& msDD(_msc->dataDescription());
  MSPolarizationColumns& msPol(_msc->polarization());
  Int iFreq = getIndex(_coordType, "FREQ");
  Int nChan = _nPixel(iFreq);
  Int nCorr = _nPixel(getIndex(_coordType,"STOKES"));
  // assume spectral line, make source table to allow restfreq to be entered
  //if (nChan>33) addSourceTable_p=True; 
  if (nChan>0) _addSourceTable=True; 

  // fill out the polarization info (only single entry allowed in fits input)
  _ms.polarization().addRow();
  msPol.numCorr().put(0,nCorr);
  msPol.corrType().put(0,_corrType);
  msPol.corrProduct().put(0,_corrProduct);
  msPol.flagRow().put(0,False);

  //  Table fqTab=bt.fullTable("",Table::Scratch);
  Table fqTab=bt.fullTable();
  Int nRow=fqTab.nrow();
  ScalarColumn<Int> colFrqSel(fqTab,"FRQSEL");
  Matrix<Double> ifFreq(_nIF,nRow);
  Matrix<Float> chWidth(_nIF,nRow);
  Matrix<Float> totalBandwidth(_nIF,nRow);
  // The type of the column changes according to the number of entries
  if (_nIF==1) {
    ScalarColumn<Double> colIFFreq(fqTab,"IF FREQ");
    ScalarColumn<Float> colChWidth(fqTab,"CH WIDTH");
    ScalarColumn<Float> colTotalBandwidth(fqTab,"TOTAL BANDWIDTH");
    for (Int i=0; i<nRow; i++) {
      ifFreq(0,i)=colIFFreq(i);
      chWidth(0,i)=colChWidth(i);
      totalBandwidth(0,i)=colTotalBandwidth(i);
    }
  } else {
    ArrayColumn<Double> colIFFreq(fqTab,"IF FREQ");
    ArrayColumn<Float> colChWidth(fqTab,"CH WIDTH");
    ArrayColumn<Float> colTotalBandwidth(fqTab,"TOTAL BANDWIDTH");
    try{
      colIFFreq.getColumn(ifFreq);
      colChWidth.getColumn(chWidth);
      colTotalBandwidth.getColumn(totalBandwidth);
    }catch(std::exception& x) {
      _log << LogOrigin("MSFitsInput", "fillSpectralWindowTable")
             << LogIO::DEBUG1 << x.what() << LogIO::POST;
    }
    catch(...) {
      _log << LogOrigin("MSFitsInput", "fillSpectralWindowTable")
             << LogIO::DEBUG1 << "unknown Error"  << LogIO::POST;
    }
  }

  for (Int spw=0; spw<nSpW; spw++) {
    _ms.spectralWindow().addRow();
    _ms.dataDescription().addRow();
    
    msDD.spectralWindowId().put(spw,spw);
    msDD.polarizationId().put(spw,0);
    msDD.flagRow().put(spw,False);
    Int ifc=0;
    Int freqGroup = 0;
    if (_nIF>0) {
      ifc=spw%_nIF;
      freqGroup = spw/_nIF;
    }
    Int fqRow=spw/max(1,_nIF);
    if (fqRow != colFrqSel(fqRow)-1) 
      _log << LogIO::SEVERE << "Trouble interpreting FQ table, id's may be wrong" << LogIO::POST;
    msSpW.name().put(spw,"none");
    msSpW.ifConvChain().put(spw,ifc);
    msSpW.numChan().put(spw,nChan);
    Double refChan = _refPix(iFreq);
    // using data from FQ table
    Double refFreq=_refVal(iFreq)+ifFreq(ifc,fqRow);

    Double chanBandwidth=chWidth(ifc,fqRow);
    //TT debug
    Vector<Double> chanFreq(nChan),resolution(nChan);
    for (Int i=0; i < nChan; i++) {
      chanFreq(i)= refFreq + (i+1-refChan) * chanBandwidth;
    }
    resolution=abs(chanBandwidth);

    //if altrval (and altrpix) fits keywords exist use
    //recalucalated values instead of the data form FQ table
    if (_useAltrval) {

      refFreq = _refFreq;
      chanFreq = _chanFreq;
    }
    msSpW.chanFreq().put(spw,chanFreq);
    msSpW.chanWidth().put(spw,resolution); 
    msSpW.effectiveBW().put(spw,resolution);
    msSpW.refFrequency().put(spw,refFreq);
    msSpW.resolution().put(spw,resolution);
    msSpW.totalBandwidth().put(spw,totalBandwidth(ifc,fqRow));
    if (chanBandwidth>0) {
      msSpW.netSideband().put(spw,1);
    } else {
      msSpW.netSideband().put(spw,-1);
    }
    msSpW.freqGroup().put(spw,freqGroup);
    msSpW.freqGroupName().put(spw,"none");
    msSpW.flagRow().put(spw,False);
    // set the reference frames for frequency
    msSpW.measFreqRef().put(spw,_freqsys);
  }
}

void MSFitsInput::fillSpectralWindowTable()
{
  MSSpWindowColumns& msSpW(_msc->spectralWindow());
  MSDataDescColumns& msDD(_msc->dataDescription());
  MSPolarizationColumns& msPol(_msc->polarization());
  Int iFreq = getIndex(_coordType, "FREQ");
  Int nChan = _nPixel(iFreq);
  Int nCorr = _nPixel(getIndex(_coordType,"STOKES"));
  // assume spectral line, make source table to allow restfreq to be entered
  //if (nChan>33) addSourceTable_p=True; 
  if (nChan>0) _addSourceTable=True; 

  // fill out the polarization info (only single entry allowed in fits input)
  _ms.polarization().addRow();
  msPol.numCorr().put(0,nCorr);
  msPol.corrType().put(0,_corrType);
  msPol.corrProduct().put(0,_corrProduct);
  msPol.flagRow().put(0,False);

  Int spw=0;
  _ms.spectralWindow().addRow();
  _ms.dataDescription().addRow();

  msDD.spectralWindowId().put(spw,spw);
  msDD.polarizationId().put(spw,0);
  msDD.flagRow().put(spw,False); 

  msSpW.name().put(spw,"none");
  msSpW.ifConvChain().put(spw,0);
  msSpW.numChan().put(spw,nChan);
  Double refChan = _refPix(iFreq);
  Double refFreq=_refVal(iFreq);
  Double chanBandwidth=_delta(iFreq);
  Vector<Double> chanFreq(nChan),resolution(nChan);
  for (Int i=0; i < nChan; i++) {
    chanFreq(i)= refFreq + (i+1-refChan) * chanBandwidth;
  }
  resolution=chanBandwidth;
  //if altrval (and altrpix) fits keywords exist use
  //recalucalated values
  if (_useAltrval) {
    refFreq = _refFreq;
    chanFreq = _chanFreq;
  }
  msSpW.chanFreq().put(spw,chanFreq);
  msSpW.chanWidth().put(spw,resolution);
  msSpW.effectiveBW().put(spw,resolution);
  msSpW.refFrequency().put(spw,refFreq);
  msSpW.resolution().put(spw,resolution);
  msSpW.totalBandwidth().put(spw,abs(nChan*chanBandwidth));
  if (chanBandwidth>0) {
    msSpW.netSideband().put(spw,1);
  } else {
    msSpW.netSideband().put(spw,-1);
  }
  msSpW.freqGroup().put(spw,0);
  msSpW.freqGroupName().put(spw,"none");
  msSpW.flagRow().put(spw,False);
  // set the reference frames for frequency
  msSpW.measFreqRef().put(spw,_freqsys);
}

// Returns the Direction Measure reference for UVW and other appropriate columns
// in _msc (which must exist but have empty columns before you can set it!).
MDirection::Types MSFitsInput::getDirectionFrame(Double epoch) {

    MDirection::Types epochRef = MDirection::J2000;
    if (nearAbs(epoch, 1950.0, 0.01))
        epochRef = _array == "VLA" ? MDirection::B1950_VLA : MDirection::B1950;
    _log << LogOrigin("MSFitsInput", "getDirectionFrame")
           << LogIO::DEBUG1 << "epochRef ok " << LogIO::POST;

    return epochRef;
}

void MSFitsInput::fillFieldTable(BinaryTable& bt, Int nField) {
    MSFieldColumns& msField(_msc->field());
    TableRecord btKeywords = bt.getKeywords();
    if (!btKeywords.isDefined("NO_IF")) {
        throw(AipsError("MSFitsInput: Illegal SU file: no number of IFs"));
    }
    uInt noif = bt.getKeywords().asuInt("NO_IF");
    // Table suTab=bt.fullTable("",Table::Scratch);
    Table suTab = bt.fullTable();
    ScalarColumn<Int> id(suTab, "ID. NO.");
    ScalarColumn<String> name(suTab, "SOURCE");
    ScalarColumn<Int> qual(suTab, "QUAL");
    Bool multiqual = False;
    Int minqual, maxqual;
    minMax(minqual, maxqual, qual.getColumn());
    if (minqual != maxqual)
        multiqual = True;
    ScalarColumn<String> code(suTab, "CALCODE");
    // ScalarColumn<Float> iflux(suTab,"IFLUX"); // etc Q, U, V (Jy)
    ScalarColumn<Double> ra(suTab, "RAEPO"); //degrees
    ScalarColumn<Double> dec(suTab, "DECEPO"); //degrees
    ScalarColumn<Double> raapp(suTab, "RAAPP"); //degrees
    ScalarColumn<Double> decapp(suTab, "DECAPP"); //degrees
    ScalarColumn<Double> epoch(suTab, "EPOCH"); //years
    ScalarColumn<Double> pmra(suTab, "PMRA"); //deg/day
    ScalarColumn<Double> pmdec(suTab, "PMDEC"); //deg/day
    if (Int(suTab.nrow()) < nField) {
        _log << LogOrigin("MSFitsInput", __func__)
               << LogIO::NORMAL
                << "Input Source id's not sequential, adding empty rows in output"
                << LogIO::POST;
    }
    Int outRow = -1;
    // RESTFREQ and LSRVEL are 2D columns according to the AIPS Memo 117
    //restFreq_p.resize(noif, suTab.nrow());
    _sysVel.resize(noif, suTab.nrow());
    Bool throwImmediately = False;
    try {
    	ArrayColumn<Double> restfreq(suTab,"RESTFREQ");  // Hz
    	ArrayColumn<Double> sysvel(suTab,"LSRVEL"); // m/s
    	restfreq.getColumn(_restFreq);
        // purposeful assignment of throwImmediately
        // because it appears that the sense of rows and columns are reversed here
        uInt nrestfreqs = _restFreq.nrow();
        throwImmediately = nrestfreqs != noif;
        ThrowIf(
    			throwImmediately,
    			"Inconsistent SU table, number of elements in rest frequency column is "
    			+ String::toString(nrestfreqs) + " but number of IFs is "
    			+ String::toString(noif)
    	);
    	sysvel.getColumn(_sysVel);
    }
    catch (const std::exception& x) {
    	ThrowIf(throwImmediately, x.what());
    	if(noif>1){
    		_log << LogOrigin("MSFitsInput", __func__) << LogIO::WARN
    				<< x.what() << ": " << "Inconsistent setup of RESTFREQ and LSRVEL columns." << endl
    				<< "With NO_IF>1, they should be arrays not scalars." << LogIO::POST;
    	}
      _restFreq.resize(noif, suTab.nrow());
      ScalarColumn<Double> restfreq(suTab,"RESTFREQ");  // Hz
      ScalarColumn<Double> sysvel(suTab,"LSRVEL"); // m/s
      Vector<Double> tmprf(suTab.nrow());
      Vector<Double> tmpsv(suTab.nrow());
      restfreq.getColumn(tmprf);
      sysvel.getColumn(tmpsv);
      for(uInt ii=0; ii<suTab.nrow(); ii++){
	_restFreq(0,ii) = tmprf(ii);
	_sysVel(0,ii) = tmpsv(ii);
      }
    }      
    // set the DIRECTION MEASURE REFERENCE for appropriate columns

    //   IF UVFITS CAME FROM AIPS, AND THE FIRST ROW OF SU TABLE
    //   CONTAINS A PLANET THAT WAS TRACKED BY THE CORRELATOR (EPOCH=-1),
    //   THE D.M. REFERENCE WILL BE J2000, WHICH MAY NOT BE CORRECT
    //   FOR THE PLANETS IN THE LIST (see defect 3636).

    MDirection::Types epochRefZero = getDirectionFrame(epoch(0));
    if (epochRefZero != _epochRef)
        _log << LogOrigin("MSFitsInput", __func__)
               << LogIO::WARN << "The direction measure reference code, "
                << epochRefZero << "\n"
                << "for the first field does not match the one from the FITS header, "
                << _epochRef
                << ".\nThis might cause a problem for the reference frame"
                << " of the output's UVW column." << LogIO::POST;
    for (Int inRow = 0; inRow < (Int) suTab.nrow(); inRow++) {
        Int fld = id(inRow) - 1;
        // add empty rows until the row number in the output matches the source id
        while (fld > outRow) {
            // Append a flagged, empty row to the FIELD table
            _ms.field().addRow();
            outRow++;
            Vector<MDirection> nullDir(1);
            nullDir(0).set(MVDirection(0.0, 0.0), MDirection::Ref(epochRefZero));
            msField.phaseDirMeasCol().put(outRow, nullDir);
            msField.delayDirMeasCol().put(outRow, nullDir);
            msField.referenceDirMeasCol().put(outRow, nullDir);
            msField.flagRow().put(outRow, True);
        }
        msField.sourceId().put(fld, -1); // source table not filled in
        msField.code().put(fld, code(inRow));
        String theFldName;
        if (multiqual)
            theFldName = name(inRow) + "_" + String::toString(qual(inRow));
        else
            theFldName = name(inRow);
        msField.name().put(fld, theFldName);
        Int numPoly = 0;
        if (!nearAbs(pmra(inRow), 0.0) || !nearAbs(pmdec(inRow), 0.0)) {
            numPoly = 1;
        }
        // The code below will write the direction in B1950 or J2000 coordinates if
        // the direction is constant. However it will use apparent Coordinates (I
        // am not sure if this means APP, JTRUE, BTRUE or what), if the proper
        // motion is non-zero.  If the epoch in the incoming SU
        // table is "-1" (via AIPS UVFITS, a planet tracked by the correlator), it
        // will adopt the epochRefZero and use the ra/dec (not raapp/decapp).
        // The handling of planets should be cleaned up (defect 3636).
        // In all cases the time will be the date of the start of the observation.
        MDirection::Types epochRef = MDirection::APP;
        MVDirection refDir;
        if (numPoly == 0) {
            if (near(epoch(inRow), 2000.0, 0.01)) {
                epochRef = MDirection::J2000;
            } else if (nearAbs(epoch(inRow), 1950.0, 0.01)) {
                if (_array == "VLA")
                    epochRef = MDirection::B1950_VLA;
                else
                    epochRef = MDirection::B1950;
            } else if (epoch(inRow) == -1.0) {
                epochRef = epochRefZero;
                _log << LogOrigin("MSFitsInput", __func__)
                       << " Assuming standard epoch " << " for " << name(inRow)
                        << ".  Be aware that this may not be correct." << endl;
            } else {
                _log << LogOrigin("MSFitsInput", __func__)
                       << " Cannot handle epoch in SU table: " << epoch(inRow)
                        << LogIO::EXCEPTION;
            }
            refDir = MVDirection(ra(inRow) * C::degree, dec(inRow) * C::degree);

        } else {
            refDir = MVDirection(raapp(inRow) * C::degree, decapp(inRow)
                    * C::degree);
        }
        Vector<MDirection> radecMeas(numPoly + 1);
        radecMeas(0).set(refDir, MDirection::Ref(epochRef));
        if (numPoly == 1) {
            radecMeas(1).set(MVDirection(pmra(inRow) * C::degree / C::day,
                    pmdec(inRow) * C::degree / C::day), MDirection::Ref(
                    epochRef));
        }

        // Get the time from the observation subtable. I have assumed that this bit
        // of the observation table has been filled by now.
        const Vector<Double> obsTimes = _msc->observation().timeRange()(0);

        msField.time().put(fld, obsTimes(0));
        msField.numPoly().put(fld, numPoly);
        msField.delayDirMeasCol().put(fld, radecMeas);
        msField.phaseDirMeasCol().put(fld, radecMeas);
        msField.referenceDirMeasCol().put(fld, radecMeas);
        msField.flagRow().put(fld, False);
    }
}

// single source fits case
void MSFitsInput::fillFieldTable(Int nField) {
    // some UVFITS files have the source number set, but have no SU
    // table. We will assume there is only a single source in that case
    // and set all fieldId's back to zero
    if (nField > 1) {
        _msc->fieldId().fillColumn(0);
    }

    MSFieldColumns& msField(_msc->field());
    _ms.field().addRow();
    Int fld = 0;
    msField.sourceId().put(fld, -1); // source table not used
    msField.code().put(fld, " ");
    msField.name().put(fld, _object);
    Vector<MDirection> radecMeas(1);
    radecMeas(0).set(MVDirection(_refVal(getIndex(_coordType, "RA"))
            * C::degree, _refVal(getIndex(_coordType, "DEC")) * C::degree),
            MDirection::Ref(_epochRef));

    msField.numPoly().put(fld, 0);
    msField.delayDirMeasCol().put(fld, radecMeas);
    msField.phaseDirMeasCol().put(fld, radecMeas);
    msField.referenceDirMeasCol().put(fld, radecMeas);

    // Use TIME_RANGE in OBSERVATION table to set TIME here.
    const Vector<Double> obsTimes = _msc->observation().timeRange()(0);
    msField.time().put(fld, obsTimes(0));

}

void MSFitsInput::fillFeedTable() {
    MSFeedColumns& msfc(_msc->feed());

    // find out the POLARIZATION_TYPE
    // In the fits files we handle there can be only a single, uniform type
    // of polarization so the following should work.
    MSPolarizationColumns& msPolC(_msc->polarization());
    Int numCorr = msPolC.numCorr()(0);
    Vector<String> rec_type(2);
    rec_type = "?";
    if (_corrType(0) >= Stokes::RR && _corrType(numCorr - 1) <= Stokes::LL) {
        rec_type(0) = "R";
        rec_type(1) = "L";
    }
    if (_corrType(0) >= Stokes::XX && _corrType(numCorr - 1) <= Stokes::YY) {
        rec_type(0) = "X";
        rec_type(1) = "Y";
    }

    Matrix<Complex> polResponse(2, 2);
    polResponse = 0.;
    polResponse(0, 0) = polResponse(1, 1) = 1.;
    Matrix<Double> offset(2, 2);
    offset = 0.;
    Vector<Double> position(3);
    position = 0.;

    // fill the feed table
    Int row = -1;
    // Use TIME_RANGE in OBSERVATION table to set TIME here.
    const Vector<Double> obsTimes = _msc->observation().timeRange()(0);
    // nAnt as here ensures ANTENNA and FEED have the same number
    //   of rows since some ants may not be present in the visibility data
    Int nAnt = _msc->antenna().nrow();
    for (Int ant = 0; ant < nAnt; ++ant) {
        _ms.feed().addRow();
        row++;
        msfc.antennaId().put(row, ant);
        msfc.beamId().put(row, -1);
        msfc.feedId().put(row, 0);
        msfc.interval().put(row, 0);
        //    msfc.phasedFeedId().put(row,-1);
        msfc.spectralWindowId().put(row, -1); // all
        msfc.time().put(row, obsTimes(0));
        msfc.numReceptors().put(row, 2);
        msfc.beamOffset().put(row, offset);
        msfc.polarizationType().put(row, rec_type);
        msfc.polResponse().put(row, polResponse);
        msfc.position().put(row, position);
        msfc.receptorAngle().put(row, _receptorAngle(Slice(2 * ant, 2)));
    }
}

void MSFitsInput::fillExtraTables() {
    // fill the pointing table and possibly the source table
    // run though the main table, find field changes, and add pointing rows
    // as needed by looking up the field info in the field table
    // If requested also look for new spectralwindows and add source
    // table entries for each field/spw combination

    if (_addSourceTable)
        _log << LogOrigin("MSFitsInput", __func__)
               << LogIO::NORMAL << "Filling SOURCE table (this may take some time)." << LogIO::POST;

    Int nrow = _ms.nrow();
    Int nAnt = _ms.antenna().nrow();
    Int lastFieldId = -1;
    Int lastDDId = -1;
    Double lastTime = 0;
    Vector<Int> fieldId = _msc->fieldId().getColumn();
    Vector<Int> ddId;
    if (_addSourceTable){
      ddId = _msc->dataDescId().getColumn();
    }

    std::map<pair<Int,Int>, Int> sourceFieldIndex; // for the case we need to write the source table

    ProgressMeter meter(0.0, nrow * 1.0, "UVFITS Filler", "rows copied",
                "", "", True, nrow / 100);

    for (Int i = 0; i < nrow; i++) {
        if (fieldId(i) != lastFieldId || (_addSourceTable && ddId(i)
                != lastDDId)) {
            lastFieldId = fieldId(i);
            if (i > 0)
                lastTime = _msc->time()(i - 1);
            Array<Double> pointingDir = _msc->field().phaseDir()(lastFieldId);
            String name = _msc->field().name()(lastFieldId);
            //Int numPoly = _msc->field().numPoly()(lastFieldId);
            Double time = _msc->time()(i);
            Int np = _ms.pointing().nrow();
            if (np > 0) {
                // fix up time and interval for previous entries
                Double midTime = (lastTime + _msc->pointing().time()(np - 1))
                        / 2;
                Double interval = lastTime - _msc->pointing().time()(np - 1)
                        + _msc->interval()(i - 1);
                for (Int j = 0; j < nAnt; j++) {
                    _msc->pointing().time().put(np - j - 1, midTime);
                    _msc->pointing().timeOrigin().put(np - j - 1, midTime);
                    _msc->pointing().interval().put(np - j - 1, interval);
                }
            }
            /* This is not right for concatenating later for mosaicing
             As it is a useless piece of info copy from Field...field table will
             do

             // The ISMStMan is used for all but antennaId, so only put once
             for (Int j=0; j<nAnt; j++) {
             _ms.pointing().addRow();
             _msc->pointing().antennaId().put(np+j, j);
             if (j==0) {
             _msc->pointing().time().put(np+j,time);
             _msc->pointing().timeOrigin().put(np+j,time);
             _msc->pointing().interval().put(np+j,0);
             _msc->pointing().name().put(np+j, name);
             _msc->pointing().numPoly().put(np+j, numPoly);
             _msc->pointing().direction().put(np+j,pointingDir);
             _msc->pointing().target().put(np+j,pointingDir);
             _msc->pointing().tracking().put(np+j,True);
             }
             }
             */
            if (_addSourceTable) {

                lastDDId = ddId(i);
                Int spwId = _msc->dataDescription().spectralWindowId()(lastDDId);
                // now check if we've seen this field for this spectral window
                // Use indexed access to the SOURCE sub-table
		pair<Int, Int> myfldspw = make_pair(lastFieldId, spwId);
		if(sourceFieldIndex.find(myfldspw) == sourceFieldIndex.end()){

                    sourceFieldIndex.insert(std::make_pair(myfldspw, 1)); 

                    _ms.source().addRow();
                    Int j = _ms.source().nrow() - 1;
                    MSSourceColumns & mss = _msc->source();
                    mss.sourceId().put(j, lastFieldId);
                    _msc->field().sourceId().put(lastFieldId, lastFieldId);
                    mss.name().put(j, name);
                    Matrix<Double> phaseDir = _msc->field().phaseDir()(
                            lastFieldId);
                    Vector<Double> srcDir = phaseDir.column(0), rate(2);
                    if (phaseDir.ncolumn() > 1)
                        rate = phaseDir.column(1);
                    else
                        rate = 0.0;
                    mss.direction().put(j, srcDir);
                    mss.properMotion().put(j, rate);
                    mss.time().put(j, time);
                    mss.interval().put(j, DBL_MAX);
                    mss.spectralWindowId().put(j, spwId);
                    Vector<Double> sysVel(1);
		    // sysVel was extracted from LSRVEL in SU table
		    if(0<=lastFieldId && (uInt)lastFieldId<_sysVel.ncolumn()){
		      sysVel(0) = _sysVel(0, lastFieldId);
		    }
		    else{
		      _log << LogOrigin("MSFitsInput", "fillExtraTable")
			     << LogIO::WARN << "No systemic velocity for field " << lastFieldId << LogIO::POST;
		      sysVel(0) = 0.;
		    }		      

                    mss.sysvel().put(j, sysVel);
                    mss.numLines().put(j, 1);
                    Vector<String> transition(1);
                    transition(0) = "";
                    mss.transition().put(j, transition);
                    Vector<Double> restFreqs(1);

		    if(0<=lastFieldId && (uInt)lastFieldId<_restFreq.ncolumn()){
		      restFreqs(0) = _restFreq(0, lastFieldId);
		    }
		    else{
		      _log << LogOrigin("MSFitsInput", "fillExtraTable")
			     << LogIO::WARN << "No rest frequency for field " << lastFieldId << LogIO::POST;
		      restFreqs(0) = 0.;
		    }		      

                    mss.restFrequency().put(j, restFreqs);
                    mss.calibrationGroup().put(j, -1);
		    // sourceModel is left as is (we have no model information to fill in)
                }
            }
        }
        meter.update((i + 1) * 1.0);
    }

    // fix up last interval
    lastTime = _msc->time()(nrow - 1);
    Int np = _ms.pointing().nrow();
    if (np > 0) {
        // fix up time and interval for previous entries
        Double midTime = (lastTime + _msc->pointing().time()(np - 1)) / 2;
        Double interval = lastTime - _msc->pointing().time()(np - 1)
                + _msc->interval()(nrow - 1);
        for (Int j = 0; j < nAnt; j++) {
            _msc->pointing().time().put(np - j - 1, midTime);
            _msc->pointing().timeOrigin().put(np - j - 1, midTime);
            _msc->pointing().interval().put(np - j - 1, interval);
        }
    }
}

void MSFitsInput::fixEpochReferences() {
    if (_timsys == "IAT")
        _timsys = "TAI";
    if (_timsys == "UTC" || _timsys == "TAI") {
        if (_timsys == "UTC")
            _msc->setEpochRef(MEpoch::UTC, False);
        if (_timsys == "TAI")
            _msc->setEpochRef(MEpoch::TAI, False);
    } else {
        if (_timsys != "")
            _log << LogOrigin("MSFitsInput", "fixEpochReferences")
                   << LogIO::SEVERE << "Unhandled time reference frame: "
                    << _timsys << LogIO::POST;
    }
}

void MSFitsInput::setFreqFrameVar(BinaryTable& binTab) {

    ConstFitsKeywordList kwlist = binTab.kwlist();
    const FitsKeyword* kw;

    kwlist.first();
    String frame;

    while ((kw = kwlist.next())) {
        String kwname = kw->name();
        if (kwname == "VELTYP") {
            frame = kw->asString();
        }
    }
    if (frame.contains("LSR")) {
        _freqsys = MFrequency::LSRK; // because some smart people use only LSR
        if (frame.contains("LSRD")) // in uvfits !
            _freqsys = MFrequency::LSRD;
    } else if (frame.contains("REST")) {
        _freqsys = MFrequency::REST;
    } else if (frame.contains("BARY")) {
        _freqsys = MFrequency::BARY;
    } else if (frame.contains("GEO")) {
        _freqsys = MFrequency::GEO;
    } else if (frame.contains("TOPO")) {
        _freqsys = MFrequency::TOPO;
    } else if (frame.contains("GALAC")) {
        _freqsys = MFrequency::GALACTO;
    } else if (frame.contains("LOCAL") || frame.contains("LGROUP")) {
        _freqsys = MFrequency::LGROUP;
    } else if (frame.contains("CMB")) {
        _freqsys = MFrequency::CMB;
    }
}

void MSFitsInput::updateSpectralWindowTable() {

    MSSpWindowColumns& msSpW(_msc->spectralWindow());
    msSpW.measFreqRef().fillColumn(_freqsys);

}

void MSFitsInput::checkRequiredAxis() {
   // Check if required axes are there
   if (getIndex(_coordType, "COMPLEX") < 0) {
       _log << "Data does not have a COMPLEX axis" << LogIO::EXCEPTION;
   }
   if (getIndex(_coordType, "STOKES") < 0) {
       _log << "Data does not have a STOKES axis" << LogIO::EXCEPTION;
   }
   if (getIndex(_coordType, "FREQ") < 0) {
       _log << "Data does not have a FREQ axis" << LogIO::EXCEPTION;
   }
   if ((getIndex(_coordType, "RA") < 0) && (getIndex(_coordType, "RA---SIN")
           < 0) && (getIndex(_coordType, "RA---NCP") < 0) && (getIndex(
           _coordType, "RA---SCP") < 0)) {
       _log << "Data does not have a RA axis" << LogIO::EXCEPTION;
   }
   if ((getIndex(_coordType, "DEC") < 0)
           && (getIndex(_coordType, "DEC--SIN") < 0) && (getIndex(
           _coordType, "DEC--NCP") < 0) && (getIndex(_coordType, "DEC--SCP")
           < 0)) {
       _log << "Data does not have a DEC axis" << LogIO::EXCEPTION;
   }
}

void MSFitsInput::getAxisInfo(ConstFitsKeywordList& kwl) {
    // Extracts the axis related info. from the UV table keyword list and
    // saves them in the arrays.
    kwl.first();
    const Regex trailing(" *$");

    const FitsKeyword *kw;
    String table = (kw = kwl(FITS::EXTNAME)) ? kw->asString() : "";
    if (!table.contains("UV")) {
         _log << "This is not a uv table!" << LogIO::EXCEPTION;
    }
    const Int nAxis = kwl(FITS::TFIELDS)->asInt();
    if (nAxis < 1) {
        _log << "Data has no axes!" << LogIO::EXCEPTION;
    }

    String tdim = kwl("TDIM")->asString();
    IPosition ipos;
    FITSKeywordUtil::fromTDIM(ipos, tdim);

    uInt shp =ipos.nelements();
    _nPixel.resize(shp);
    _refVal.resize(shp);
    _refPix.resize(shp);
    _delta.resize(shp);
    _coordType.resize(shp);

    for (uInt i = 0; i < shp; i++) {
        _nPixel(i) = ipos(i);
        if (_nPixel(i) < 0) {
            _log << "Axes " << i << " cannot have a negative value"
                    << LogIO::EXCEPTION;
        }
        const char* tmp;

        tmp = (String::toString(i + 1).append("CTYP").append(String::toString(nAxis))).chars();
        _coordType(i) = String(kwl(tmp)->asString()).before(trailing);

        tmp = (String::toString(i + 1).append("CRVL").append(String::toString(nAxis))).chars();
        _refVal(i) = kwl(tmp)->asDouble();

        tmp = (String::toString(i + 1).append("CRPX").append(String::toString(nAxis))).chars();
        _refPix(i) = kwl(tmp)->asDouble();

        tmp = (String::toString(i + 1).append("CDLT").append(String::toString(nAxis))).chars();
        _delta(i) = kwl(tmp)->asDouble();

        //tmp = (String::toString(i + 1).append("CROT").append(String::toString(nAxis))).chars();
        //cRot_p(i) = kwl(tmp)->asDouble();
    }
    _log << LogOrigin("MSFitsInput", "fillMSMainTable")
               << LogIO::DEBUG1
               << "coordType=" << _coordType
               << "\nrefVal=" << _refVal
               << "\nrefPix=" << _refPix
               << "\ndelta=" << _delta
               << "\n_nPixel=" << _nPixel
               << LogIO::POST;

}

void MSFitsInput::sortPolarizations() {
    // Sort out the order of the polarizations and find the sort indices
    // to put them in 'standard' order: PP,PQ,QP,QQ
    const uInt iPol = getIndex(_coordType, "STOKES");
    const uInt numCorr = _nPixel(iPol);
    _corrType.resize(numCorr);
    for (uInt i = 0; i < numCorr; i++) {
        // note: 1-based ref pix
        _corrType(i) = ifloor(_refVal(iPol) + (i + 1 - _refPix(iPol))
                * _delta(iPol) + 0.5);
        // convert AIPS-convention Stokes description to Casacore enum
        switch (_corrType(i)) {
        case -8:
            _corrType(i) = Stokes::YX;
            break;
        case -7:
            _corrType(i) = Stokes::XY;
            break;
        case -6:
            _corrType(i) = Stokes::YY;
            break;
        case -5:
            _corrType(i) = Stokes::XX;
            break;
        case -4:
            _corrType(i) = Stokes::LR;
            break;
        case -3:
            _corrType(i) = Stokes::RL;
            break;
        case -2:
            _corrType(i) = Stokes::LL;
            break;
        case -1:
            _corrType(i) = Stokes::RR;
            break;
        default:
            if (_corrType(i) < 0) {
                _log << "Unknown Correlation type: " << _corrType(i)
                        << LogIO::EXCEPTION;
            }
        }
    }
    Vector<Int> tmp(_corrType.copy());
    // Sort the polarizations to standard order. Could probably use
    // GenSortIndirect here.
    GenSort<Int>::sort(_corrType);
    _corrIndex.resize(numCorr);
    // Get the sort indices to rearrange the data to standard order
    for (uInt i = 0; i < numCorr; i++) {
        for (uInt j = 0; j < numCorr; j++) {
            if (_corrType(j) == tmp(i))
                _corrIndex[i] = j;
        }
    }
    
    // Figure out the correlation products from the polarizations
    _corrProduct.resize(2, numCorr);
    _corrProduct = 0;
    for (uInt i = 0; i < numCorr; i++) {
        const Stokes::StokesTypes cType = Stokes::type(_corrType(i));
        Fallible<Int> receptor = Stokes::receptor1(cType);
        Bool warn = False;
        if (receptor.isValid()) {
            _corrProduct(0, i) = receptor;
        } else if (!warn) {
            warn = True;
            _log << LogIO::WARN
                    << "Cannot deduce receptor 1 for correlations of type: "
                    << Stokes::name(cType) << LogIO::POST;
        }
        receptor = Stokes::receptor2(cType);
        if (receptor.isValid()) {
            _corrProduct(1, i) = receptor;
        } else if (!warn) {
            warn = True;
            _log << LogIO::WARN
                    << "Cannot deduce receptor 2 for correlations of type: "
                    << Stokes::name(cType) << LogIO::POST;
        }
    }
}

void MSFitsInput::fillPolarizationTable() {
    MSPolarizationColumns& msPol(_msc->polarization());
    Int nCorr = _nPixel(getIndex(_coordType, "STOKES"));
    // fill out the polarization info (only single entry allowed in fits input)
    _ms.polarization().addRow();
    msPol.numCorr().put(0, nCorr);
    msPol.corrType().put(0, _corrType);
    msPol.corrProduct().put(0, _corrProduct);
    msPol.flagRow().put(0, False);
}

void MSFitsInput::fillSpectralWindowTable(BinaryTable& bt) {

    MSSpWindowColumns& msSpW(_msc->spectralWindow());
    MSDataDescColumns& msDD(_msc->dataDescription());

    const Regex trailing(" *$"); 
    ConstFitsKeywordList kwl = bt.kwlist();
    const FitsKeyword* kw;
    kwl.first();
    Int nIF = (kw = kwl("NO_IF")) ? kw->asInt() : 1;
    _nIF = nIF;

    Table fqTab = bt.fullTable();
    Int nRow = fqTab.nrow();
    ScalarColumn<Int> colFrqSel(fqTab, "FRQSEL");
    Matrix<Double> ifFreq(_nIF, nRow);
    Matrix<Float> chWidth(_nIF, nRow);
    Matrix<Float> totalBandwidth(_nIF, nRow);
    // The type of the column changes according to the number of entries
    if (_nIF == 1) {
        ScalarColumn<Double> colIFFreq(fqTab, "IF FREQ");
        ScalarColumn<Float> colChWidth(fqTab, "CH WIDTH");
        ScalarColumn<Float> colTotalBandwidth(fqTab, "TOTAL BANDWIDTH");
        for (Int i = 0; i < nRow; i++) {
            ifFreq(0, i) = colIFFreq(i);
            chWidth(0, i) = colChWidth(i);
            totalBandwidth(0, i) = colTotalBandwidth(i);
        }
    }
    else {
        ArrayColumn<Double> colIFFreq(fqTab, "IF FREQ");
        ArrayColumn<Float> colChWidth(fqTab, "CH WIDTH");
        ArrayColumn<Float> colTotalBandwidth(fqTab, "TOTAL BANDWIDTH");
        colIFFreq.getColumn(ifFreq);
        colChWidth.getColumn(chWidth);
        colTotalBandwidth.getColumn(totalBandwidth);
    }

    Int nSpW = nIF;
    Int iFreq = getIndex(_coordType, "FREQ");
    Int nChan = _nPixel(iFreq);
    if (nChan > 0)
                _addSourceTable = True;

    for (Int spw = 0; spw < nSpW; spw++) {
        _ms.spectralWindow().addRow();
        _ms.dataDescription().addRow();

        msDD.spectralWindowId().put(spw, spw);
        msDD.polarizationId().put(spw, 0);
        msDD.flagRow().put(spw, False);
        Int ifc = 0;
        Int freqGroup = 0;
        if (_nIF > 0) {
            ifc = spw % _nIF;
            freqGroup = spw / _nIF;
        }
        Int fqRow = spw / max(1, _nIF);
        if (fqRow != colFrqSel(fqRow) - 1)
            _log << LogOrigin("MSFitsInput", "fillSpectralWindowTable")
                   << LogIO::SEVERE
                    << "Trouble interpreting FQ table, id's may be wrong"
                    << LogIO::POST;
        msSpW.name().put(spw, "none");
        msSpW.ifConvChain().put(spw, ifc);
        msSpW.numChan().put(spw, nChan);
        Double refChan = _refPix(iFreq);
        // using data from FQ table
        Double refFreq = _refVal(iFreq) + ifFreq(ifc, fqRow);

        Double chanBandwidth = chWidth(ifc, fqRow);
        Vector<Double> chanFreq(nChan), resolution(nChan);
        for (Int i = 0; i < nChan; i++) {
            chanFreq(i) = refFreq + (i + 1 - refChan) * chanBandwidth;
        }
        resolution = abs(chanBandwidth);

        //if altrval (and altrpix) fits keywords exist use
        //recalucalated values instead of the data form FQ table
        /*
        if (useAltrval) {

            refFreq = refFreq_p;
            chanFreq = chanFreq_p;
        }
        */
        msSpW.chanFreq().put(spw, chanFreq);
        msSpW.chanWidth().put(spw, resolution);
        msSpW.effectiveBW().put(spw, resolution);
        msSpW.refFrequency().put(spw, refFreq);
        msSpW.resolution().put(spw, resolution);
        msSpW.totalBandwidth().put(spw, totalBandwidth(ifc, fqRow));
        if (chanBandwidth > 0) {
            msSpW.netSideband().put(spw, 1);
        }
        else {
            msSpW.netSideband().put(spw, -1);
        }
        msSpW.freqGroup().put(spw, freqGroup);
        msSpW.freqGroupName().put(spw, "none");
        msSpW.flagRow().put(spw, False);
        // set the reference frames for frequency
        //msSpW.measFreqRef().put(spw, freqsys_p);
    }
}

void MSFitsInput::fillMSMainTable(BinaryTable& bt) {
    MSColumns& msc(*_msc);
    const Regex trailing(" *$");

    ConstFitsKeywordList kwl = bt.kwlist();
    //FitsKeywordList pkw = kwl;
    const FitsKeyword* kw;
    kwl.first();

    // get the uv table column names
    Int nFields = (kw = kwl("TFIELDS")) ? kw->asInt() : -1;
    if (nFields == -1) {
        _log << LogOrigin("MSFitsInput", "fillMSMainTable")
                << "Could not find the number of fields of the uv table"
                << LogIO::EXCEPTION;
    }
 
    String object = (kw = kwl(FITS::OBJECT)) ? kw->asString()
            : "unknown";

    Vector<String> TType(nFields);
    Vector<Double> TScal(nFields);
    Vector<Double> TZero(nFields);

    kwl.first();
    for (Int i = 0; i < nFields; i++) {
        TType(i) = (kw = kwl(FITS::TTYPE, i + 1)) ? String(kw->asString()) : "";
        TType(i) = TType(i).before(trailing);
        TScal(i) = (kw = kwl(FITS::TSCAL, i + 1)) ? kw->asDouble() : 1;
        TZero(i) = (kw = kwl(FITS::TZERO, i + 1)) ? kw->asDouble() : 0;
    }
    _log << LogOrigin("MSFitsInput", "fillMSMainTable") << LogIO::DEBUG1
            << "TType=" << TType << "\nTScal=" << TScal << "\nTZero=" << TZero
            << LogIO::POST;

    Int nCorr = _nPixel(getIndex(_coordType, "STOKES"));
    Int nChan = _nPixel(getIndex(_coordType, "FREQ"));
    Int ns = max(1, _nIF);
    Int nrows = bt.nrows();

    long estStor = nrows * ns / 1024 * 
        (7 * 8 + 11 * 4 + (2 * nCorr + 3 * nCorr * nChan) * 4 + 
         nCorr * nChan + 1);
    float needS = estStor / 1024.;
    Directory curD(_msFile);
    float freeS = curD.freeSpaceInMB();
 
    _log << LogOrigin("MSFitsInput", "MSFitsInput")
           << ((needS > freeS) ? LogIO::WARN : LogIO::DEBUG1) 
           << "Estimate of Needed Storage Space in MB: " 
           << 0.9 * needS << "~" << 1.6 * needS 
           << "\n                       Free Space in MB: " << freeS 
           << LogIO::POST;

    Matrix<Complex> vis(nCorr, nChan);
    Vector<Float> sigma(nCorr);
    Matrix<Float> weightSpec(nCorr, nChan);
    Vector<Float> weight(nCorr);
    const Int nCat = 3;

    Vector<String> cat(nCat);
    cat(0) = "FLAG_CMD";
    cat(1) = "ORIGINAL";
    cat(2) = "USER";
    msc.flagCategory().rwKeywordSet().define("CATEGORY", cat);
    Cube<Bool> flagCat(nCorr, nChan, nCat, False);
    Matrix<Bool> flag = flagCat.xyPlane(0); // references flagCat's storage


    Int iU, iV, iW;
    iU = getIndexContains(TType, "UU");
    iV = getIndexContains(TType, "VV");
    iW = getIndexContains(TType, "WW");
    if (iU < 0 || iV < 0 || iW < 0) {
        throw(AipsError("MSFitsInput: Cannot find UVW information"));
    }

    Int iBsln = getIndex(TType, "BASELINE");
    Int iSubarr = getIndex(TType, "SUBARRAY");
    Int iAnt1 = getIndex(TType, "ANTENNA1");
    Int iAnt2 = getIndex(TType, "ANTENNA2");
    Int iTime0 = getIndex(TType, "DATE");
    Int iSource = getIndex(TType, "SOURCE");
    Int iFreq = getIndex(TType, "FREQSEL");
    Int iVis = getIndex(TType, "VISIBILITIES");

    _log << LogIO::NORMAL << "Fill MS Main Table of " << nrows
            << " rows uvfits visibility data " << LogIO::POST;
    Int row = -1;

    Double interval, exposure;
    interval = 0.0;
    exposure = 0.0;
    Bool discernIntExp(True);
    Double discernedInt(DBL_MAX);

    ProgressMeter meter(0.0, nrows * 1.0, "UVFITS Filler", "rows copied", "",
            "", True, nrows / 100);

    Vector<Double> uvw(3);

    // Remember last-filled values for TSM use
    Int lastFillArrayId, lastFillFieldId, lastFillScanNumber;
    lastFillArrayId = -1;
    lastFillFieldId = -1, lastFillScanNumber = 0;
    Double lastFillTime = 0;

    // Keep track of array-specific scanNumbers, FieldIds and FreqIds
    Vector<Int> scanNumber(1);
    Vector<Int> lastFieldId(1);
    Vector<Int> lastFreqId(1);
    scanNumber = 0;
    lastFieldId = -1;
    lastFreqId = -1;
    _nArray = -1;

    Bool lastRowFlag = False;
    for (Int group = 0; group < nrows; group++) {
        const Table tb = (group < 1) ? bt.thisRow() : bt.nextRow();
        try {
            ScalarColumn<Float> colDate(tb, TType(iTime0));
            ScalarColumn<Float> colUU(tb, TType(iU));
            ScalarColumn<Float> colVV(tb, TType(iV));
            ScalarColumn<Float> colWW(tb, TType(iW));
            ScalarColumn<Float> colBL;
            ScalarColumn<Float> colSubarr, colAnt1, colAnt2;
            ArrayColumn<Float> colVis(tb, TType(iVis));

            if (iBsln >= 0) {
              colBL = ScalarColumn<Float>(tb, TType(iBsln));
            } else {
              colSubarr = ScalarColumn<Float>(tb, TType(iSubarr));
              colAnt1 = ScalarColumn<Float>(tb, TType(iAnt1));
              colAnt2 = ScalarColumn<Float>(tb, TType(iAnt2));
            }

            Int visL = 1;
            for (uInt i = 0; i < _nPixel.nelements(); i++)
                visL *= _nPixel(i);
            Vector<Float> visib(visL);

            visib = colVis.getColumn();
            Double time = (colDate.asdouble(0) - 2400000.5) * C::day ;
            //Double time = colDate.asdouble(0);
            //Time tm(time);

            // Extract fqid
            Int freqId = 1;
            if (iFreq >= 0) {
                ScalarColumn<Float> colFrqSel(tb, TType(iFreq));
                freqId = (Int) colFrqSel.asfloat(0);
            }

            // Extract field Id
            Int fieldId = 0;
            if (iSource >= 0) {
                ScalarColumn<Float> colSU(tb, TType(iSource));
                // make 0-based
                fieldId = (Int) colSU.asfloat(0) - 1;
            }

            // Extract uvw
            uvw(0) = colUU.asdouble(0);
            uvw(1) = colVV.asdouble(0);
            uvw(2) = colWW.asdouble(0);
            // Convert from units of seconds to meters
            uvw *= C::c;

            // Extract array/baseline/antenna info
            Int arrayId;
            std::pair<Int, Int> ants;
            if (iBsln >= 0) {
                Float baseline = colBL.asfloat(0);
                ants = _extractAntennas(baseline);
                arrayId = Int(100.0 * (baseline - Int(baseline) + 0.001));
            } else {
                Int antenna1 = _priGroup.parm(iAnt1);
                Int antenna2 = _priGroup.parm(iAnt2);
                ants = _extractAntennas(antenna1, antenna2);
                arrayId = _priGroup.parm(iSubarr) - 1;
            }
            _nArray = max(_nArray, arrayId + 1);
            Int ant1 = ants.first;
            Int ant2 = ants.second;

            // Ensure arrayId-specific params are of correct length:
            if (scanNumber.shape() < _nArray) {
                scanNumber.resize(_nArray, True);
                lastFieldId.resize(_nArray, True);
                lastFreqId.resize(_nArray, True);
                scanNumber(_nArray - 1) = 0;
                lastFieldId(_nArray - 1) = -1;
                lastFreqId(_nArray - 1) = -1;
            }

            // Detect new scan (field or freqid change) for each arrayId
            if (fieldId != lastFieldId(arrayId) || freqId
                    != lastFreqId(arrayId) || time - lastFillTime > 300.0) {
                scanNumber(arrayId)++;
                lastFieldId(arrayId) = fieldId;
                lastFreqId(arrayId) = freqId;
            }

            // keep track of minimum which is the only one
            // (if time step is larger than UVFITS precision (and zero))
            discernIntExp = True;
            Double tempint;
            tempint = time - lastFillTime;
            if (tempint > 0.01) {
                discernedInt = min(discernedInt, tempint);
            }

            // Work out which axis increments fastests, pol or channel
            Bool polFastest = (getIndex(_coordType, "STOKES") < getIndex(
                    _coordType, "FREQ"));
            const Int nx = (polFastest ? nChan : nCorr);
            const Int ny = (polFastest ? nCorr : nChan);

            Int count = 0;
            for (Int ifno = 0; ifno < max(1, _nIF); ifno++) {
                // IFs go to separate rows in the MS
                _ms.addRow();
                row++;

                // fill in values for all the unused columns
                if (row == 0) {
                    msc.feed1().put(row, 0);
                    msc.feed2().put(row, 0);
                    msc.flagRow().put(row, False);
                    lastRowFlag = False;
                    msc.processorId().put(row, -1);
                    msc.observationId().put(row, 0);
                    msc.stateId().put(row, -1);
                }

                // Fill scanNumber if changed since last row
                if (scanNumber(arrayId) != lastFillScanNumber) {
                    msc.scanNumber().put(row, scanNumber(arrayId));
                    lastFillScanNumber = scanNumber(arrayId);
                }

                weight = 0.0;
                count = 0;
                // Loop over chans and corrs:
                for (Int ix = 0; ix < nx; ix++) {
                    for (Int iy = 0; iy < ny; iy++) {
                        const Float visReal = visib(count++);
                        const Float visImag = visib(count++);
                        const Float wt = visib(count++);
                        const Int pol = (polFastest ? _corrIndex[iy]
                                : _corrIndex[ix]);
                        const Int chan = (polFastest ? ix : iy);
                        if (wt <= 0.0) {
                            weightSpec(pol, chan) = abs(wt);
                            flag(pol, chan) = True;
                            weight(pol) += abs(wt);
                        } else {
                            weightSpec(pol, chan) = wt;
                            flag(pol, chan) = False;
                            weight(pol) += wt;
                        }
                        vis(pol, chan) = Complex(visReal, visImag);
                    }
                }

                // calculate sigma (weight = inverse variance)
                for (Int nc = 0; nc < nCorr; nc++) {
                    if (weight(nc) > 0.0) {
                        sigma(nc) = sqrt(1.0 / weight(nc));
                    } else {
                        sigma(nc) = 0.0;
                    }
                }

                // If available, store interval/exposure
                if (!discernIntExp) {
                    msc.interval().put(row, interval);
                    msc.exposure().put(row, exposure);
                }

                msc.data().put(row, vis);

                msc.weight().put(row, weight);
                msc.sigma().put(row, sigma);
                msc.weightSpectrum().put(row, weightSpec);

                msc.flag().put(row, flag);
                msc.flagCategory().put(row, flagCat);
                Bool rowFlag = allEQ(flag, True);
                if (rowFlag != lastRowFlag) {
                    msc.flagRow().put(row, rowFlag);
                    lastRowFlag = rowFlag;
                }

                if (arrayId != lastFillArrayId) {
                    msc.arrayId().put(row, arrayId);
                    lastFillArrayId = arrayId;
                }

                msc.antenna1().put(row, ant1);
                msc.antenna2().put(row, ant2);
                if (time != lastFillTime) {
                    msc.time().put(row, time);
                    msc.timeCentroid().put(row, time);
                    lastFillTime = time;
                }
                msc.uvw().put(row, uvw);

                // determine the spectralWindowId
                Int spW = ifno;
                if (iFreq >= 0) {
                    //spW = (Int) colFrqSel.asfloat(0) - 1; // make 0-based
                    spW = freqId - 1; // make 0-based
                    if (_nIF > 0) {
                        spW *= _nIF;
                        spW += ifno;
                    }
                }
                // nSpW = max(nSpW, spW + 1);

                // Always put DDI (SSM) since it might change rapidly
                msc.dataDescId().put(row, spW);

                // store the fieldId
                if (fieldId != lastFillFieldId) {
                    msc.fieldId().put(row, fieldId);
                    // nField = max(nField, fieldId + 1);
                    lastFillFieldId = fieldId;
                }

            }
            meter.update((group + 1) * 1.0);
        }
        catch (const std::exception& x) {
            _log << LogOrigin("MSFitsInput", "fillMSMainTable")
                    << "Exception while filling MS main table. " << x.what()
                    << LogIO::EXCEPTION;
        }

    }
    // If determining interval on-the-fly, fill interval/exposure columns
    //  now:
    if (discernIntExp) {
        discernedInt = floor(100.0 * discernedInt + 0.5) / 100.0;
        msc.interval().fillColumn(discernedInt);
        msc.exposure().fillColumn(discernedInt);
    }

    // fill the receptorAngle with defaults, just in case there is no AN table
    _receptorAngle.resize(2 * _nAntRow);
    _receptorAngle = 0;
    // set the Measure References

    if (iSource < 0) {
        double ra=0.;
        double dec=0.;
        ra = _refVal(getIndex(_coordType, "RA"));
        dec = _refVal(getIndex(_coordType, "DEC"));
        fillFieldTable(ra, dec, object);
    }
}

std::pair<Int, Int> MSFitsInput::_extractAntennas(Int ant1, Int ant2) {
    _nAntRow = max(_nAntRow, ant1);
    _nAntRow = max(_nAntRow, ant2);
    _uniqueAnts.insert(ant1);
    _uniqueAnts.insert(ant2);
    // make 0-based
    ant1--;
    ant2--;
    return make_pair(ant1, ant2);
}

std::pair<Int, Int> MSFitsInput::_extractAntennas(Float baseline) {
    Int ant1 = Int(baseline) / 256;
    Int ant2 = Int(baseline) - ant1 * 256;
    return _extractAntennas(ant1, ant2);
}

void MSFitsInput::fillObservationTable(ConstFitsKeywordList& kwl) {
    const FitsKeyword* kw;
    const Regex trailing(" *$"); // trailing blanks
    kwl.first();
    _ms.observation().addRow();
    String observer;
    observer = (kw = kwl(FITS::OBSERVER)) ? kw->asString() : "";
    observer = observer.before(trailing);
    MSObservationColumns msObsCol(_ms.observation());
    msObsCol.observer().put(0, observer);
    String telescope = (kw = kwl(FITS::TELESCOP)) ? kw->asString() : "unknown";
    telescope = telescope.before(trailing);
    if (telescope == "HATCREEK")
        telescope = "BIMA";
    String instrume = (kw = kwl(FITS::INSTRUME)) ? kw->asString() : "unknown";
    instrume = instrume.before(trailing);
    msObsCol.telescopeName().put(0, telescope);
    msObsCol.scheduleType().put(0, "");
    msObsCol.project().put(0, "");

    //Double epoch = (kw = kwl(FITS::EPOCH)) ? kw->asDouble() : 2000;

    String date;
    date = (kw = kwl(FITS::DATE_OBS)) ? kw->asString() : "";
    if (date == "")
        date = "2000-01-01";
    String date_map;
    date_map = (kw = kwl(FITS::DATE_MAP)) ? kw->asString() : "";
    MVTime timeVal, timeRel;
    MEpoch::Types epochRef;
    FITSDateUtil::fromFITS(timeVal, epochRef, date, "UTC");
    FITSDateUtil::fromFITS(timeRel, epochRef, date_map, "UTC");
    Vector<Double> times(2);
    times(0) = timeVal.second();
    times(1) = timeVal.second();
    _obsTime(0) = times(0);
    _obsTime(1) = times(1);

    msObsCol.timeRange().put(0, times);
    msObsCol.releaseDate().put(0, timeRel.second());
    msObsCol.flagRow().put(0, False);

}

void MSFitsInput::fillPointingTable() {
    // fill the pointing table.  run though the main table, find field changes,
    // and add pointing rows as needed by looking up the field info in the field table


    if (_addSourceTable)
        _log << LogOrigin("MSFitsInput", "fillPointingTable")
                << LogIO::NORMAL << "Filling Pointing table." << LogIO::POST;

    Int nrow = _ms.nrow();
    Int nAnt = _ms.antenna().nrow();
    Int lastFieldId = -1;

    Double lastTime = 0;
    Vector<Int> fieldId = _msc->fieldId().getColumn();
    Vector<Int> ddId;
    if (_addSourceTable)
        ddId = _msc->dataDescId().getColumn();

    ProgressMeter meter(0.0, nrow * 1.0, "UVFITS Filler", "rows copied", "",
            "", True, nrow / 100);

    for (Int i = 0; i < nrow; i++) {
        if (fieldId(i) != lastFieldId) {
            lastFieldId = fieldId(i);
            if (i > 0)
                lastTime = _msc->time()(i - 1);
            Array<Double> pointingDir = _msc->field().phaseDir()(lastFieldId);
            String name = _msc->field().name()(lastFieldId);
            Int numPoly = _msc->field().numPoly()(lastFieldId);
            Double time = _msc->time()(i);
            Int np = _ms.pointing().nrow();
            if (np > 0) {
                // fix up time and interval for previous entries
                Double midTime = (lastTime + _msc->pointing().time()(np - 1))
                        / 2;
                Double interval = lastTime - _msc->pointing().time()(np - 1)
                        + _msc->interval()(i - 1);
                for (Int j = 0; j < nAnt; j++) {
                    _msc->pointing().time().put(np - j - 1, midTime);
                    _msc->pointing().timeOrigin().put(np - j - 1, midTime);
                    _msc->pointing().interval().put(np - j - 1, interval);
                }
            }

            for (Int j = 0; j < nAnt; j++) {
                _ms.pointing().addRow();
                _msc->pointing().antennaId().put(np + j, j);
                if (j == 0) {
                    _msc->pointing().time().put(np + j, time);
                    _msc->pointing().timeOrigin().put(np + j, time);
                    _msc->pointing().interval().put(np + j, 0);
                    _msc->pointing().name().put(np + j, name);
                    _msc->pointing().numPoly().put(np + j, numPoly);
                    _msc->pointing().direction().put(np + j, pointingDir);
                    _msc->pointing().target().put(np + j, pointingDir);
                    _msc->pointing().tracking().put(np + j, True);
                }
            }

        }
        meter.update((i + 1) * 1.0);
    }

    // fix up last interval
    lastTime = _msc->time()(nrow - 1);
    Int np = _ms.pointing().nrow();
    if (np > 0) {
        // fix up time and interval for previous entries
        Double midTime = (lastTime + _msc->pointing().time()(np - 1)) / 2;
        Double interval = lastTime - _msc->pointing().time()(np - 1)
                + _msc->interval()(nrow - 1);
        for (Int j = 0; j < nAnt; j++) {
            _msc->pointing().time().put(np - j - 1, midTime);
            _msc->pointing().timeOrigin().put(np - j - 1, midTime);
            _msc->pointing().interval().put(np - j - 1, interval);
        }
    }
}

void MSFitsInput::fillSourceTable() {

    _log << LogOrigin("MSFitsInput", "fillSourceTable") << LogIO::NORMAL
            << "Filling SOURCE table." << LogIO::POST;
    Int numRow = 1;
    if (numRow > 0) {
        String tName = _ms.tableName();
        MSSummary mss(&_ms, tName);

        Record mainRec;
        mss.listMain(_log, mainRec);

        //Record fieldRec;
        //mss.listField(_log, fieldRec, True);
        ProgressMeter meter(0.0, mainRec.nfields() * 1.0, "UVFITS Filler",
                "rows copied", "", "", True, mainRec.nfields() * 300 / 100);

        for (uInt i = 0; i < mainRec.nfields() - 5; i++) {
            Int fnum = mainRec.fieldNumber(String("scan_").append(
                    String::toString(i + 1)));
            Record rec = mainRec.subRecord(fnum).subRecord(String('0'));
            Double time1 = rec.asDouble("BeginTime");
            Double time2 = rec.asDouble("IntegrationTime");
            Int fid = rec.asInt("FieldId");
            String name = rec.asString("FieldName");

            //Int numPoly = ;
            Vector<Int> spwIds = rec.asArrayInt("SpwIds");
            for (uInt spwId = 0; spwId < spwIds.nelements(); spwId++) {
                MSSourceIndex sourceIndex(_ms.source());
                sourceIndex.sourceId() = fid;
                sourceIndex.spectralWindowId() = spwIds(spwId);

                Vector<rownr_t> rows = sourceIndex.getRowNumbers();
                if (rows.nelements() == 0) {
                    _ms.source().addRow();
                    Int j = _ms.source().nrow() - 1;
                    MSSourceColumns & msc = _msc->source();
                    msc.sourceId().put(j, fid);
                    msc.name().put(j, name);
                    Matrix<Double> phaseDir = _msc->field().phaseDir()(fid);
                    Vector<Double> srcDir = phaseDir.column(0);
                    Vector<Double> rate(2);
                    if (phaseDir.ncolumn() > 1)
                        rate = phaseDir.column(1);
                    else
                        rate = 0.0;
                    msc.direction().put(j, srcDir);
                    msc.properMotion().put(j, rate);
                    msc.time().put(j, time1 * C::day);
                    msc.interval().put(j,time2);
                    msc.spectralWindowId().put(j, spwId);
                    Vector<Double> sysVel(1);
                    sysVel(0) = 0.;
                    msc.sysvel().put(j, sysVel);
                    msc.numLines().put(j, 1);
                    Vector<String> transition(1);
                    transition(0) = "";
                    msc.transition().put(j, transition);
                    Vector<Double> restFreqs(1);
                    restFreqs(0) = _restfreq;
                    if (restFreqs(0) <= 0.0) {
                        restFreqs(0) = _msc->spectralWindow().refFrequency()(
                                spwId);
                    }
                    msc.restFrequency().put(j, restFreqs);
                    msc.calibrationGroup().put(j, -1);
                    //String code = _msc->field().c.code().asString();
                    msc.code().put(j, _msc->field().code()(fid));

                }
                //meter.update((i + 1) * 1.0);
            }
        }
    }
    else {

        //////////////////this is uselessly slow
        // fill the source table. run though the main table look for new spectralwindows
        // and add source table entries for each field/spw combination
        Int nrow = _ms.nrow();
        Int lastFieldId = -1;
        Int lastDDId = -1;
        Vector<Int> fieldId = _msc->fieldId().getColumn();
        Vector<Int> ddId = _msc->dataDescId().getColumn();

        ProgressMeter meter(0.0, nrow * 1.0, "UVFITS Filler", "rows copied",
                "", "", True, nrow / 100);

        for (Int i = 0; i < nrow; i++) {
            if (fieldId(i) != lastFieldId || (ddId(i) != lastDDId)) {
                lastFieldId = fieldId(i);
                Array<Double> pointingDir = _msc->field().phaseDir()(
                        lastFieldId);
                String name = _msc->field().name()(lastFieldId);
                //Int numPoly = _msc->field().numPoly()(lastFieldId);
                Double time = _msc->time()(i);

                lastDDId = ddId(i);
                Int spwId = _msc->dataDescription().spectralWindowId()(
                        lastDDId);
                // now check if we've seen this field for this spectral window
                // Use indexed access to the SOURCE sub-table
                MSSourceIndex sourceIndex(_ms.source());
                sourceIndex.sourceId() = lastFieldId;
                sourceIndex.spectralWindowId() = spwId;
                Vector<rownr_t> rows = sourceIndex.getRowNumbers();
                if (rows.nelements() == 0) {
                    _ms.source().addRow();
                    Int j = _ms.source().nrow() - 1;
                    MSSourceColumns & mss = _msc->source();
                    mss.sourceId().put(j, lastFieldId);
                    _msc->field().sourceId().put(lastFieldId, lastFieldId);
                    mss.name().put(j, name);
                    Matrix<Double> phaseDir = _msc->field().phaseDir()(
                            lastFieldId);
                    Vector<Double> srcDir = phaseDir.column(0), rate(2);
                    if (phaseDir.ncolumn() > 1)
                        rate = phaseDir.column(1);
                    else
                        rate = 0.0;
                    mss.direction().put(j, srcDir);
                    mss.properMotion().put(j, rate);
                    mss.time().put(j, time);
                    mss.interval().put(j, DBL_MAX);
                    mss.spectralWindowId().put(j, spwId);
                    Vector<Double> sysVel(1);
                    sysVel(0) = 0.;
                    mss.sysvel().put(j, sysVel);
                    mss.numLines().put(j, 1);
                    Vector<String> transition(1);
                    transition(0) = "";
                    mss.transition().put(j, transition);
                    Vector<Double> restFreqs(1);
                    restFreqs(0) = _restfreq;
                    if (restFreqs(0) <= 0.0) {
                        // put in the reference freq as default for the rest frequency
                        restFreqs(0) = _msc->spectralWindow().refFrequency()(
                                spwId);
                    }
                    mss.restFrequency().put(j, restFreqs);
                    mss.calibrationGroup().put(j, -1);

                }
            }
            meter.update((i + 1) * 1.0);
        }
        //////////////////this is uselessly slow
    }

}

void MSFitsInput::fillFieldTable(BinaryTable& bt) {
    Int nField = bt.nrows();

    TableRecord btKeywords = bt.getKeywords();
    if (!btKeywords.isDefined("NO_IF")) {
        throw(AipsError("MSFitsInput: Illegal SU file: no number of IFs"));
    }
    uInt noif = bt.getKeywords().asuInt("NO_IF");

    MSFieldColumns& msField(_msc->field());
    // Table suTab=bt.fullTable("",Table::Scratch);
    Table suTab = bt.fullTable();
    ScalarColumn<Int> id(suTab, "ID. NO.");
    ScalarColumn<String> name(suTab, "SOURCE");
    ScalarColumn<Int> qual(suTab, "QUAL");
    Bool multiqual = False;
    Int minqual, maxqual;
    minMax(minqual, maxqual, qual.getColumn());
    if (minqual != maxqual)
        multiqual = True;
    ScalarColumn<String> code(suTab, "CALCODE");
    // ScalarColumn<Float> iflux(suTab,"IFLUX"); // etc Q, U, V (Jy)
    ScalarColumn<Double> ra(suTab, "RAEPO"); //degrees
    ScalarColumn<Double> dec(suTab, "DECEPO"); //degrees
    ScalarColumn<Double> raapp(suTab, "RAAPP"); //degrees
    ScalarColumn<Double> decapp(suTab, "DECAPP"); //degrees
    ScalarColumn<Double> epoch(suTab, "EPOCH"); //years
    ScalarColumn<Double> pmra(suTab, "PMRA"); //deg/day
    ScalarColumn<Double> pmdec(suTab, "PMDEC"); //deg/day
    if (Int(suTab.nrow()) < nField) {
        _log << LogOrigin("MSFitsInput", __func__)
               << LogIO::NORMAL
                << "Input Source id's not sequential, adding empty rows in output"
                << LogIO::POST;
    }
    Int outRow = -1;

    // RESTFREQ and LSRVEL are 2D columns according to the AIPS Memo 117
    _restFreq.resize(noif, suTab.nrow());
    _sysVel.resize(noif, suTab.nrow());
    try{
      ArrayColumn<Double> restfreq(suTab,"RESTFREQ");  // Hz
      ArrayColumn<Double> sysvel(suTab,"LSRVEL"); // m/s
      restfreq.getColumn(_restFreq);
      sysvel.getColumn(_sysVel);
    }
    catch (std::exception& x) {
      if(noif>1){
	_log << LogOrigin("MSFitsInput", __func__) << LogIO::WARN
	       << x.what() << ": " << "Inconsistent setup of RESTFREQ and LSRVEL columns." << endl
	       << "With NO_IF>1, they should be arrays not scalars." << LogIO::POST;
      }
      ScalarColumn<Double> restfreq(suTab,"RESTFREQ");  // Hz
      ScalarColumn<Double> sysvel(suTab,"LSRVEL"); // m/s
      Vector<Double> tmprf(suTab.nrow());
      Vector<Double> tmpsv(suTab.nrow());
      restfreq.getColumn(tmprf);
      sysvel.getColumn(tmpsv);
      for(uInt ii=0; ii<suTab.nrow(); ii++){
	_restFreq(0,ii) = tmprf(ii);
	_sysVel(0,ii) = tmpsv(ii);
      }
    }      

    // set the DIRECTION MEASURE REFERENCE for appropriate columns
    MDirection::Types epochRefZero = getDirectionFrame(epoch(0));
    if (epochRefZero != _epochRef)
        _log << LogOrigin("MSFitsInput", __func__)
               << LogIO::WARN << "The direction measure reference code, "
                << epochRefZero << "\n"
                << "for the first field does not match the one from the FITS header, "
                << _epochRef
                << ".\nThis might cause a problem for the reference frame"
                << " of the output's UVW column." << LogIO::POST;

    for (Int inRow = 0; inRow < (Int) suTab.nrow(); inRow++) {
        Int fld = id(inRow) - 1;
        // add empty rows until the row number in the output matches the source id
        while (fld > outRow) {
            // Append a flagged, empty row to the FIELD table
            _ms.field().addRow();
            outRow++;
            Vector<MDirection> nullDir(1);
            nullDir(0).set(MVDirection(0.0, 0.0), MDirection::Ref(epochRefZero));
            msField.phaseDirMeasCol().put(outRow, nullDir);
            msField.delayDirMeasCol().put(outRow, nullDir);
            msField.referenceDirMeasCol().put(outRow, nullDir);
            msField.flagRow().put(outRow, True);
        }
        msField.sourceId().put(fld, fld); 
        msField.code().put(fld, code(inRow));
        String theFldName;
        if (multiqual)
            theFldName = name(inRow) + "_" + String::toString(qual(inRow));
        else
            theFldName = name(inRow);
        msField.name().put(fld, theFldName);
        Int numPoly = 0;
        if (!nearAbs(pmra(inRow), 0.0) || !nearAbs(pmdec(inRow), 0.0)) {
            numPoly = 1;
        }
        // The code below will write the direction in B1950 or J2000 coordinates if
        // the direction is constant. However it will use apparent Coordinates (I
        // am not sure if this means APP, JTRUE, BTRUE or what), if the proper
        // motion is non-zero.  If the epoch in the incoming SU
        // table is "-1" (via AIPS UVFITS, a planet tracked by the correlator), it
        // will adopt the epochRefZero and use the ra/dec (not raapp/decapp).
        // The handling of planets should be cleaned up (defect 3636).
        // In all cases the time will be the date of the start of the observation.
        MDirection::Types epochRef = MDirection::APP;
        MVDirection refDir;

        if (numPoly == 0) {
            if (near(epoch(inRow), 2000.0, 0.01)) {
                epochRef = MDirection::J2000;
            } else if (nearAbs(epoch(inRow), 1950.0, 0.01)) {
                if (_array == "VLA")
                    epochRef = MDirection::B1950_VLA;
                else
                    epochRef = MDirection::B1950;
            } else if (epoch(inRow) == -1.0) {
                epochRef = epochRefZero;
                _log << LogOrigin("MSFitsInput", __func__)
                       << " Assuming standard epoch " << " for " << name(inRow)
                        << ".  Be aware that this may not be correct." << endl;
            } else {
                _log << LogOrigin("MSFitsInput", __func__)
                       << " Cannot handle epoch in SU table: " << epoch(inRow)
                        << LogIO::EXCEPTION;
            }
            refDir = MVDirection(ra(inRow) * C::degree, dec(inRow) * C::degree);

        } else {
            refDir = MVDirection(raapp(inRow) * C::degree, decapp(inRow)
                    * C::degree);
        }
        Vector<MDirection> radecMeas(numPoly + 1);
        radecMeas(0).set(refDir, MDirection::Ref(epochRef));
        if (numPoly == 1) {
            radecMeas(1).set(MVDirection(pmra(inRow) * C::degree / C::day,
                    pmdec(inRow) * C::degree / C::day), MDirection::Ref(
                    epochRef));
        }

        msField.time().put(fld, _obsTime(0));
        msField.numPoly().put(fld, numPoly);
        msField.delayDirMeasCol().put(fld, radecMeas);
        msField.phaseDirMeasCol().put(fld, radecMeas);
        msField.referenceDirMeasCol().put(fld, radecMeas);
        msField.flagRow().put(fld, False);
    }
}

void MSFitsInput::fillFieldTable(double ra, double dec, String source) {
    MSFieldColumns& msField(_msc->field());

    _ms.field().addRow();

    msField.sourceId().put(0, 0); 
    msField.code().put(0, "");
    msField.name().put(0, source);
    Int numPoly = 0;

    //MDirection::Types epochRef = MDirection::APP;
    MDirection::Types epochRef = _epochRef;
    MVDirection refDir;

    refDir = MVDirection(ra * C::degree, dec * C::degree);
    Vector<MDirection> radecMeas(1);
    radecMeas(0).set(refDir, MDirection::Ref(epochRef));

    msField.time().put(0, _obsTime(0));
    msField.numPoly().put(0, numPoly);
    msField.delayDirMeasCol().put(0, radecMeas);
    msField.phaseDirMeasCol().put(0, radecMeas);
    msField.referenceDirMeasCol().put(0, radecMeas);
    msField.flagRow().put(0, False);

}
        

} //# NAMESPACE CASACORE - END

