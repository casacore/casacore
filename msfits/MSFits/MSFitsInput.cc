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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$
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
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/Tables/Table.h>
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
    infile_p(0), msc_p(0), addSourceTable_p(False), itsLog(LogOrigin(
            "MSFitsInput", "MSFitsInput")), newNameStyle(useNewStyle) {
    // First, lets verify that fitsfile exists and that it appears to be a
    // FITS file.
    File f(fitsFile);
    if (!f.exists() || !f.isReadable()) {
        itsLog << LogOrigin("MSFitsInput", "MSFitsInput")
               << "File " << fitsFile << " does not exist or is not readable"
                << LogIO::EXCEPTION;
    }
    // First attempt at validating that it's a FITS file
    if (!f.isRegular()) {
        itsLog << LogOrigin("MSFitsInput", "MSFitsInput")
               << "File " << fitsFile
                << " is not a plain file (maybe a directory?)"
                << LogIO::EXCEPTION;
    }
    // We should probably look for SIMPLE = here

    String errmsg;
    NewFile fileOK(True);
    if (!fileOK.valueOK(msFile, errmsg)) {
        itsLog << LogOrigin("MSFitsInput", "MSFitsInput")
               << "Error in output file: " << errmsg << LogIO::EXCEPTION;
    }

    msFile_p = msFile;

    itsLog << LogOrigin("MSFitsInput", "MSFitsInput")
           << LogIO::NORMAL << "Converting FITS file '" << fitsFile
            << "' to MeasurementSet '" << msFile << "'" << LogIO::POST;

    // Open the FITS file for reading
    infile_p = new FitsInput(fitsFile.chars(), FITS::Disk);
    //cout << "++infile_p->hdutype()=" << infile_p->hdutype() << endl;

    obsTime.resize(2);
    MVTime timeVal;
    MEpoch::Types epochRef;
    FITSDateUtil::fromFITS(timeVal, epochRef, "2000-01-01", "UTC");
    obsTime(0) = timeVal.second();
    obsTime(1) = timeVal.second();

    if (infile_p) {
        if (infile_p->err() == FitsIO::IOERR) {
            itsLog << LogOrigin("MSFitsInput", "MSFitsInput")
                   << "Failed to read file " << fitsFile << LogIO::EXCEPTION;
        } else if (infile_p->err()) {
            itsLog << LogOrigin("MSFitsInput", "MSFitsInput")
                   << "Failed to read initial record -- exiting."
                    << LogIO::EXCEPTION;
        } else {

            if (checkInput(*infile_p)) {
                if (infile_p->hdutype() == FITS::PrimaryGroupHDU) {
                    priGroup_p.attach(*infile_p);
                    //cout << "==infile_p->hdutype()=" << infile_p->hdutype() << endl;
                }
                if (infile_p->hdutype() == FITS::PrimaryTableHDU) {
                    priTable_p.attach(*infile_p);
                    //cout << "--infile_p->hdutype()=" << infile_p->hdutype() << endl;
                }
            }
        }
    } else {
        itsLog << LogOrigin("MSFitsInput", "MSFitsInput")
               << "Failed to open fits file " << fitsFile << LogIO::EXCEPTION;
    }
}

void MSFitsInput::readRandomGroupUVFits(Int obsType) {
        itsLog << LogOrigin("MSFitsInput", "readRandomGroupUVFits")
               << LogIO::POST;
        Int nField = 0, nSpW = 0;

        useAltrval = False;
        getPrimaryGroupAxisInfo();

        Bool useTSM = True;

        setupMeasurementSet(msFile_p, useTSM, obsType);

        // fill the OBSERVATION table
        fillObsTables();

        Int totMem = HostInfo::memoryTotal();

        // 8 bytes per complex number and the other data like flag, weight is
        // is 1/2 of the total
        Int estMem = priGroup_p.gcount() * max(1, nIF_p) / 1024 * nPixel_p(
                getIndex(coordType_p, "STOKES")) * nPixel_p(getIndex(
                coordType_p, "FREQ")) * 8 * 2;
        
        Int ns = max(1, nIF_p);
        Int nc = nPixel_p( getIndex(coordType_p, "STOKES"));
        Int nf = nPixel_p( getIndex(coordType_p, "FREQ"));
        //cout << "priGroup_p.gcount()=" << priGroup_p.gcount()
        //     << " ns=" << ns << " nc=" << nc << " nf=" << nf << endl;
        long estStor = priGroup_p.gcount() * ns / 1024 * 
             (7 * 8 + 11 * 4 + (2 * nc + 3 * nc * nf) * 4 + nc * nf + 1);
        float needS = estStor / 1024. ;
        //cout << "=========msFile_p=" << msFile_p 
        //     << " estStor=" << estStor << endl;
        Directory curD(msFile_p);
        float freeS = curD.freeSpaceInMB();
 
        itsLog << LogOrigin("MSFitsInput", "readRandomGroupUVFits")
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
            catch(AipsError ex) {
                itsLog << LogOrigin("MSFitsInput", "readRandomGroupUVFits")
                   << ex.getMesg() 
                   << LogIO::EXCEPTION;
            }
        } else {
            //else fill row wise
            try {
                fillMSMainTable(nField, nSpW);
            }
            catch(AipsError ex) {
                itsLog << LogOrigin("MSFitsInput", "readRandomGroupUVFits")
                   << ex.getMesg() 
                   << LogIO::EXCEPTION;
            }
        }
        // now handle the BinaryTable extensions for the subtables
        Bool haveAn = False, haveField = False, haveSpW = False;

        while (infile_p->rectype() != FITS::EndOfFile && !infile_p->err()) {
            if (infile_p->hdutype() != FITS::BinaryTableHDU) {
                itsLog << LogOrigin("MSFitsInput", "readRandomGroupUVFits")
                       << LogIO::NORMAL << "Skipping unhandled extension"
                       << LogIO::POST;
                infile_p->skip_hdu();
            } else {
                BinaryTable binTab(*infile_p);
                // see if we can recognize the type
                String type = binTab.extname();
                itsLog << LogOrigin("MSFitsInput", "readRandomGroupUVFits")
                       << LogIO::DEBUG1 << "Found binary table of type "
                       << type << " following data" << LogIO::POST;

                itsLog << LogOrigin("MSFitsInput", "readRandomGroupUVFits")
                       << LogIO::NORMAL
                       << "extname=" << type << " nrows=" << binTab.nrows()
                       << " ncols=" << binTab.ncols() << " rowsize=" << binTab.rowsize() 
                       << " pcount=" << binTab.pcount() << " gcount=" << binTab.gcount()
                       << LogIO::POST;
                
                if (type.contains("AN") && !haveAn) {
                    haveAn = True;
                    fillAntennaTable(binTab);
                } else if (type.contains("FQ") && !haveSpW) {
                    haveSpW = True;
                    fillSpectralWindowTable(binTab, nSpW);

                } else if (type.contains("SU") && !haveField) {
                    haveField = True;
                    fillFieldTable(binTab, nField);
                    setFreqFrameVar(binTab);
                    //in case spectral window was already filled
                    if (haveSpW) {
                        updateSpectralWindowTable();
                    }
                } else {
                    itsLog << LogOrigin("MSFitsInput", "readRandomGroupUVFits")
                           << LogIO::NORMAL
                           << "Skipping table, duplicate or unrecognized type: "
                           << type << LogIO::POST;
                    //    binTab.fullTable("", Table::Scratch); // infile.skip_hdu();
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

        //fillPointingTable();
        //fillSourceTable();


        fixEpochReferences();

        if (!haveAn) {
            itsLog << LogOrigin("MSFitsInput", "readRandomGroupUVFits")
                   << "Cannot find an AN Table. This is required."
                   << LogIO::EXCEPTION;
        }
        fillFeedTable();
}

void MSFitsInput::readPrimaryTableUVFits(Int obsType) {
    itsLog << LogOrigin("MSFitsInput", "readPrimaryTableUVFits") 
           << "msFile_p=" << msFile_p
           << "obsType=" << obsType
           << LogIO::POST;
           
    useAltrval = False;

    Bool useTSM = False;

    epochRef_p = getDirectionFrame(2000.0);
    setupMeasurementSet(msFile_p, useTSM, obsType);
    //cout << "now processing the tables....." << endl;

    ConstFitsKeywordList kwlist = priTable_p.kwlist();

    const FitsKeyword* kw;
    kwlist.first();

    //this date is not source observation time!
    String date = "2000-01-01";
    date = (kw = kwlist(FITS::DATE)) ? kw->asString() : date;

    MVTime timeVal;
    MEpoch::Types epochRef;
    FITSDateUtil::fromFITS(timeVal, epochRef, date, "UTC");
    obsTime(0) = timeVal.second();
    obsTime(1) = timeVal.second();
    //cout << "timeVal=" << timeVal << endl;
    //cout << "obstime=" << obsTime(0) << " " << obsTime(1) << endl;

    fillHistoryTable(kwlist);
    Bool moreToDo = true;
    while (moreToDo &&
           infile_p->rectype() != FITS::EndOfFile && !infile_p->err()) { 
        if (//infile_p->rectype() != FITS::HDURecord ||
            infile_p->hdutype() != FITS::BinaryTableHDU) {
            itsLog << LogOrigin("MSFitsInput", "readPrimaryTableUVFits")
                   << LogIO::NORMAL << "Skipping unhandled extension"
                   << LogIO::POST;
            //cout << "infile_p->err()=" << infile_p->err() << endl;
            infile_p->skip_hdu();
            //cout << "infile_p->err()=" << infile_p->err() << endl; 
        } 
        else {
            //cout << "rectype=" << infile_p->rectype() << endl;
            //cout << "rectype FITS::HDURecord=" << FITS::HDURecord << endl;
            //cout << "Data type is " << infile_p->datatype() << endl;
            itsLog << LogOrigin("MSFitsInput", "readPrimaryTableUVFits")
                   << LogIO::DEBUG1 << "Binary Table HDU ------>>>" << LogIO::POST;

            BinaryTable* fqTab = 0;
            while (moreToDo &&
                   infile_p->hdutype() == FITS::BinaryTableHDU) {
                itsLog << LogOrigin("MSFitsInput", "readPrimaryTableUVFits")
                       << LogIO::DEBUG1
                       << "Found binary table of type "
                       << infile_p->rectype() << " following data"
                       << LogIO::POST;

                BinaryTable* bt = new BinaryTable(*infile_p);
                String type = bt->extname();

                itsLog << LogOrigin("MSFitsInput", "readPrimaryTableUVFits")
                       << LogIO::NORMAL
                       << "extname=" << bt->extname() << " nrows=" << bt->nrows()
                       << " ncols=" << bt->ncols() << " rowsize=" << bt->rowsize() 
                       //<< " pcount=" << bt->pcount() << " gcount=" << bt->gcount()
                       << LogIO::POST;

                if (type.contains("AN")) {
                    nAnt_p = bt->nrows();
                    fillAntennaTable(*bt);
                } 
                else if (type.contains("FQ")) {
                    //Int nSpW = bt->nrows();
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
                else if (type.contains("UV")) {
                    //showBinaryTable(*bt);
                    //fillOtherUVTables(*bt, *fqTab);
                    //TableRecord btKeywords = bt.getKeywords();

                    ConstFitsKeywordList kwl = bt->kwlist();

                    //FitsKeywordList pkw = kwl;
                    //cout << "UV table keywords:\n" << pkw << endl;

                    fillObservationTable(kwl);
                    fillHistoryTable(kwl);

                    getAxisInfo(kwl);
                    sortPolarizations();
                    fillPolarizationTable();
                    fillSpectralWindowTable(*fqTab);

                    try {
                        fillMSMainTable(*bt);
                    }
                    catch(AipsError ex) {
                        itsLog << LogOrigin("MSFitsInput", "readPrimaryTableUVFits")
                               << ex.getMesg() 
                               << LogIO::EXCEPTION;
                    }
                    //fillPointingTable();
                    fillSourceTable();
                    fillFeedTable();

                    moreToDo = false;
        }
                else {
                    //bt->fullTable();
                    //infile_p->skip_all(FITS::BinaryTableHDU);
                    infile_p->skip_hdu();
                    itsLog << LogOrigin("MSFitsInput", "readPrimaryTableUVFits")
                           << LogIO::NORMAL << "skip " << type << LogIO::POST;
                }
                itsLog << LogOrigin("MSFitsInput", "readPrimaryTable")
                       << LogIO::DEBUG1 << "<<<------ Binary Table HDU" << LogIO::POST;
            }

            //fill source table
        }
    }


}

void MSFitsInput::readFitsFile(Int obsType) {
    itsLog << LogOrigin("MSFitsInput", "readFitsFile")
           << LogIO::DEBUG2
           << "hdutype=" << infile_p->hdutype()
           << LogIO::POST;
    if (infile_p->hdutype() == FITS::PrimaryGroupHDU) {
        try {
            readRandomGroupUVFits(obsType);
        }
        catch(AipsError ex) {
           itsLog << LogOrigin("MSFitsInput", "readFitsFile")
                  << ex.getMesg() 
                  << LogIO::EXCEPTION;
        }
    } else if (infile_p->hdutype() == FITS::PrimaryTableHDU) {
        //itsLog << LogOrigin("MSFitsInput", "readFitsFile")
        //   << LogIO::SEVERE
        //  << "Primary Table uvfits not yet handled!"
        //   << LogIO::EXCEPTION;
        try {
            readPrimaryTableUVFits(obsType);
        }
        catch(AipsError ex) {
           itsLog << LogOrigin("MSFitsInput", "readFitsFile")
                  << ex.getMesg() 
                  << LogIO::EXCEPTION;
        }
    } 
    else {
        itsLog << LogOrigin("MSFitsInput", "readFitsFile")
           << LogIO::SEVERE << "unhandled extension type! "
           << LogIO::EXCEPTION;
    }
}

MSFitsInput::~MSFitsInput() {
    delete infile_p;
    delete msc_p;
}

Bool MSFitsInput::checkInput(FitsInput& infile) {
    // Check that we have a valid UV fits file
    if (infile.rectype() != FITS::HDURecord) {
        itsLog << LogOrigin("MSFitsInput", "checkInput")
               << "file does not start with standard hdu record."
               << LogIO::EXCEPTION;
    }
    itsLog << LogOrigin("MSFitsInput", "checkInput")
           << LogIO::DEBUG1
           << "infile.hdutype(): " << infile.hdutype() 
           << LogIO::POST;
    
    //visibilty must be one of these type
    if (infile.hdutype() != FITS::PrimaryGroupHDU &&
        infile.hdutype() != FITS::PrimaryArrayHDU &&
         infile.hdutype() != FITS::PrimaryTableHDU) {
        itsLog << LogOrigin("MSFitsInput", "checkInput")
               << "Error, neither primary group nor primary table"
               << LogIO::EXCEPTION;
    }
    FITS::ValueType dataType = infile.datatype();
    //cout << "dataType=" << dataType
    //     << " FITS::FLOAT=" << FITS::FLOAT
    //     << " FITS::SHORT=" << FITS::SHORT
    //     << " FITS::LONG=" << FITS::LONG
    //     << " FITS::BYTE=" << FITS::BYTE << endl;
    if (dataType != FITS::FLOAT &&
         dataType != FITS::SHORT &&
         dataType != FITS::LONG &&
         dataType != FITS::BYTE) {
        itsLog << LogOrigin("MSFitsInput", "checkInput")
               << "Error, this class handles only FLOAT, SHORT, LONG and BYTE data "
               << "(BITPIX=-32,16,32,8) at present" << LogIO::EXCEPTION;
    }
    return True;
}

void MSFitsInput::getPrimaryGroupAxisInfo() {
    itsLog << LogOrigin("MSFitsInput", "getPrimaryGroupAxisInfo");
    // Extracts the axis related info. from the PrimaryGroup object and
    // returns them in the form of arrays.
    const Regex trailing(" *$"); // trailing blanks
    const Int nAxis = priGroup_p.dims();
    if (nAxis < 1) {
        itsLog << "Data has no axes!" << LogIO::EXCEPTION;
    }
    nPixel_p.resize(nAxis);
    refVal_p.resize(nAxis);
    refPix_p.resize(nAxis);
    delta_p.resize(nAxis);
    coordType_p.resize(nAxis);
    for (Int i = 0; i < nAxis; i++) {
        nPixel_p(i) = priGroup_p.dim(i);
        if (nPixel_p(i) < 0) {
            itsLog << "Axes " << i << " cannot have a negative value"
                    << LogIO::EXCEPTION;
        }
        coordType_p(i) = priGroup_p.ctype(i);
        coordType_p(i) = coordType_p(i).before(trailing);
        refVal_p(i) = static_cast<Double> (priGroup_p.crval(i));
        refPix_p(i) = static_cast<Double> (priGroup_p.crpix(i));
        delta_p(i) = static_cast<Double> (priGroup_p.cdelt(i));
    }

    // Check if required axes are there
    if (getIndex(coordType_p, "COMPLEX") < 0) {
        itsLog << "Data does not have a COMPLEX axis" << LogIO::EXCEPTION;
    }
    if (getIndex(coordType_p, "STOKES") < 0) {
        itsLog << "Data does not have a STOKES axis" << LogIO::EXCEPTION;
    }
    if (getIndex(coordType_p, "FREQ") < 0) {
        itsLog << "Data does not have a FREQ axis" << LogIO::EXCEPTION;
    }
    if ((getIndex(coordType_p, "RA") < 0) && (getIndex(coordType_p, "RA---SIN")
            < 0) && (getIndex(coordType_p, "RA---NCP") < 0) && (getIndex(
            coordType_p, "RA---SCP") < 0)) {
        itsLog << "Data does not have a RA axis" << LogIO::EXCEPTION;
    }
    if ((getIndex(coordType_p, "DEC") < 0)
            && (getIndex(coordType_p, "DEC--SIN") < 0) && (getIndex(
            coordType_p, "DEC--NCP") < 0) && (getIndex(coordType_p, "DEC--SCP")
            < 0)) {
        itsLog << "Data does not have a DEC axis" << LogIO::EXCEPTION;
    }

    // Sort out the order of the polarizations and find the sort indices
    // to put them in 'standard' order: PP,PQ,QP,QQ
    const uInt iPol = getIndex(coordType_p, "STOKES");
    const uInt numCorr = nPixel_p(iPol);
    corrType_p.resize(numCorr);
    for (uInt i = 0; i < numCorr; i++) {
        // note: 1-based ref pix
        corrType_p(i) = ifloor(refVal_p(iPol) + (i + 1 - refPix_p(iPol))
                * delta_p(iPol) + 0.5);
        // convert AIPS-convention Stokes description to Casacore enum
        switch (corrType_p(i)) {
        case -8:
            corrType_p(i) = Stokes::YX;
            break;
        case -7:
            corrType_p(i) = Stokes::XY;
            break;
        case -6:
            corrType_p(i) = Stokes::YY;
            break;
        case -5:
            corrType_p(i) = Stokes::XX;
            break;
        case -4:
            corrType_p(i) = Stokes::LR;
            break;
        case -3:
            corrType_p(i) = Stokes::RL;
            break;
        case -2:
            corrType_p(i) = Stokes::LL;
            break;
        case -1:
            corrType_p(i) = Stokes::RR;
            break;
        case 4:
            corrType_p(i) = Stokes::V;
            break;
        case 3:
            corrType_p(i) = Stokes::U;
            break;
        case 2:
            corrType_p(i) = Stokes::Q;
            break;
        case 1:
            corrType_p(i) = Stokes::I;
            break;
        default:
            if (corrType_p(i) < 0) {
                itsLog << "Unknown Correlation type: " << corrType_p(i)
                        << LogIO::EXCEPTION;
            }
        }
    }
    Vector<Int> tmp(corrType_p.copy());
    // Sort the polarizations to standard order. Could probably use
    // GenSortIndirect here.
    GenSort<Int>::sort(corrType_p);
    corrIndex_p.resize(numCorr);
    // Get the sort indices to rearrange the data to standard order
    for (uInt i = 0; i < numCorr; i++) {
        for (uInt j = 0; j < numCorr; j++) {
            if (corrType_p(j) == tmp(i))
                corrIndex_p[i] = j;
        }
    }

    // Figure out the correlation products from the polarizations
    corrProduct_p.resize(2, numCorr);
    corrProduct_p = 0;
    for (uInt i = 0; i < numCorr; i++) {
        const Stokes::StokesTypes cType = Stokes::type(corrType_p(i));
        Fallible<Int> receptor = Stokes::receptor1(cType);
        Bool warn = False;
        if (receptor.isValid()) {
            corrProduct_p(0, i) = receptor;
        } else if (!warn) {
            warn = True;
            itsLog << LogIO::WARN
                    << "Cannot deduce receptor 1 for correlations of type: "
                    << Stokes::name(cType) << LogIO::POST;
        }
        receptor = Stokes::receptor2(cType);
        if (receptor.isValid()) {
            corrProduct_p(1, i) = receptor;
        } else if (!warn) {
            warn = True;
            itsLog << LogIO::WARN
                    << "Cannot deduce receptor 2 for correlations of type: "
                    << Stokes::name(cType) << LogIO::POST;
        }
    }

    // Save the object name, we may need it (for single source fits)
    const FitsKeyword* kwp;
    object_p = (kwp = priGroup_p.kw(FITS::OBJECT)) ? kwp->asString()
            : "unknown";
    object_p = object_p.before(trailing);
    // Save the array name
    array_p = (kwp = priGroup_p.kw(FITS::TELESCOP)) ? kwp->asString()
            : "unknown";
    array_p = array_p.before(trailing);
    // Save the RA/DEC epoch (for ss fits)
    if (priGroup_p.kw(FITS::EPOCH))
        epoch_p = (priGroup_p.kw(FITS::EPOCH))->asFloat();
    else if (priGroup_p.kw(FITS::EQUINOX))
        epoch_p = (priGroup_p.kw(FITS::EQUINOX))->asFloat();
    else {
        epoch_p = 2000.0;
        itsLog << LogIO::WARN
                << "Cannot find epoch of data, defaulting to J2000"
                << LogIO::POST;

    }
    //epoch_p = (kwp=priGroup_p.kw(FITS::EPOCH)) ? kwp->asFloat() : 2000.0;
    epochRef_p = getDirectionFrame(epoch_p);

    // Get the spectral information
    freqsys_p = MFrequency::TOPO;
    restfreq_p = 0.0;
    Record header;
    Vector<String> ignore;
    Bool ok = FITSKeywordUtil::getKeywords(header, priGroup_p.kwlist(), ignore);
    if (ok) {
        Int spectralAxis;
        Double referenceChannel, referenceFrequency, deltaFrequency;
        Vector<Double> frequencies;
        MDoppler::Types velPref;
        // Many of the following aren't used since they have been obtained
        // in other ways.
        ok = FITSSpectralUtil::fromFITSHeader(spectralAxis, referenceChannel,
                referenceFrequency, deltaFrequency, frequencies, freqsys_p,
                velPref, restfreq_p, itsLog, header);

        //    cout << "Discerned type: " << MFrequency::showType(freqsys_p) << endl;
        //    cout << "Discerned restfreq: " << restfreq_p << endl;

        // Override freqsys_p from FITSSpectralUtil, if SPECSYS keyword present
        if (header.isDefined(String("specsys"))) {
            String fframe;
            header.get("specsys", fframe);
            MFrequency::getType(freqsys_p, fframe);
            //      cout << "SPECSYS Override: " << MFrequency::showType(freqsys_p) << endl;
        }

        // Be strict about use of ALTREF-derived frequencies:
        //  Only if frame not enforced with SPECSYS keyword,
        //  sufficient info is available to do the back-calculation,
        //  and if that back calculation takes us out of the TOPO
        //  Otherwise, we assume that the header and FQ frequencies are
        //  SPECSYS (or TOPO) and are correct.
        useAltrval = (!header.isDefined(String("specsys")) && header.isDefined(
                String("altrval")) && header.isDefined(String("restfreq"))
                && freqsys_p != MFrequency::TOPO);
        //    cout << "useAltrval = " << boolalpha << useAltrval << endl;

        refFreq_p = referenceFrequency;
        chanFreq_p = frequencies;
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
    //cout << "setup New Table" << endl;
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
        Int nCorr = nPixel_p(getIndex(coordType_p, "STOKES"));
        Int nChan = nPixel_p(getIndex(coordType_p, "FREQ"));
        nIF_p = getIndex(coordType_p, "IF");
        if (nIF_p >= 0) {
            nIF_p = nPixel_p(nIF_p);
        } else {
            nIF_p = 1;
        }

        // Choose an appropriate tileshape
        IPosition dataShape(2, nCorr, nChan);
        IPosition tileShape = MSTileLayout::tileShape(dataShape, obsType,
                array_p);
        itsLog << LogOrigin("MSFitsInput", "setupMeasurementSet");
        itsLog << LogIO::NORMAL << "Using tile shape " << tileShape << " for "
                << array_p << " with obstype=" << obsType << LogIO::POST;
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
    //cout << "ms created" << endl;

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

    ms_p = ms;
    msc_p = new MSColumns(ms_p);
    msc_p->setDirectionRef(epochRef_p); // Does the subtables.

    // UVW is the only Direction type Measures column in the main table.
    msc_p->setUVWRef(Muvw::castType(epochRef_p));
}

void MSFitsInput::fillObsTables() {
    const Regex trailing(" *$"); // trailing blanks
    const FitsKeyword* kwp;
    ms_p.observation().addRow();
    String observer;
    observer = (kwp = priGroup_p.kw(FITS::OBSERVER)) ? kwp->asString() : "";
    observer = observer.before(trailing);
    MSObservationColumns msObsCol(ms_p.observation());
    msObsCol.observer().put(0, observer);
    String telescope = (kwp = priGroup_p.kw(FITS::TELESCOP)) ? kwp->asString()
            : "unknown";
    telescope = telescope.before(trailing);
    if (telescope == "HATCREEK")
        telescope = "BIMA";
    msObsCol.telescopeName().put(0, telescope);
    msObsCol.scheduleType().put(0, "");
    msObsCol.project().put(0, "");

    String date;
    date = (kwp = priGroup_p.kw(FITS::DATE_OBS)) ? kwp->asString() : "";
    if (date == "") {
        // try FITS::DATE instead
        //  (but this will find DATE-MAP which may not be correct...)
        date = (kwp = priGroup_p.kw(FITS::DATE)) ? kwp->asString() : "";
    }
    if (date == "")
        date = "2000-01-01";
    MVTime timeVal;
    MEpoch::Types epochRef;
    FITSDateUtil::fromFITS(timeVal, epochRef, date, "UTC");
    Vector<Double> times(2);
    times(0) = timeVal.second();
    times(1) = timeVal.second(); // change this to last time in input
    obsTime(0) = times(0);
    obsTime(1) = times(1);

    msObsCol.timeRange().put(0, times);
    msObsCol.releaseDate().put(0, times(0)); // just use TIME_RANGE for now
    Double time = timeVal.second();
    msObsCol.flagRow().put(0, False);

    // Store all keywords from the first HISTORY keyword onwards in History table
    String history = (kwp = priGroup_p.kw(FITS::HISTORY)) ? kwp->comm() : "";
    history = history.before(trailing);
    MSHistoryColumns msHisCol(ms_p.history());
    Int row = -1;
    while (history != "") {
        ms_p.history().addRow();
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
        history = (kwp = priGroup_p.nextkw()) ? kwp->comm() : "";
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
    MSHistoryColumns msHisCol(ms_p.history());
    Int row = ms_p.history().nrow() - 1;
    kwl.first();
    while ((kw = kwl.next())) {
        String nm = kw->name();
        //cout << "nm=" << nm << endl;
        if (nm == "HISTORY" || nm == "COMMENT" || nm == "") {
            history = kw->comm();
            //cout << "history=" << history << endl;
            history = history.before(trailing);
            ms_p.history().addRow();
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
    itsLog << LogOrigin("MSFitsInput", "fillMSMainTable");
    // Get access to the MS columns
    MSColumns& msc(*msc_p);
    const Regex trailing(" *$"); // trailing blanks

    // get the random group parameter names
    Int nParams;
    Int nGroups;
    nParams = priGroup_p.pcount();
    nGroups = priGroup_p.gcount();
    Vector<String> pType(nParams);
    for (Int i = 0; i < nParams; i++) {
        pType(i) = priGroup_p.ptype(i);
        pType(i) = pType(i).before(trailing);
    }
    Int totRows = nGroups * max(1, nIF_p);

    Int nCorr = nPixel_p(getIndex(coordType_p, "STOKES"));
    Int nChan = nPixel_p(getIndex(coordType_p, "FREQ"));

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
    // get indices for time
    Int iTime0 = getIndex(pType, "DATE", 0);
    Int iTime1 = getIndex(pType, "DATE", 1);
    // get index for source
    Int iSource = getIndex(pType, "SOURCE");
    // get index for Freq
    Int iFreq = getIndex(pType, "FREQSEL");

    // get index for Integration time
    Int iInttim = getIndex(pType, "INTTIM");

    receptorAngle_p.resize(1);
    nAnt_p = 0;
    itsLog << LogIO::NORMAL << "Reading and writing " << nGroups
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
    nArray_p = -1;

    Bool lastRowFlag = False;
    Vector<Int> ant1(totRows);
    Vector<Int> ant2(totRows);
    Vector<Double> interv(totRows);
    Vector<Double> expos(totRows);
    Vector<Int> datDescId(totRows);

    ms_p.addRow(totRows);
    Int nif = max(1, nIF_p);

    // Loop over groups
    for (Int group = 0; group < nGroups; group++) {

        // Read next group and
        priGroup_p.read();

        // Extract time in MJD seconds
        const Double JDofMJD0 = 2400000.5;
        Double time = priGroup_p.parm(iTime0);
        time -= JDofMJD0;
        if (iTime1 >= 0)
            time += priGroup_p.parm(iTime1);
        time *= C::day;

        // Extract fqid
        Int freqId = Int(priGroup_p.parm(iFreq));

        // Extract field Id
        Int fieldId = 0;
        if (iSource >= 0) {
            // make 0-based
            fieldId = (Int) priGroup_p.parm(iSource) - 1;
        }
        Float baseline = priGroup_p.parm(iBsln);
        Int arrayId = Int(100.0 * (baseline - Int(baseline) + 0.001));
        nArray_p = max(nArray_p, arrayId + 1);
        for (Int k = 0; k < nif; ++k) {

            Int index = group * nif + k;
            // Extract uvw
            uvw(0, index) = priGroup_p.parm(iU) * C::c;
            uvw(1, index) = priGroup_p.parm(iV) * C::c;
            uvw(2, index) = priGroup_p.parm(iW) * C::c;
            // Convert from units of seconds to meters

            ant1(index) = Int(baseline) / 256;
            nAnt_p = max(nAnt_p, ant1(index));
            ant2(index) = Int(baseline) - ant1(index) * 256;
            nAnt_p = max(nAnt_p, ant2(index));
            ant1(index)--;
            ant2(index)--; // make 0-based
        }
        // Ensure arrayId-specific params are of correct length:
        if (scanNumber.shape() < nArray_p) {
            scanNumber.resize(nArray_p, True);
            lastFieldId.resize(nArray_p, True);
            lastFreqId.resize(nArray_p, True);
            scanNumber(nArray_p - 1) = 0;
            lastFieldId(nArray_p - 1) = -1;
            lastFreqId(nArray_p - 1) = -1;
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
            exposure = priGroup_p.parm(iInttim);
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
        Bool polFastest = (getIndex(coordType_p, "STOKES") < getIndex(
                coordType_p, "FREQ"));
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
                    const Float visReal = priGroup_p(count++);
                    const Float visImag = priGroup_p(count++);
                    const Float wt = priGroup_p(count++);
                    const Int pol = (polFastest ? corrIndex_p[iy]
                            : corrIndex_p[ix]);
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
                spW = (Int) priGroup_p.parm(iFreq) - 1; // make 0-based
                if (nIF_p > 0) {
                    spW *= nIF_p;
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
    receptorAngle_p = 0;
}

// Extract the data from the PrimaryGroup object and stick it into
// the MeasurementSet 
// Doing it row by row
void MSFitsInput::fillMSMainTable(Int& nField, Int& nSpW) {
    itsLog << LogOrigin("MSFitsInput", "fillMSMainTable");
    // Get access to the MS columns
    MSColumns& msc(*msc_p);
    const Regex trailing(" *$"); // trailing blanks

    // get the random group parameter names
    Int nParams;
    Int nGroups;
    nParams = priGroup_p.pcount();
    nGroups = priGroup_p.gcount();
    Vector<String> pType(nParams);
    for (Int i = 0; i < nParams; i++) {
        pType(i) = priGroup_p.ptype(i);
        pType(i) = pType(i).before(trailing);
    }

    Int nCorr = nPixel_p(getIndex(coordType_p, "STOKES"));
    Int nChan = nPixel_p(getIndex(coordType_p, "FREQ"));


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
    // get indices for time
    Int iTime0 = getIndex(pType, "DATE", 0);
    Int iTime1 = getIndex(pType, "DATE", 1);
    // get index for source
    Int iSource = getIndex(pType, "SOURCE");
    // get index for Freq
    Int iFreq = getIndex(pType, "FREQSEL");

    // get index for Integration time
    Int iInttim = getIndex(pType, "INTTIM");

    receptorAngle_p.resize(1);
    nAnt_p = 0;
    itsLog << LogIO::NORMAL << "Reading and writing " << nGroups
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
    nArray_p = -1;

    Bool lastRowFlag = False;

    // Loop over groups
    for (Int group = 0; group < nGroups; group++) {

        // Read next group and
        priGroup_p.read();

        // Extract time in MJD seconds
        //  (this has VERY limited precision [~0.01s])
        const Double JDofMJD0 = 2400000.5;
        Double time = priGroup_p.parm(iTime0);
        time -= JDofMJD0;
        if (iTime1 >= 0)
            time += priGroup_p.parm(iTime1);
        time *= C::day;

        // Extract fqid
        Int freqId = Int(priGroup_p.parm(iFreq));

        // Extract field Id
        Int fieldId = 0;
        if (iSource >= 0) {
            // make 0-based
            fieldId = (Int) priGroup_p.parm(iSource) - 1;
        }

        // Extract uvw
        uvw(0) = priGroup_p.parm(iU);
        uvw(1) = priGroup_p.parm(iV);
        uvw(2) = priGroup_p.parm(iW);
        // Convert from units of seconds to meters
        uvw *= C::c;

        // Extract array/baseline/antenna info
        Float baseline = priGroup_p.parm(iBsln);
        Int arrayId = Int(100.0 * (baseline - Int(baseline) + 0.001));
        nArray_p = max(nArray_p, arrayId + 1);

        Int ant1 = Int(baseline) / 256;
        nAnt_p = max(nAnt_p, ant1);
        Int ant2 = Int(baseline) - ant1 * 256;
        nAnt_p = max(nAnt_p, ant2);
        ant1--;
        ant2--; // make 0-based

        // Ensure arrayId-specific params are of correct length:
        if (scanNumber.shape() < nArray_p) {
            scanNumber.resize(nArray_p, True);
            lastFieldId.resize(nArray_p, True);
            lastFreqId.resize(nArray_p, True);
            scanNumber(nArray_p - 1) = 0;
            lastFieldId(nArray_p - 1) = -1;
            lastFreqId(nArray_p - 1) = -1;
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
            exposure = priGroup_p.parm(iInttim);
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
        Bool polFastest = (getIndex(coordType_p, "STOKES") < getIndex(
                coordType_p, "FREQ"));
        const Int nx = (polFastest ? nChan : nCorr);
        const Int ny = (polFastest ? nCorr : nChan);

        Int count = 0;
        for (Int ifno = 0; ifno < max(1, nIF_p); ifno++) {
            // IFs go to separate rows in the MS
            ms_p.addRow();
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
                    const Float visReal = priGroup_p(count++);
                    const Float visImag = priGroup_p(count++);
                    const Float wt = priGroup_p(count++);
                    const Int pol = (polFastest ? corrIndex_p[iy]
                            : corrIndex_p[ix]);
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
                spW = (Int) priGroup_p.parm(iFreq) - 1; // make 0-based
                if (nIF_p > 0) {
                    spW *= nIF_p;
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
    receptorAngle_p = 0;
    // set the Measure References


    // Extract and print data manager cache statistics
    // for flag and weight spectrum
    // ROTiledStManAccessor dmFlag(ms_p, "TiledFlag");
    //  cout << "TiledFlag statistics" << endl;
    //  dmFlag.showCacheStatistics(cout);
    //  cout << "---------------------------------------------------" << endl;

    //  cout << endl;
    //  cout << "TiledWgtSpectrum statistics" << endl;
    //  ROTiledStManAccessor dmWeight(ms_p, "TiledWgtSpectrum");
    //  dmWeight.showCacheStatistics(cout);
    //  cout << "---------------------------------------------------" << endl;
}

void MSFitsInput::fillAntennaTable(BinaryTable& bt) {
    const Regex trailing(" *$"); // trailing blanks
    TableRecord btKeywords = bt.getKeywords();
    Int nAnt, nAntMax;
    Bool missingAnts = False;
    nAntMax = nAnt_p;
    if (nAnt_p != bt.nrows()) {
        itsLog << LogOrigin("MSFitsInput", "fillAntennaTable")
               << array_p
                << " telescope quirk detected.  Filler purports to construct the full"
                << " ANTENNA table with possible blank entries."
                << LogIO::NORMAL1 << LogIO::POST;
        missingAnts = True;
    }

    nAnt = bt.nrows();
    //cout << "nAnt=" << nAnt << endl;
    if (nAnt - 1 > nAntMax)
        nAntMax = nAnt - 1;

    if (missingAnts)
        receptorAngle_p.resize(2 * (nAntMax + 1));
    else
        receptorAngle_p.resize(2 * nAnt);
    receptorAngle_p = 0.0;
    Vector<Double> arrayXYZ(3);
    arrayXYZ = 0.0;
    if (!btKeywords.isDefined("ARRAYX") || !btKeywords.isDefined("ARRAYY")
            || !btKeywords.isDefined("ARRAYZ")) {
        throw(AipsError("MSFitsInput: Illegal AN file: no antenna positions"));
    }
    arrayXYZ(0) = bt.getKeywords().asdouble("ARRAYX");
    arrayXYZ(1) = bt.getKeywords().asdouble("ARRAYY");
    arrayXYZ(2) = bt.getKeywords().asdouble("ARRAYZ");

    // itsLog << LogIO::NORMAL << "number of antennas ="<<nAnt<<LogIO::POST;
    // itsLog << LogIO::NORMAL << "array ref pos:"<<arrayXYZ<<LogIO::POST;

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
    String arrnam = "Unknown";
    if (btKeywords.isDefined("ARRNAM")) {
        arrnam = btKeywords.asString("ARRNAM");
    }
    // store the time keywords
    ms_p.antenna().rwKeywordSet().define(String("RDATE"), rdate);
    ms_p.antenna().rwKeywordSet().define(String("GSTIA0"), gst);
    ms_p.antenna().rwKeywordSet().define(String("DEGPDY"), degpdy);
    ms_p.antenna().rwKeywordSet().define(String("TIMSYS"), timsys);
    //save value to set time reference frame elsewhere
    timsys_p = timsys;
    // Fill in some likely values
    Float diameter = 25;
    if (array_p == "ATCA")
        diameter = 22;
    Bool doSMA = (array_p == "SMA");
    Bool doCARMA = (array_p == "CARMA");
    if (doSMA)
        diameter = 6;
    if (array_p == "ATA")
        diameter = 6.1;
    if (array_p == "HATCREEK" || array_p == "BIMA")
        diameter = 6.1;
    if (array_p == "GMRT")
        diameter = 45.0;
    if (array_p == "IRAM_PDB" || array_p == "IRAM PDB")
        diameter = 15.0;

    //   Table anTab=bt.fullTable("",Table::Scratch);
    Table anTab = bt.fullTable();
    MSAntennaColumns& ant(msc_p->antenna());
    ROScalarColumn<String> name(anTab, "ANNAME");
    ROScalarColumn<Int> id(anTab, "NOSTA");
    ROScalarColumn<Int> mountType(anTab, "MNTSTA");
    ROScalarColumn<Float> offset(anTab, "STAXOF");
    ROScalarColumn<Float> polangleA(anTab, "POLAA");
    ROScalarColumn<Float> polangleB(anTab, "POLAB");
    ROArrayColumn<Double> antXYZ(anTab, "STABXYZ");
    Vector<Float> antDiams(nAnt);
    antDiams.set(diameter);


    //If it has a column called DIAMETER ...make use of it
    if (anTab.tableDesc().isColumn("DIAMETER")) {
        ROScalarColumn<Float> fitsDiams(anTab, "DIAMETER");
        antDiams = fitsDiams.getColumn();
    } else if ((array_p == "OVRO") || (array_p == "CARMA")) {
        for (Int i = 0; i < nAnt; i++) {
            //Crystal Brogan has guaranteed that it is always this order
            if (id(i) <= 6)
                antDiams(i) = 10.4;
            else
                antDiams(i) = 6.1;
        }
    }
    // Prepare handling of UVFITS Antenna position coord conventions:
    // VLA requires rotation of local coords:
    Bool doVLARot = (array_p == "VLA");
    // initialize rotation matrix with zero rotation
    Matrix<Double> posRot = Rot3D(0, 0.0);
    // Similar transformations are added for SMA and CARMA for now.
    // May need to make these default processing for most of the telescopes
    // in future.
    if (doVLARot) {
        // Array position for VLA from aips may be wrong, so use
        //  authoritative position from measures (station positions
        //  are from on-line system and are relative to this)
        MPosition vlaCentre;
        AlwaysAssert(MeasTable::Observatory(vlaCentre, "VLA"), AipsError);
        arrayXYZ = vlaCentre.getValue().getValue();
        // Form rotation around Z axis by VLA longitude=atan(arrayY/arrayX)
        Double vlaLong = atan2(arrayXYZ(1), arrayXYZ(0));
        posRot = Rot3D(2, vlaLong); // Applied to each ant position below
    }
    if (doSMA || doCARMA) {
        //do VLA-like rotation...for SMA and CARMA
        MPosition arrayCentre;
        AlwaysAssert(MeasTable::Observatory(arrayCentre, array_p), AipsError);
        arrayXYZ = arrayCentre.getValue().getValue();
        Double arrayLong = atan2(arrayXYZ(1), arrayXYZ(0));
        posRot = Rot3D(2, arrayLong); // Applied to each ant position below
    }
    // All "VLBI" (==arrayXYZ<1000) requires y-axis reflection:
    //  (ATCA looks like "VLBI" in UVFITS, but is already correct)
    Bool doVLBIRefl = ((array_p != "ATCA") && allLE(abs(arrayXYZ), 1000.0));

    // add antenna info to table
    ant.setPositionRef(MPosition::ITRF);
    Int row = ms_p.antenna().nrow();

    if (missingAnts && (row < nAntMax)) {
        Int n = nAntMax - row + 1;
        for (Int i = 0; i < n; i++) {
            ms_p.antenna().addRow();
            row++;
        }
    }
    row = ms_p.antenna().nrow() - 1;
    for (uInt i = 0; i < ms_p.antenna().nrow(); i++)
        ant.flagRow().put(i, True);

    for (Int i = 0; i < nAnt; i++) {
        if (!missingAnts) {
            ms_p.antenna().addRow();
            row++;
        } else {
            row = id(i) - 1;
        }

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
        case 5:
        case 6:
            mount = "BIZARRE";
            break;
        default:
            mount = "SPACE_HALCA";
            break;
        }
        //overwrite mount type for SMA
        if (doSMA)
            mount = "ALT-AZ";
        ant.flagRow().put(row, False);
        ant.mount().put(row, mount);
        //cout << "array_p=" << array_p << endl;
        if (array_p == "CARMA" && newNameStyle) {
            ostringstream oss;
            oss << "CA" << id(i);
            ant.name().put(row, oss.str());
        }
        else if (array_p == "EVLA" && newNameStyle) {
            ostringstream oss;
            oss << "EA" << setw(2) << setfill('0') << id(i);
            ant.name().put(row, oss.str());
        }
        else if (doVLARot && newNameStyle) {
            ostringstream oss;
            //if (name(i).contains("EVLA"))
            //    oss << "EA" << setw(2) << setfill('0') << id(i);
            //else
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
        // ROArrayColumn antXYZ(i) may need coord transform; do it in corXYZ:
        Vector<Double> corXYZ = antXYZ(i);
        // If nec, rotate coordinates out of local VLA frame to ITRF
        //if ( doVLARot ) corXYZ=product(posRot,corXYZ);
        if (doVLARot || doSMA || doCARMA)
            corXYZ = product(posRot, corXYZ);

        // If nec, reflect y-coord to yield right-handed geocentric:
        if (doVLBIRefl)
            corXYZ(1) = -corXYZ(1);

        ant.position().put(row, arrayXYZ + corXYZ);
        // store the angle for use in the feed table
        receptorAngle_p(2 * i + 0) = polangleA(i) * C::degree;
        receptorAngle_p(2 * i + 1) = polangleB(i) * C::degree;
    }

    // store these items in non-standard keywords for now
    ant.name().rwKeywordSet().define("ARRAY_NAME", arrnam);
    ant.position().rwKeywordSet().define("ARRAY_POSITION", arrayXYZ);
}



void MSFitsInput::fillSpectralWindowTable(BinaryTable& bt, Int nSpW)
{

  //cout << "fSWT type: " << MFrequency::showType(freqsys_p) << endl;

  MSSpWindowColumns& msSpW(msc_p->spectralWindow());
  MSDataDescColumns& msDD(msc_p->dataDescription());
  MSPolarizationColumns& msPol(msc_p->polarization());
  Int iFreq = getIndex(coordType_p, "FREQ");
  Int nChan = nPixel_p(iFreq);
  Int nCorr = nPixel_p(getIndex(coordType_p,"STOKES"));
  // assume spectral line, make source table to allow restfreq to be entered
  //if (nChan>33) addSourceTable_p=True; 
  if (nChan>0) addSourceTable_p=True; 

  // fill out the polarization info (only single entry allowed in fits input)
  ms_p.polarization().addRow();
  msPol.numCorr().put(0,nCorr);
  msPol.corrType().put(0,corrType_p);
  msPol.corrProduct().put(0,corrProduct_p);
  msPol.flagRow().put(0,False);

  //  Table fqTab=bt.fullTable("",Table::Scratch);
  Table fqTab=bt.fullTable();
  Int nRow=fqTab.nrow();
  ROScalarColumn<Int> colFrqSel(fqTab,"FRQSEL");
  Matrix<Double> ifFreq(nIF_p,nRow);
  Matrix<Float> chWidth(nIF_p,nRow);
  Matrix<Float> totalBandwidth(nIF_p,nRow);
  // The type of the column changes according to the number of entries
  //cout << "nIF_p=" << nIF_p << endl;
  if (nIF_p==1) {
    ROScalarColumn<Double> colIFFreq(fqTab,"IF FREQ");
    ROScalarColumn<Float> colChWidth(fqTab,"CH WIDTH");
    ROScalarColumn<Float> colTotalBandwidth(fqTab,"TOTAL BANDWIDTH");
    for (Int i=0; i<nRow; i++) {
      ifFreq(0,i)=colIFFreq(i);
      chWidth(0,i)=colChWidth(i);
      totalBandwidth(0,i)=colTotalBandwidth(i);
    }
  } else {
    ROArrayColumn<Double> colIFFreq(fqTab,"IF FREQ");
    ROArrayColumn<Float> colChWidth(fqTab,"CH WIDTH");
    ROArrayColumn<Float> colTotalBandwidth(fqTab,"TOTAL BANDWIDTH");
    try{
      colIFFreq.getColumn(ifFreq);
      colChWidth.getColumn(chWidth);
      colTotalBandwidth.getColumn(totalBandwidth);
    }catch(AipsError x) {
      itsLog << LogOrigin("MSFitsInput", "fillSpectralWindowTable")
             << LogIO::DEBUG1 << x.getMesg() << LogIO::POST;
    }
    catch(...) {
      itsLog << LogOrigin("MSFitsInput", "fillSpectralWindowTable")
             << LogIO::DEBUG1 << "unknown Error"  << LogIO::POST;
    }
  }

  for (Int spw=0; spw<nSpW; spw++) {
    ms_p.spectralWindow().addRow();
    ms_p.dataDescription().addRow();
    
    msDD.spectralWindowId().put(spw,spw);
    msDD.polarizationId().put(spw,0);
    msDD.flagRow().put(spw,False);
    Int ifc=0;
    Int freqGroup = 0;
    if (nIF_p>0) {
      ifc=spw%nIF_p;
      freqGroup = spw/nIF_p;
    }
    Int fqRow=spw/max(1,nIF_p);
    if (fqRow != colFrqSel(fqRow)-1) 
      itsLog << LogIO::SEVERE << "Trouble interpreting FQ table, id's may be wrong" << LogIO::POST; 
    msSpW.name().put(spw,"none");
    msSpW.ifConvChain().put(spw,ifc);
    msSpW.numChan().put(spw,nChan);
    Double refChan = refPix_p(iFreq);
    // using data from FQ table
    Double refFreq=refVal_p(iFreq)+ifFreq(ifc,fqRow);

    Double chanBandwidth=chWidth(ifc,fqRow);
    //TT debug
    Vector<Double> chanFreq(nChan),resolution(nChan);
    for (Int i=0; i < nChan; i++) {
      chanFreq(i)= refFreq + (i+1-refChan) * chanBandwidth;
    }
    resolution=abs(chanBandwidth);

    //if altrval (and altrpix) fits keywords exist use
    //recalucalated values instead of the data form FQ table
    if (useAltrval) {

      refFreq = refFreq_p;
      chanFreq = chanFreq_p;
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
    msSpW.measFreqRef().put(spw,freqsys_p);
  }
}

void MSFitsInput::fillSpectralWindowTable()
{
  MSSpWindowColumns& msSpW(msc_p->spectralWindow());
  MSDataDescColumns& msDD(msc_p->dataDescription());
  MSPolarizationColumns& msPol(msc_p->polarization());
  Int iFreq = getIndex(coordType_p, "FREQ");
  Int nChan = nPixel_p(iFreq);
  Int nCorr = nPixel_p(getIndex(coordType_p,"STOKES"));
  // assume spectral line, make source table to allow restfreq to be entered
  //if (nChan>33) addSourceTable_p=True; 
  if (nChan>0) addSourceTable_p=True; 

  // fill out the polarization info (only single entry allowed in fits input)
  ms_p.polarization().addRow();
  msPol.numCorr().put(0,nCorr);
  msPol.corrType().put(0,corrType_p);
  msPol.corrProduct().put(0,corrProduct_p);
  msPol.flagRow().put(0,False);

  Int spw=0;
  ms_p.spectralWindow().addRow();
  ms_p.dataDescription().addRow();

  msDD.spectralWindowId().put(spw,spw);
  msDD.polarizationId().put(spw,0);
  msDD.flagRow().put(spw,False); 

  msSpW.name().put(spw,"none");
  msSpW.ifConvChain().put(spw,0);
  msSpW.numChan().put(spw,nChan);
  Double refChan = refPix_p(iFreq);
  Double refFreq=refVal_p(iFreq);
  Double chanBandwidth=delta_p(iFreq);
  Vector<Double> chanFreq(nChan),resolution(nChan);
  for (Int i=0; i < nChan; i++) {
    chanFreq(i)= refFreq + (i+1-refChan) * chanBandwidth;
  }
  resolution=chanBandwidth;
  //if altrval (and altrpix) fits keywords exist use
  //recalucalated values
  if (useAltrval) {
    refFreq = refFreq_p;
    chanFreq = chanFreq_p;
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
  msSpW.measFreqRef().put(spw,freqsys_p);
}

// Returns the Direction Measure reference for UVW and other appropriate columns
// in msc_p (which must exist but have empty columns before you can set it!).
MDirection::Types MSFitsInput::getDirectionFrame(Double epoch) {

    MDirection::Types epochRef = MDirection::J2000;
    if (nearAbs(epoch, 1950.0, 0.01))
        epochRef = array_p == "VLA" ? MDirection::B1950_VLA : MDirection::B1950;
    itsLog << LogOrigin("MSFitsInput", "getDirectionFrame") 
           << LogIO::DEBUG1 << "epochRef ok " << LogIO::POST;

    return epochRef;
}

void MSFitsInput::fillFieldTable(BinaryTable& bt, Int nField) {
    MSFieldColumns& msField(msc_p->field());

    TableRecord btKeywords = bt.getKeywords();
    if (!btKeywords.isDefined("NO_IF")) {
        throw(AipsError("MSFitsInput: Illegal SU file: no number of IFs"));
    }
    uInt noif = bt.getKeywords().asuInt("NO_IF");

    // Table suTab=bt.fullTable("",Table::Scratch);
    Table suTab = bt.fullTable();
    ROScalarColumn<Int> id(suTab, "ID. NO.");
    ROScalarColumn<String> name(suTab, "SOURCE");
    ROScalarColumn<Int> qual(suTab, "QUAL");
    Bool multiqual = False;
    Int minqual, maxqual;
    minMax(minqual, maxqual, qual.getColumn());
    if (minqual != maxqual)
        multiqual = True;
    ROScalarColumn<String> code(suTab, "CALCODE");
    // ROScalarColumn<Float> iflux(suTab,"IFLUX"); // etc Q, U, V (Jy)
    ROScalarColumn<Double> ra(suTab, "RAEPO"); //degrees
    ROScalarColumn<Double> dec(suTab, "DECEPO"); //degrees
    ROScalarColumn<Double> raapp(suTab, "RAAPP"); //degrees
    ROScalarColumn<Double> decapp(suTab, "DECAPP"); //degrees
    ROScalarColumn<Double> epoch(suTab, "EPOCH"); //years
    ROScalarColumn<Double> pmra(suTab, "PMRA"); //deg/day
    ROScalarColumn<Double> pmdec(suTab, "PMDEC"); //deg/day
    if (Int(suTab.nrow()) < nField) {
        itsLog << LogOrigin("MSFitsInput", "fillFieldTable")
               << LogIO::NORMAL
                << "Input Source id's not sequential, adding empty rows in output"
                << LogIO::POST;
    }
    Int outRow = -1;

    // RESTFREQ and LSRVEL are 2D columns according to the AIPS Memo 117
    restFreq_p.resize(noif, suTab.nrow());
    sysVel_p.resize(noif, suTab.nrow());
    try{
      ROArrayColumn<Double> restfreq(suTab,"RESTFREQ");  // Hz
      ROArrayColumn<Double> sysvel(suTab,"LSRVEL"); // m/s
      restfreq.getColumn(restFreq_p);
      sysvel.getColumn(sysVel_p);
    }
    catch (std::exception x) {
      if(noif>1){
	itsLog << LogOrigin("MSFitsInput", "fillFieldTable") << LogIO::WARN
	       << x.what() << ": " << "Inconsistent setup of RESTFREQ and LSRVEL columns." << endl
	       << "With NO_IF>1, they should be arrays not scalars." << LogIO::POST;
      }
      ROScalarColumn<Double> restfreq(suTab,"RESTFREQ");  // Hz
      ROScalarColumn<Double> sysvel(suTab,"LSRVEL"); // m/s
      Vector<Double> tmprf(suTab.nrow());
      Vector<Double> tmpsv(suTab.nrow());
      restfreq.getColumn(tmprf);
      sysvel.getColumn(tmpsv);
      for(uInt ii=0; ii<suTab.nrow(); ii++){
	restFreq_p(0,ii) = tmprf(ii);
	sysVel_p(0,ii) = tmpsv(ii);
      }
    }      

    // set the DIRECTION MEASURE REFERENCE for appropriate columns

    //   IF UVFITS CAME FROM AIPS, AND THE FIRST ROW OF SU TABLE
    //   CONTAINS A PLANET THAT WAS TRACKED BY THE CORRELATOR (EPOCH=-1),
    //   THE D.M. REFERENCE WILL BE J2000, WHICH MAY NOT BE CORRECT
    //   FOR THE PLANETS IN THE LIST (see defect 3636).

    MDirection::Types epochRefZero = getDirectionFrame(epoch(0));
    if (epochRefZero != epochRef_p)
        itsLog << LogOrigin("MSFitsInput", "fillFieldTable")
               << LogIO::WARN << "The direction measure reference code, "
                << epochRefZero << "\n"
                << "for the first field does not match the one from the FITS header, "
                << epochRef_p
                << ".\nThis might cause a problem for the reference frame"
                << " of the output's UVW column." << LogIO::POST;

    for (Int inRow = 0; inRow < (Int) suTab.nrow(); inRow++) {
        Int fld = id(inRow) - 1;
        // add empty rows until the row number in the output matches the source id
        while (fld > outRow) {
            // Append a flagged, empty row to the FIELD table
            ms_p.field().addRow();
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
                if (array_p == "VLA")
                    epochRef = MDirection::B1950_VLA;
                else
                    epochRef = MDirection::B1950;
            } else if (epoch(inRow) == -1.0) {
                epochRef = epochRefZero;
                itsLog << LogOrigin("MSFitsInput", "fillFieldTable")
                       << " Assuming standard epoch " << " for " << name(inRow)
                        << ".  Be aware that this may not be correct." << endl;
            } else {
                itsLog << LogOrigin("MSFitsInput", "fillFieldTable")
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
        const Vector<Double> obsTimes = msc_p->observation().timeRange()(0);

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
        msc_p->fieldId().fillColumn(0);
    }

    MSFieldColumns& msField(msc_p->field());
    ms_p.field().addRow();
    Int fld = 0;
    msField.sourceId().put(fld, -1); // source table not used
    msField.code().put(fld, " ");
    msField.name().put(fld, object_p);
    Vector<MDirection> radecMeas(1);
    radecMeas(0).set(MVDirection(refVal_p(getIndex(coordType_p, "RA"))
            * C::degree, refVal_p(getIndex(coordType_p, "DEC")) * C::degree),
            MDirection::Ref(epochRef_p));

    msField.numPoly().put(fld, 0);
    msField.delayDirMeasCol().put(fld, radecMeas);
    msField.phaseDirMeasCol().put(fld, radecMeas);
    msField.referenceDirMeasCol().put(fld, radecMeas);

    // Use TIME_RANGE in OBSERVATION table to set TIME here.
    const Vector<Double> obsTimes = msc_p->observation().timeRange()(0);
    msField.time().put(fld, obsTimes(0));

}

void MSFitsInput::fillFeedTable() {
    MSFeedColumns& msfc(msc_p->feed());

    // find out the POLARIZATION_TYPE
    // In the fits files we handle there can be only a single, uniform type
    // of polarization so the following should work.
    MSPolarizationColumns& msPolC(msc_p->polarization());
    Int numCorr = msPolC.numCorr()(0);
    Vector<String> rec_type(2);
    rec_type = "?";
    if (corrType_p(0) >= Stokes::RR && corrType_p(numCorr - 1) <= Stokes::LL) {
        rec_type(0) = "R";
        rec_type(1) = "L";
    }
    if (corrType_p(0) >= Stokes::XX && corrType_p(numCorr - 1) <= Stokes::YY) {
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
    const Vector<Double> obsTimes = msc_p->observation().timeRange()(0);
    // nAnt as here ensures ANTENNA and FEED have the same number
    //   of rows (nAnt_p <= nAnt, since some ants not in data)
    Int nAnt = msc_p->antenna().nrow();
    for (Int ant = 0; ant < nAnt; ant++) {
        ms_p.feed().addRow();
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
        msfc.receptorAngle().put(row, receptorAngle_p(Slice(2 * ant, 2)));
    }
}

void MSFitsInput::fillExtraTables() {
    // fill the pointing table and possibly the source table
    // run though the main table, find field changes, and add pointing rows
    // as needed by looking up the field info in the field table
    // If requested also look for new spectralwindows and add source
    // table entries for each field/spw combination

    if (addSourceTable_p)
        itsLog << LogOrigin("MSFitsInput", "fillExtraTables")
               << LogIO::NORMAL << "Filling SOURCE table." << LogIO::POST;

    Int nrow = ms_p.nrow();
    Int nAnt = ms_p.antenna().nrow();
    Int lastFieldId = -1;
    Int lastDDId = -1;
    Double lastTime = 0;
    Vector<Int> fieldId = msc_p->fieldId().getColumn();
    Vector<Int> ddId;
    if (addSourceTable_p){
      ddId = msc_p->dataDescId().getColumn();
    }

    SimpleOrderedMap <pair<Int,Int>, Int> sourceFieldIndex(-1); // for the case we need to write the source table

    ProgressMeter meter(0.0, nrow * 1.0, "UVFITS Filler", "rows copied",
                "", "", True, nrow / 100);

    for (Int i = 0; i < nrow; i++) {
        if (fieldId(i) != lastFieldId || (addSourceTable_p && ddId(i)
                != lastDDId)) {
            lastFieldId = fieldId(i);
            if (i > 0)
                lastTime = msc_p->time()(i - 1);
            Array<Double> pointingDir = msc_p->field().phaseDir()(lastFieldId);
            String name = msc_p->field().name()(lastFieldId);
            //Int numPoly = msc_p->field().numPoly()(lastFieldId);
            Double time = msc_p->time()(i);
            Int np = ms_p.pointing().nrow();
            if (np > 0) {
                // fix up time and interval for previous entries
                Double midTime = (lastTime + msc_p->pointing().time()(np - 1))
                        / 2;
                Double interval = lastTime - msc_p->pointing().time()(np - 1)
                        + msc_p->interval()(i - 1);
                for (Int j = 0; j < nAnt; j++) {
                    msc_p->pointing().time().put(np - j - 1, midTime);
                    msc_p->pointing().timeOrigin().put(np - j - 1, midTime);
                    msc_p->pointing().interval().put(np - j - 1, interval);
                }
            }
            /* This is not right for concatenating later for mosaicing
             As it is a useless piece of info copy from Field...field table will
             do

             // The ISMStMan is used for all but antennaId, so only put once
             for (Int j=0; j<nAnt; j++) {
             ms_p.pointing().addRow();
             msc_p->pointing().antennaId().put(np+j, j);
             if (j==0) {
             msc_p->pointing().time().put(np+j,time);
             msc_p->pointing().timeOrigin().put(np+j,time);
             msc_p->pointing().interval().put(np+j,0);
             msc_p->pointing().name().put(np+j, name);
             msc_p->pointing().numPoly().put(np+j, numPoly);
             msc_p->pointing().direction().put(np+j,pointingDir);
             msc_p->pointing().target().put(np+j,pointingDir);
             msc_p->pointing().tracking().put(np+j,True);
             }
             }
             */
            if (addSourceTable_p) {

                lastDDId = ddId(i);
                Int spwId = msc_p->dataDescription().spectralWindowId()(lastDDId);
                // now check if we've seen this field for this spectral window
                // Use indexed access to the SOURCE sub-table
		pair<Int, Int> myfldspw = make_pair(lastFieldId, spwId);
		if(!sourceFieldIndex.isDefined(myfldspw)){

		    sourceFieldIndex.define(myfldspw, 1); 

                    ms_p.source().addRow();
                    Int j = ms_p.source().nrow() - 1;
                    MSSourceColumns & mss = msc_p->source();
                    mss.sourceId().put(j, lastFieldId);
                    msc_p->field().sourceId().put(lastFieldId, lastFieldId);
                    mss.name().put(j, name);
                    Matrix<Double> phaseDir = msc_p->field().phaseDir()(
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
		    if(0<=lastFieldId && (uInt)lastFieldId<sysVel_p.ncolumn()){
		      sysVel(0) = sysVel_p(0, lastFieldId);
		    }
		    else{
		      itsLog << LogOrigin("MSFitsInput", "fillExtraTable")
			     << LogIO::WARN << "No systemic velocity for field " << lastFieldId << LogIO::POST;
		      sysVel(0) = 0.;
		    }		      

                    mss.sysvel().put(j, sysVel);
                    mss.numLines().put(j, 1);
                    Vector<String> transition(1);
                    transition(0) = "";
                    mss.transition().put(j, transition);
                    Vector<Double> restFreqs(1);

		    if(0<=lastFieldId && (uInt)lastFieldId<restFreq_p.ncolumn()){
		      restFreqs(0) = restFreq_p(0, lastFieldId);
		    }
		    else{
		      itsLog << LogOrigin("MSFitsInput", "fillExtraTable")
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
    lastTime = msc_p->time()(nrow - 1);
    Int np = ms_p.pointing().nrow();
    if (np > 0) {
        // fix up time and interval for previous entries
        Double midTime = (lastTime + msc_p->pointing().time()(np - 1)) / 2;
        Double interval = lastTime - msc_p->pointing().time()(np - 1)
                + msc_p->interval()(nrow - 1);
        for (Int j = 0; j < nAnt; j++) {
            msc_p->pointing().time().put(np - j - 1, midTime);
            msc_p->pointing().timeOrigin().put(np - j - 1, midTime);
            msc_p->pointing().interval().put(np - j - 1, interval);
        }
    }
}

void MSFitsInput::fixEpochReferences() {
    if (timsys_p == "IAT")
        timsys_p = "TAI";
    if (timsys_p == "UTC" || timsys_p == "TAI") {
        if (timsys_p == "UTC")
            msc_p->setEpochRef(MEpoch::UTC, False);
        if (timsys_p == "TAI")
            msc_p->setEpochRef(MEpoch::TAI, False);
    } else {
        if (timsys_p != "")
            itsLog << LogOrigin("MSFitsInput", "fixEpochReferences")
                   << LogIO::SEVERE << "Unhandled time reference frame: "
                    << timsys_p << LogIO::POST;
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
        freqsys_p = MFrequency::LSRK; // because some smart people use only LSR
        if (frame.contains("LSRD")) // in uvfits !
            freqsys_p = MFrequency::LSRD;
    } else if (frame.contains("REST")) {
        freqsys_p = MFrequency::REST;
    } else if (frame.contains("BARY")) {
        freqsys_p = MFrequency::BARY;
    } else if (frame.contains("GEO")) {
        freqsys_p = MFrequency::GEO;
    } else if (frame.contains("TOPO")) {
        freqsys_p = MFrequency::TOPO;
    } else if (frame.contains("GALAC")) {
        freqsys_p = MFrequency::GALACTO;
    } else if (frame.contains("LOCAL") || frame.contains("LGROUP")) {
        freqsys_p = MFrequency::LGROUP;
    } else if (frame.contains("CMB")) {
        freqsys_p = MFrequency::CMB;
    }
}

void MSFitsInput::updateSpectralWindowTable() {

    MSSpWindowColumns& msSpW(msc_p->spectralWindow());
    msSpW.measFreqRef().fillColumn(freqsys_p);

}

void MSFitsInput::checkRequiredAxis() {
   // Check if required axes are there
   if (getIndex(coordType_p, "COMPLEX") < 0) {
       itsLog << "Data does not have a COMPLEX axis" << LogIO::EXCEPTION;
   }
   if (getIndex(coordType_p, "STOKES") < 0) {
       itsLog << "Data does not have a STOKES axis" << LogIO::EXCEPTION;
   }
   if (getIndex(coordType_p, "FREQ") < 0) {
       itsLog << "Data does not have a FREQ axis" << LogIO::EXCEPTION;
   }
   if ((getIndex(coordType_p, "RA") < 0) && (getIndex(coordType_p, "RA---SIN")
           < 0) && (getIndex(coordType_p, "RA---NCP") < 0) && (getIndex(
           coordType_p, "RA---SCP") < 0)) {
       itsLog << "Data does not have a RA axis" << LogIO::EXCEPTION;
   }
   if ((getIndex(coordType_p, "DEC") < 0)
           && (getIndex(coordType_p, "DEC--SIN") < 0) && (getIndex(
           coordType_p, "DEC--NCP") < 0) && (getIndex(coordType_p, "DEC--SCP")
           < 0)) {
       itsLog << "Data does not have a DEC axis" << LogIO::EXCEPTION;
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
         itsLog << "This is not a uv table!" << LogIO::EXCEPTION;
    }
    const Int nAxis = kwl(FITS::TFIELDS)->asInt();
    if (nAxis < 1) {
        itsLog << "Data has no axes!" << LogIO::EXCEPTION;
    }

    String tdim = kwl("TDIM")->asString();
    //cout << "tdim=" << tdim << endl;
    IPosition ipos;
    FITSKeywordUtil::fromTDIM(ipos, tdim);

    uInt shp =ipos.nelements();
    //cout << "ipos=" << shp << endl;

    nPixel_p.resize(shp);
    refVal_p.resize(shp);
    refPix_p.resize(shp);
    delta_p.resize(shp);
    coordType_p.resize(shp);

    for (uInt i = 0; i < shp; i++) {
        nPixel_p(i) = ipos(i);
        if (nPixel_p(i) < 0) {
            itsLog << "Axes " << i << " cannot have a negative value"
                    << LogIO::EXCEPTION;
        }
        const char* tmp;

        tmp = (String::toString(i + 1).append("CTYP").append(String::toString(nAxis))).chars();
        coordType_p(i) = String(kwl(tmp)->asString()).before(trailing);

        tmp = (String::toString(i + 1).append("CRVL").append(String::toString(nAxis))).chars();
        refVal_p(i) = kwl(tmp)->asDouble();

        tmp = (String::toString(i + 1).append("CRPX").append(String::toString(nAxis))).chars();
        refPix_p(i) = kwl(tmp)->asDouble();

        tmp = (String::toString(i + 1).append("CDLT").append(String::toString(nAxis))).chars();
        delta_p(i) = kwl(tmp)->asDouble();

        //tmp = (String::toString(i + 1).append("CROT").append(String::toString(nAxis))).chars();
        //cRot_p(i) = kwl(tmp)->asDouble();
    }
    itsLog << LogOrigin("MSFitsInput", "fillMSMainTable")
               << LogIO::DEBUG1
               << "coordType=" << coordType_p
               << "\nrefVal=" << refVal_p
               << "\nrefPix=" << refPix_p
               << "\ndelta=" << delta_p
               << "\nnPixel_p=" << nPixel_p
               << LogIO::POST;

}

void MSFitsInput::sortPolarizations() {
    // Sort out the order of the polarizations and find the sort indices
    // to put them in 'standard' order: PP,PQ,QP,QQ
    const uInt iPol = getIndex(coordType_p, "STOKES");
    const uInt numCorr = nPixel_p(iPol);
    corrType_p.resize(numCorr);
    for (uInt i = 0; i < numCorr; i++) {
        // note: 1-based ref pix
        corrType_p(i) = ifloor(refVal_p(iPol) + (i + 1 - refPix_p(iPol))
                * delta_p(iPol) + 0.5);
        // convert AIPS-convention Stokes description to Casacore enum
        switch (corrType_p(i)) {
        case -8:
            corrType_p(i) = Stokes::YX;
            break;
        case -7:
            corrType_p(i) = Stokes::XY;
            break;
        case -6:
            corrType_p(i) = Stokes::YY;
            break;
        case -5:
            corrType_p(i) = Stokes::XX;
            break;
        case -4:
            corrType_p(i) = Stokes::LR;
            break;
        case -3:
            corrType_p(i) = Stokes::RL;
            break;
        case -2:
            corrType_p(i) = Stokes::LL;
            break;
        case -1:
            corrType_p(i) = Stokes::RR;
            break;
        default:
            if (corrType_p(i) < 0) {
                itsLog << "Unknown Correlation type: " << corrType_p(i)
                        << LogIO::EXCEPTION;
            }
        }
    }
    Vector<Int> tmp(corrType_p.copy());
    // Sort the polarizations to standard order. Could probably use
    // GenSortIndirect here.
    GenSort<Int>::sort(corrType_p);
    corrIndex_p.resize(numCorr);
    // Get the sort indices to rearrange the data to standard order
    for (uInt i = 0; i < numCorr; i++) {
        for (uInt j = 0; j < numCorr; j++) {
            if (corrType_p(j) == tmp(i))
                corrIndex_p[i] = j;
        }
    }
    
    // Figure out the correlation products from the polarizations
    corrProduct_p.resize(2, numCorr);
    corrProduct_p = 0;
    for (uInt i = 0; i < numCorr; i++) {
        const Stokes::StokesTypes cType = Stokes::type(corrType_p(i));
        Fallible<Int> receptor = Stokes::receptor1(cType);
        Bool warn = False;
        if (receptor.isValid()) {
            corrProduct_p(0, i) = receptor;
        } else if (!warn) {
            warn = True;
            itsLog << LogIO::WARN
                    << "Cannot deduce receptor 1 for correlations of type: "
                    << Stokes::name(cType) << LogIO::POST;
        }
        receptor = Stokes::receptor2(cType);
        if (receptor.isValid()) {
            corrProduct_p(1, i) = receptor;
        } else if (!warn) {
            warn = True;
            itsLog << LogIO::WARN
                    << "Cannot deduce receptor 2 for correlations of type: "
                    << Stokes::name(cType) << LogIO::POST;
        }
    }
}

void MSFitsInput::fillPolarizationTable() {
    MSPolarizationColumns& msPol(msc_p->polarization());
    Int nCorr = nPixel_p(getIndex(coordType_p, "STOKES"));

    // fill out the polarization info (only single entry allowed in fits input)
    ms_p.polarization().addRow();
    msPol.numCorr().put(0, nCorr);
    msPol.corrType().put(0, corrType_p);
    msPol.corrProduct().put(0, corrProduct_p);
    msPol.flagRow().put(0, False);
}

void MSFitsInput::fillSpectralWindowTable(BinaryTable& bt) {

    MSSpWindowColumns& msSpW(msc_p->spectralWindow());
    MSDataDescColumns& msDD(msc_p->dataDescription());

    const Regex trailing(" *$"); 
    ConstFitsKeywordList kwl = bt.kwlist();
    const FitsKeyword* kw;
    kwl.first();
    Int nIF = (kw = kwl("NO_IF")) ? kw->asInt() : 1;
    nIF_p = nIF;

    Table fqTab = bt.fullTable();
    Int nRow = fqTab.nrow();
    ROScalarColumn<Int> colFrqSel(fqTab, "FRQSEL");
    Matrix<Double> ifFreq(nIF_p, nRow);
    Matrix<Float> chWidth(nIF_p, nRow);
    Matrix<Float> totalBandwidth(nIF_p, nRow);
    // The type of the column changes according to the number of entries
    if (nIF_p == 1) {
        ROScalarColumn<Double> colIFFreq(fqTab, "IF FREQ");
        ROScalarColumn<Float> colChWidth(fqTab, "CH WIDTH");
        ROScalarColumn<Float> colTotalBandwidth(fqTab, "TOTAL BANDWIDTH");
        for (Int i = 0; i < nRow; i++) {
            ifFreq(0, i) = colIFFreq(i);
            chWidth(0, i) = colChWidth(i);
            totalBandwidth(0, i) = colTotalBandwidth(i);
        }
    }
    else {
        ROArrayColumn<Double> colIFFreq(fqTab, "IF FREQ");
        ROArrayColumn<Float> colChWidth(fqTab, "CH WIDTH");
        ROArrayColumn<Float> colTotalBandwidth(fqTab, "TOTAL BANDWIDTH");
        colIFFreq.getColumn(ifFreq);
        colChWidth.getColumn(chWidth);
        colTotalBandwidth.getColumn(totalBandwidth);
    }

    Int nSpW = nIF;
    Int iFreq = getIndex(coordType_p, "FREQ");
    Int nChan = nPixel_p(iFreq);
    if (nChan > 0)
                addSourceTable_p = True;

    for (Int spw = 0; spw < nSpW; spw++) {
        ms_p.spectralWindow().addRow();
        ms_p.dataDescription().addRow();

        msDD.spectralWindowId().put(spw, spw);
        msDD.polarizationId().put(spw, 0);
        msDD.flagRow().put(spw, False);
        Int ifc = 0;
        Int freqGroup = 0;
        if (nIF_p > 0) {
            ifc = spw % nIF_p;
            freqGroup = spw / nIF_p;
        }
        Int fqRow = spw / max(1, nIF_p);
        if (fqRow != colFrqSel(fqRow) - 1)
            itsLog << LogOrigin("MSFitsInput", "fillSpectralWindowTable")
                   << LogIO::SEVERE
                    << "Trouble interpreting FQ table, id's may be wrong"
                    << LogIO::POST;
        msSpW.name().put(spw, "none");
        msSpW.ifConvChain().put(spw, ifc);
        msSpW.numChan().put(spw, nChan);
        Double refChan = refPix_p(iFreq);
        // using data from FQ table
        Double refFreq = refVal_p(iFreq) + ifFreq(ifc, fqRow);

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
    MSColumns& msc(*msc_p);
    const Regex trailing(" *$");

    ConstFitsKeywordList kwl = bt.kwlist();
    //FitsKeywordList pkw = kwl;
    //cout << "-----\n" << pkw << endl;
    const FitsKeyword* kw;
    kwl.first();

    // get the uv table column names
    Int nFields = (kw = kwl("TFIELDS")) ? kw->asInt() : -1;
    if (nFields == -1) {
        itsLog << LogOrigin("MSFitsInput", "fillMSMainTable")
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
    itsLog << LogOrigin("MSFitsInput", "fillMSMainTable") << LogIO::DEBUG1
            << "TType=" << TType << "\nTScal=" << TScal << "\nTZero=" << TZero
            << LogIO::POST;

    Int nCorr = nPixel_p(getIndex(coordType_p, "STOKES"));
    Int nChan = nPixel_p(getIndex(coordType_p, "FREQ"));
    Int ns = max(1, nIF_p);
    Int nrows = bt.nrows();

    long estStor = nrows * ns / 1024 * 
        (7 * 8 + 11 * 4 + (2 * nCorr + 3 * nCorr * nChan) * 4 + 
         nCorr * nChan + 1);
    float needS = estStor / 1024.;
    //cout << " --------msFile_p=" << msFile_p << endl;
    Directory curD(msFile_p);
    float freeS = curD.freeSpaceInMB();
 
    itsLog << LogOrigin("MSFitsInput", "MSFitsInput")
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
    Int iTime0 = getIndex(TType, "DATE");
    Int iSource = getIndex(TType, "SOURCE");
    Int iFreq = getIndex(TType, "FREQSEL");
    Int iVis = getIndex(TType, "VISIBILITIES");



    //receptorAngle_p.resize(1);
    //nAnt_p = 0;

    itsLog << LogIO::NORMAL << "Fill MS Main Table of " << nrows
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
    nArray_p = -1;

    Bool lastRowFlag = False;

    for (Int group = 0; group < nrows; group++) {
        const Table tb = (group < 1) ? bt.thisRow() : bt.nextRow();
        //if (group == 0)
        //tb.deepCopy("nname", Table::New);

        //const TableDesc td = tb.tableDesc();
        //td.show();
        //cout << "nr=" << tb.nrow() << " nc=" << tb.isNull() << endl;

        try {
            ROScalarColumn<Float> colDate(tb, TType(iTime0));
            ROScalarColumn<Float> colUU(tb, TType(iU));
            ROScalarColumn<Float> colVV(tb, TType(iV));
            ROScalarColumn<Float> colWW(tb, TType(iW));
            ROScalarColumn<Float> colBL(tb, TType(iBsln));
            ROArrayColumn<Float> colVis(tb, TType(iVis));

            Int visL = 1;
            for (uInt i = 0; i < nPixel_p.nelements(); i++)
                visL *= nPixel_p(i);
            Vector<Float> visib(visL);

            visib = colVis.getColumn();
            //cout << "visib=" << visib << endl;

            Double time = (colDate.asdouble(0) - 2400000.5) * C::day ;
            //Double time = colDate.asdouble(0);
            //Time tm(time);
            //cout << "time=" << time
            //        << " tf=" << time - (TZero(iTime0))
            //        <<  " tm="  << tm << endl;


            // Extract fqid
            Int freqId = 1;
            if (iFreq >= 0) {
                ROScalarColumn<Float> colFrqSel(tb, TType(iFreq));
                freqId = (Int) colFrqSel.asfloat(0);
            }

            // Extract field Id
            Int fieldId = 0;
            if (iSource >= 0) {
                ROScalarColumn<Float> colSU(tb, TType(iSource));
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
            Float baseline = colBL.asfloat(0);
            Int arrayId = Int(100.0 * (baseline - Int(baseline) + 0.001));
            nArray_p = max(nArray_p, arrayId + 1);

            Int ant1 = Int(baseline) / 256;
            nAnt_p = max(nAnt_p, ant1);
            Int ant2 = Int(baseline) - ant1 * 256;
            nAnt_p = max(nAnt_p, ant2);
            ant1--;
            ant2--; // make 0-based

            // Ensure arrayId-specific params are of correct length:
            if (scanNumber.shape() < nArray_p) {
                scanNumber.resize(nArray_p, True);
                lastFieldId.resize(nArray_p, True);
                lastFreqId.resize(nArray_p, True);
                scanNumber(nArray_p - 1) = 0;
                lastFieldId(nArray_p - 1) = -1;
                lastFreqId(nArray_p - 1) = -1;
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
            Bool polFastest = (getIndex(coordType_p, "STOKES") < getIndex(
                    coordType_p, "FREQ"));
            const Int nx = (polFastest ? nChan : nCorr);
            const Int ny = (polFastest ? nCorr : nChan);

            Int count = 0;
            for (Int ifno = 0; ifno < max(1, nIF_p); ifno++) {
                // IFs go to separate rows in the MS
                ms_p.addRow();
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
                        const Int pol = (polFastest ? corrIndex_p[iy]
                                : corrIndex_p[ix]);
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
                    if (nIF_p > 0) {
                        spW *= nIF_p;
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
        } catch (AipsError x) {
            itsLog << LogOrigin("MSFitsInput", "fillMSMainTable")
                    << "Exception while filling MS main table. " << x.getMesg()
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
    receptorAngle_p.resize(2 * nAnt_p);
    receptorAngle_p = 0;
    // set the Measure References

    if (iSource < 0) {
        //cout << "fill field---------" 
        //       << "\ncoordType=" << coordType_p
        //       << "\nrefVal=" << refVal_p
        //       << "\nrefPix=" << refPix_p
        //       << "\ndelta=" << delta_p
        //       << "\nnPixel_p=" << nPixel_p
        //       << endl;
        double ra=0.;
        double dec=0.;
        ra = refVal_p(getIndex(coordType_p, "RA"));
        dec = refVal_p(getIndex(coordType_p, "DEC"));
        fillFieldTable(ra, dec, object);

    }


}

void MSFitsInput::fillObservationTable(ConstFitsKeywordList& kwl) {
    const FitsKeyword* kw;
    const Regex trailing(" *$"); // trailing blanks
    kwl.first();
    ms_p.observation().addRow();
    String observer;
    observer = (kw = kwl(FITS::OBSERVER)) ? kw->asString() : "";
    observer = observer.before(trailing);
    MSObservationColumns msObsCol(ms_p.observation());
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
    obsTime(0) = times(0);
    obsTime(1) = times(1);

    msObsCol.timeRange().put(0, times);
    msObsCol.releaseDate().put(0, timeRel.second());
    msObsCol.flagRow().put(0, False);

}

void MSFitsInput::fillPointingTable() {
    // fill the pointing table.  run though the main table, find field changes,
    // and add pointing rows as needed by looking up the field info in the field table


    if (addSourceTable_p)
        itsLog << LogOrigin("MSFitsInput", "fillPointingTable")
                << LogIO::NORMAL << "Filling Pointing table." << LogIO::POST;

    Int nrow = ms_p.nrow();
    Int nAnt = ms_p.antenna().nrow();
    Int lastFieldId = -1;

    Double lastTime = 0;
    Vector<Int> fieldId = msc_p->fieldId().getColumn();
    Vector<Int> ddId;
    if (addSourceTable_p)
        ddId = msc_p->dataDescId().getColumn();

    ProgressMeter meter(0.0, nrow * 1.0, "UVFITS Filler", "rows copied", "",
            "", True, nrow / 100);

    for (Int i = 0; i < nrow; i++) {
        if (fieldId(i) != lastFieldId) {
            lastFieldId = fieldId(i);
            if (i > 0)
                lastTime = msc_p->time()(i - 1);
            Array<Double> pointingDir = msc_p->field().phaseDir()(lastFieldId);
            String name = msc_p->field().name()(lastFieldId);
            Int numPoly = msc_p->field().numPoly()(lastFieldId);
            Double time = msc_p->time()(i);
            Int np = ms_p.pointing().nrow();
            if (np > 0) {
                // fix up time and interval for previous entries
                Double midTime = (lastTime + msc_p->pointing().time()(np - 1))
                        / 2;
                Double interval = lastTime - msc_p->pointing().time()(np - 1)
                        + msc_p->interval()(i - 1);
                for (Int j = 0; j < nAnt; j++) {
                    msc_p->pointing().time().put(np - j - 1, midTime);
                    msc_p->pointing().timeOrigin().put(np - j - 1, midTime);
                    msc_p->pointing().interval().put(np - j - 1, interval);
                }
            }

            for (Int j = 0; j < nAnt; j++) {
                ms_p.pointing().addRow();
                msc_p->pointing().antennaId().put(np + j, j);
                if (j == 0) {
                    msc_p->pointing().time().put(np + j, time);
                    msc_p->pointing().timeOrigin().put(np + j, time);
                    msc_p->pointing().interval().put(np + j, 0);
                    msc_p->pointing().name().put(np + j, name);
                    msc_p->pointing().numPoly().put(np + j, numPoly);
                    msc_p->pointing().direction().put(np + j, pointingDir);
                    msc_p->pointing().target().put(np + j, pointingDir);
                    msc_p->pointing().tracking().put(np + j, True);
                }
            }

        }
        meter.update((i + 1) * 1.0);
    }

    // fix up last interval
    lastTime = msc_p->time()(nrow - 1);
    Int np = ms_p.pointing().nrow();
    if (np > 0) {
        // fix up time and interval for previous entries
        Double midTime = (lastTime + msc_p->pointing().time()(np - 1)) / 2;
        Double interval = lastTime - msc_p->pointing().time()(np - 1)
                + msc_p->interval()(nrow - 1);
        for (Int j = 0; j < nAnt; j++) {
            msc_p->pointing().time().put(np - j - 1, midTime);
            msc_p->pointing().timeOrigin().put(np - j - 1, midTime);
            msc_p->pointing().interval().put(np - j - 1, interval);
        }
    }
}

void MSFitsInput::fillSourceTable() {

    itsLog << LogOrigin("MSFitsInput", "fillSourceTable") << LogIO::NORMAL
            << "Filling SOURCE table." << LogIO::POST;
    Int numRow = 1;
    if (numRow > 0) {
        String tName = ms_p.tableName();
        //cout << "ms name=" << tName << endl;
        MSSummary mss(&ms_p, tName);

        Record mainRec;
        mss.listMain(itsLog, mainRec);
        //cout << "mainRec=\n" << mainRec << endl;
        //cout << "num of scans=" << mainRec.nfields() << endl;

        //Record fieldRec;
        //mss.listField(itsLog, fieldRec, True);
        //cout << "fieldRec=\n" << fieldRec << endl;
        //cout << "num of fields=" << fieldRec.nfields() << endl;

        ProgressMeter meter(0.0, mainRec.nfields() * 1.0, "UVFITS Filler",
                "rows copied", "", "", True, mainRec.nfields() * 300 / 100);

        for (uInt i = 0; i < mainRec.nfields() - 5; i++) {
            Int fnum = mainRec.fieldNumber(String("scan_").append(
                    String::toString(i + 1)));
            Record rec = mainRec.subRecord(fnum).subRecord(String('0'));
            //cout << "subRecord=\n" << rec << endl;

            Double time1 = rec.asDouble("BeginTime");
            Double time2 = rec.asDouble("IntegrationTime");
            Int fid = rec.asInt("FieldId");
            String name = rec.asString("FieldName");

            //Int numPoly = ;
            Vector<Int> spwIds = rec.asArrayInt("SpwIds");

            //cout << "time=" << time << " fid=" << fid << " name=" << name
            //        << " spwIds=" << spwIds << endl;
            for (uInt spwId = 0; spwId < spwIds.nelements(); spwId++) {
                MSSourceIndex sourceIndex(ms_p.source());
                sourceIndex.sourceId() = fid;
                sourceIndex.spectralWindowId() = spwIds(spwId);

                Vector<uInt> rows = sourceIndex.getRowNumbers();
                if (rows.nelements() == 0) {
                    ms_p.source().addRow();
                    Int j = ms_p.source().nrow() - 1;
                    MSSourceColumns & msc = msc_p->source();
                    msc.sourceId().put(j, fid);
                    msc.name().put(j, name);
                    Matrix<Double> phaseDir = msc_p->field().phaseDir()(fid);
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
                    restFreqs(0) = restfreq_p;
                    if (restFreqs(0) <= 0.0) {
                        restFreqs(0) = msc_p->spectralWindow().refFrequency()(
                                spwId);
                    }
                    msc.restFrequency().put(j, restFreqs);
                    msc.calibrationGroup().put(j, -1);
                    //String code = msc_p->field().c.code().asString();
                    msc.code().put(j, msc_p->field().code()(fid));

                }
                //meter.update((i + 1) * 1.0);
            }
        }
    }
    else {

        //////////////////this is uselessly slow
        // fill the source table. run though the main table look for new spectralwindows
        // and add source table entries for each field/spw combination
        Int nrow = ms_p.nrow();
        Int lastFieldId = -1;
        Int lastDDId = -1;
        Vector<Int> fieldId = msc_p->fieldId().getColumn();
        Vector<Int> ddId = msc_p->dataDescId().getColumn();

        ProgressMeter meter(0.0, nrow * 1.0, "UVFITS Filler", "rows copied",
                "", "", True, nrow / 100);

        for (Int i = 0; i < nrow; i++) {
            if (fieldId(i) != lastFieldId || (ddId(i) != lastDDId)) {
                lastFieldId = fieldId(i);
                Array<Double> pointingDir = msc_p->field().phaseDir()(
                        lastFieldId);
                String name = msc_p->field().name()(lastFieldId);
                //Int numPoly = msc_p->field().numPoly()(lastFieldId);
                Double time = msc_p->time()(i);

                lastDDId = ddId(i);
                Int spwId = msc_p->dataDescription().spectralWindowId()(
                        lastDDId);
                // now check if we've seen this field for this spectral window
                // Use indexed access to the SOURCE sub-table
                MSSourceIndex sourceIndex(ms_p.source());
                sourceIndex.sourceId() = lastFieldId;
                sourceIndex.spectralWindowId() = spwId;
                Vector<uInt> rows = sourceIndex.getRowNumbers();
                if (rows.nelements() == 0) {
                    ms_p.source().addRow();
                    Int j = ms_p.source().nrow() - 1;
                    MSSourceColumns & mss = msc_p->source();
                    mss.sourceId().put(j, lastFieldId);
                    msc_p->field().sourceId().put(lastFieldId, lastFieldId);
                    mss.name().put(j, name);
                    Matrix<Double> phaseDir = msc_p->field().phaseDir()(
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
                    restFreqs(0) = restfreq_p;
                    if (restFreqs(0) <= 0.0) {
                        // put in the reference freq as default for the rest frequency
                        restFreqs(0) = msc_p->spectralWindow().refFrequency()(
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

    MSFieldColumns& msField(msc_p->field());
    // Table suTab=bt.fullTable("",Table::Scratch);
    Table suTab = bt.fullTable();
    ROScalarColumn<Int> id(suTab, "ID. NO.");
    ROScalarColumn<String> name(suTab, "SOURCE");
    ROScalarColumn<Int> qual(suTab, "QUAL");
    Bool multiqual = False;
    Int minqual, maxqual;
    minMax(minqual, maxqual, qual.getColumn());
    if (minqual != maxqual)
        multiqual = True;
    ROScalarColumn<String> code(suTab, "CALCODE");
    // ROScalarColumn<Float> iflux(suTab,"IFLUX"); // etc Q, U, V (Jy)
    ROScalarColumn<Double> ra(suTab, "RAEPO"); //degrees
    ROScalarColumn<Double> dec(suTab, "DECEPO"); //degrees
    ROScalarColumn<Double> raapp(suTab, "RAAPP"); //degrees
    ROScalarColumn<Double> decapp(suTab, "DECAPP"); //degrees
    ROScalarColumn<Double> epoch(suTab, "EPOCH"); //years
    ROScalarColumn<Double> pmra(suTab, "PMRA"); //deg/day
    ROScalarColumn<Double> pmdec(suTab, "PMDEC"); //deg/day
    if (Int(suTab.nrow()) < nField) {
        itsLog << LogOrigin("MSFitsInput", "fillFieldTable")
               << LogIO::NORMAL
                << "Input Source id's not sequential, adding empty rows in output"
                << LogIO::POST;
    }
    Int outRow = -1;

    // RESTFREQ and LSRVEL are 2D columns according to the AIPS Memo 117
    restFreq_p.resize(noif, suTab.nrow());
    sysVel_p.resize(noif, suTab.nrow());
    try{
      ROArrayColumn<Double> restfreq(suTab,"RESTFREQ");  // Hz
      ROArrayColumn<Double> sysvel(suTab,"LSRVEL"); // m/s
      restfreq.getColumn(restFreq_p);
      sysvel.getColumn(sysVel_p);
    }
    catch (std::exception x) {
      if(noif>1){
	itsLog << LogOrigin("MSFitsInput", "fillFieldTable") << LogIO::WARN
	       << x.what() << ": " << "Inconsistent setup of RESTFREQ and LSRVEL columns." << endl
	       << "With NO_IF>1, they should be arrays not scalars." << LogIO::POST;
      }
      ROScalarColumn<Double> restfreq(suTab,"RESTFREQ");  // Hz
      ROScalarColumn<Double> sysvel(suTab,"LSRVEL"); // m/s
      Vector<Double> tmprf(suTab.nrow());
      Vector<Double> tmpsv(suTab.nrow());
      restfreq.getColumn(tmprf);
      sysvel.getColumn(tmpsv);
      for(uInt ii=0; ii<suTab.nrow(); ii++){
	restFreq_p(0,ii) = tmprf(ii);
	sysVel_p(0,ii) = tmpsv(ii);
      }
    }      

    // set the DIRECTION MEASURE REFERENCE for appropriate columns
    MDirection::Types epochRefZero = getDirectionFrame(epoch(0));
    if (epochRefZero != epochRef_p)
        itsLog << LogOrigin("MSFitsInput", "fillFieldTable")
               << LogIO::WARN << "The direction measure reference code, "
                << epochRefZero << "\n"
                << "for the first field does not match the one from the FITS header, "
                << epochRef_p
                << ".\nThis might cause a problem for the reference frame"
                << " of the output's UVW column." << LogIO::POST;

    for (Int inRow = 0; inRow < (Int) suTab.nrow(); inRow++) {
        Int fld = id(inRow) - 1;
        // add empty rows until the row number in the output matches the source id
        while (fld > outRow) {
            // Append a flagged, empty row to the FIELD table
            ms_p.field().addRow();
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
                if (array_p == "VLA")
                    epochRef = MDirection::B1950_VLA;
                else
                    epochRef = MDirection::B1950;
            } else if (epoch(inRow) == -1.0) {
                epochRef = epochRefZero;
                itsLog << LogOrigin("MSFitsInput", "fillFieldTable")
                       << " Assuming standard epoch " << " for " << name(inRow)
                        << ".  Be aware that this may not be correct." << endl;
            } else {
                itsLog << LogOrigin("MSFitsInput", "fillFieldTable")
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

        msField.time().put(fld, obsTime(0));
        msField.numPoly().put(fld, numPoly);
        msField.delayDirMeasCol().put(fld, radecMeas);
        msField.phaseDirMeasCol().put(fld, radecMeas);
        msField.referenceDirMeasCol().put(fld, radecMeas);
        msField.flagRow().put(fld, False);
    }


}

void MSFitsInput::fillFieldTable(double ra, double dec, String source) {
    MSFieldColumns& msField(msc_p->field());

    ms_p.field().addRow();

    msField.sourceId().put(0, 0); 
    msField.code().put(0, "");
    msField.name().put(0, source);
    Int numPoly = 0;

    //MDirection::Types epochRef = MDirection::APP;
    MDirection::Types epochRef = epochRef_p;
    MVDirection refDir;

    refDir = MVDirection(ra * C::degree, dec * C::degree);
    Vector<MDirection> radecMeas(1);
    radecMeas(0).set(refDir, MDirection::Ref(epochRef));

    msField.time().put(0, obsTime(0));
    msField.numPoly().put(0, numPoly);
    msField.delayDirMeasCol().put(0, radecMeas);
    msField.phaseDirMeasCol().put(0, radecMeas);
    msField.referenceDirMeasCol().put(0, radecMeas);
    msField.flagRow().put(0, False);

}

} //# NAMESPACE CASACORE - END

