//# MSFITSOutput: MS to UVFITS
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

#include <casacore/msfits/MSFits/MSFitsOutput.h>
#include <casacore/msfits/MSFits/MSFitsOutputAstron.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/ms/MSOper/MSMetaData.h>
#include <casacore/tables/Tables.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/fits/FITS/fitsio.h>
#include <casacore/fits/FITS/FITSTable.h>
#include <casacore/fits/FITS/FITSDateUtil.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/MatrixMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/Quanta/Euler.h>
#include <casacore/measures/Measures/Stokes.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/Tables/TableIter.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/casa/System/ProgressMeter.h>
#include <casacore/tables/LogTables/NewFile.h>

#include <casacore/casa/stdlib.h> // for atoi()
#include <casacore/casa/sstream.h>
#include <casacore/casa/iomanip.h>

#include <casacore/casa/Logging/LogIO.h>

#include <set>
#include <limits>

namespace casacore {

MSFitsOutput::MSFitsOutput(
    const String& fitsfile, const MeasurementSet& ms,
    const String& column
) : _fitsfile(fitsfile), _column(column), _ms(ms),
    _startChan(0), _nchan(1), _stepChan(1), _avgChan(1),
    _writeSysCal(False), _asMultiSource(False), _combineSpw(False),
    _writeStation(False), _padWithFlags(False), _overwrite(False),
    _sensitivity(1.0), _fieldNumber(0) {}

void MSFitsOutput::setChannelInfo(
    Int startChan, Int nchan, Int stepChan, Int avgChan
) {
    _startChan = startChan;
    _nchan = nchan;
    _stepChan = stepChan;
    _avgChan = avgChan;
}

void MSFitsOutput::setWriteSysCal(Bool s) {
    _writeSysCal = s;
}

void MSFitsOutput::setAsMultiSource(Bool asMultiSource) {
    _asMultiSource = asMultiSource;
}

void MSFitsOutput::setCombineSpw(Bool combineSpw) {
    _combineSpw = combineSpw;
}

void MSFitsOutput::setWriteStation(Bool writeStation) {
    _writeStation = writeStation;
}

void MSFitsOutput::setSensitivity(Double sensitivity) {
    _sensitivity = sensitivity;
}

void MSFitsOutput::setPadWitFlags(Bool padWithFlags) {
    _padWithFlags = padWithFlags;
}

void MSFitsOutput::setFieldNumber(uInt fieldNumber) {
    _fieldNumber = fieldNumber;
}

void MSFitsOutput::setOverwrite(Bool overwrite) {
    _overwrite = overwrite;
}

static String toFITSDate(const MVTime &time) {
    String date, timesys;
    FITSDateUtil::toFITS(date, timesys, time);
    return date;
}

// MJD seconds to day number and day fraction
void MSFitsOutput::timeToDay(Int &day, Double &dayFraction, Double time) {
    const Double JDofMJD0 = 2400000.5;
    time /= C::day; // now in days;
    time += JDofMJD0; // now in JD
    day = Int(time);
    dayFraction = time - floor(time);
}

void MSFitsOutput::write() const {
    MSObservationColumns obsCols(_ms.observation());
    if (obsCols.nrow() > 0 && obsCols.telescopeName()(0) == "WSRT") {
        ThrowIf(
            ! MSFitsOutputAstron::writeFitsFile(_fitsfile, _ms, _column,
                _startChan, _nchan, _stepChan, _writeSysCal, _asMultiSource,
                _combineSpw, _writeStation, _sensitivity
            ), "Unable to write Astron-specific UVFITS file"
        );
        return;
    }
    LogIO os(LogOrigin("MSFitsOutput", __func__));
    os << LogIO::NORMAL << " nchan=" << _nchan << " startchan=" << _startChan
        << " stepchan=" << _stepChan << " avgchan=" << _avgChan << LogIO::POST;
    const uInt nrow = _ms.nrow();
    String msfile = _ms.tableName();
    String outfile = _fitsfile;
    if (outfile.empty()) {
        outfile = msfile.contains(Regex("\\.ms$"))
            ? msfile.before(Regex("\\.ms"), 0) + ".fits"
            : msfile + ".fits";
    }
    String errmsg;
    if (_overwrite && File(outfile).exists()) {
        RegularFile(outfile).remove();
        os << LogIO::NORMAL << "Removing existing file "
            << outfile << LogIO::POST;
    }
    else if (! _overwrite) {
        NewFile fileOK(True);
        ThrowIf (
            ! fileOK.valueOK(outfile, errmsg),
            "Error in output file : " + errmsg
        );
    }

    os << LogIO::NORMAL << "Converting MeasurementSet " << _ms.tableName()
        << " to FITS file '" << outfile << "'" << LogIO::POST;

    // Determine if this MS is a subset of a main MS.
    Bool isSubset = nrow != (1 + max(_ms.rowNumbers()));
    if (isSubset) {
        os << LogIO::NORMAL << "MS " << _ms.tableName()
            << " is a subset of another MS" << LogIO::POST;
    }

    // Find the number of IF's (spectral-windows).
    Block<Int> spwidMap;
    Vector<Int> spwids;
    uInt nrspw;
    {
        MSMetaData md(&_ms, 100);
        std::set<uInt> spwIDs = md.getSpwIDs();
        Vector<Int> allids(spwIDs.size());
        copy(spwIDs.begin(), spwIDs.end(), allids.begin());
        nrspw = _makeIdMap(spwidMap, spwids, allids);
    }

    // If not asMultiSource, check if multiple sources are present.
    Block<Int> fieldidMap;
    uInt nrfield;
    Bool doMultiSource = _asMultiSource;
    {
        ScalarColumn<Int> fldidcol(_ms, MS::columnName(MS::FIELD_ID));
        Vector<Int> fldid = fldidcol.getColumn();
        if (! doMultiSource) {
            if (! allEQ(fldid, fldid(0))) {
                doMultiSource = True;
                os << LogIO::WARN
                    << "Multiple sources are present, thus written "
                    "as a multi-source FITS file" << LogIO::POST;
            }
        }
        Vector<Int> fieldids;
        nrfield = _makeIdMap(fieldidMap, fieldids, fldid);
    }

    // Write main table. Get freq and channel-width back.
    Int refPixelFreq;
    Double refFreq, chanbw;
    auto fitsOutput = _writeMain(
        refPixelFreq, refFreq, chanbw, outfile,
        spwidMap, nrspw, fieldidMap, doMultiSource
    );
    ThrowIf (
        ! fitsOutput, "Could not write main table\n"
    );
    os << LogIO::NORMAL << "Writing AIPS FQ table" << LogIO::POST;
    ThrowIf(
        ! _writeFQ(
            fitsOutput, _ms, spwidMap, nrspw, refFreq, refPixelFreq,
            chanbw, _combineSpw, _startChan, _nchan, _stepChan, _avgChan
        ), "Could not write FQ table"
    );
    os << LogIO::NORMAL << "Writing AIPS AN table" << LogIO::POST;
    ThrowIf(
        ! _writeAN(fitsOutput, _ms, refFreq, _writeStation),
        "Could not write AN table"
    );
    if (doMultiSource) {
        os << LogIO::NORMAL << "Writing AIPS SU table" << LogIO::POST;
        if (
            ! _writeSU(fitsOutput, _ms, fieldidMap, nrfield, spwidMap, nrspw)
        ) {
            os << LogIO::NORMAL << "Could not write SU table" << LogIO::POST;
        }
    }
    // If needed, create tables from the SYSCAL table.
    // Determine if we have to skip the first SYSCAL time.
    // This is needed for WSRT MS's, where the first time in the SYSCAL
    // table is the average at the middle of the observation.
    if (_writeSysCal) {
        if (_ms.sysCal().tableDesc().ncolumn() == 0) {
            os << LogIO::WARN << "MS has no or empty SYSCAL subtable, "
                << "could not write AIPS TY table and AIPS GC table"
                << LogIO::POST;
        }
        else if (_ms.sysCal().nrow() == 0) {
            os << LogIO::WARN << "MS has empty SYSCAL subtable, "
                << "could not write AIPS TY table and AIPS GC table"
                << LogIO::POST;
        }
        else {
            Table syscal = handleSysCal(_ms, spwids, isSubset);
            os << LogIO::NORMAL << "writing AIPS TY table" << LogIO::POST;
            Bool bk = _writeTY(
                fitsOutput, _ms, syscal, spwidMap, nrspw, _combineSpw
            );
            if (! bk) {
                os << LogIO::WARN << "Could not write TY table\n"
                    << LogIO::POST;
            }
            else {
                os << LogIO::NORMAL << "Writing AIPS GC table" << LogIO::POST;
                bk = _writeGC(
                    fitsOutput, _ms, syscal, spwidMap, nrspw,
                    _combineSpw, _sensitivity, refPixelFreq, refFreq, chanbw
                );
            }
            if (!bk) {
                os << LogIO::WARN << "Could not write GC table\n"
                    << LogIO::POST;
            }
        }
    }
    if (_ms.weather().tableDesc().ncolumn() != 0) {
        os << LogIO::NORMAL << "Writing AIPS WX table" << LogIO::POST;
        Bool bk = _writeWX(fitsOutput, _ms);
        if (!bk) {
            os << LogIO::WARN << "Could not write WX table"
                << LogIO::POST;
        }
    }
    /*
     * FIXME there are still some issues with this, but I have to move on to
     * another issue for now, so commenting out SYSPOWER table support until
     * I can work on it again
     * dmehring 17feb2021
     *
    // support for adhoc NRAO SYSPOWER table
    static const String SYSPOWER = "SYSPOWER";
    File syspower_f( _ms.tableName() + "/" + SYSPOWER);
    if (_ms.keywordSet().isDefined(SYSPOWER)) {
        const auto tableName = _ms.keywordSet().asTable("SYSPOWER").tableName();
        if(Table::isReadable(tableName)) {
            Table syspower(tableName);
            os << LogIO::NORMAL << "Found SYSPOWER table" << LogIO::POST;
            if (_writeSY(fitsOutput, _ms, syspower, nrspw, spwidMap, _combineSpw)) {
                os << LogIO::NORMAL << "Wrote SY table" << LogIO::POST;
            }
            else {
                os << LogIO::WARN << "Could not write SY table" << LogIO::POST;
            }
        }
    }
    */
}

Bool MSFitsOutput::writeFitsFile(
	const String& fitsfile,
	const MeasurementSet& ms, const String& column, Int startchan,
	Int nchan, Int stepchan, Bool writeSysCal, Bool asMultiSource,
	Bool combineSpw, Bool writeStation, Double sensitivity,
	const Bool padWithFlags, Int avgchan, uInt fieldNumber,
	Bool overwrite
) {
    // A FITS table can handle only Int nrows.
    if (ms.nrow() > static_cast<rownr_t>(std::numeric_limits<Int>::max())) {
      throw AipsError("MS " + ms.tableName() + " is too big (#rows exceeds MAX_INT)");
    }
    MSFitsOutput out(fitsfile, ms, column);
    out.setChannelInfo(startchan, nchan, stepchan, avgchan);
    out.setWriteSysCal(writeSysCal);
    out.setAsMultiSource(asMultiSource);
    out.setCombineSpw(combineSpw);
    out.setWriteStation(writeStation);
    out.setSensitivity(sensitivity);
    out.setPadWitFlags(padWithFlags);
    out.setFieldNumber(fieldNumber);
    out.setOverwrite(overwrite);
    out.write();
    return True;
}

uInt MSFitsOutput::get_tbf_end(const uInt rownr, const uInt nrow,
        const uInt nif, const ScalarColumn<Double>& intimec,
        const ScalarColumn<Double>& timewidthcol,
        const ScalarColumn<Int>& inant1, const ScalarColumn<Int>& inant2,
        const Bool asMultiSource, const ScalarColumn<Int>& infieldid) {
    const Double maxTime = intimec(rownr) + 0.2 * timewidthcol(rownr);
    const Int startant1 = inant1(rownr);
    const Int startant2 = inant2(rownr);

    Int startfld = 0;
    if (asMultiSource)
        startfld = infieldid(rownr);

    uInt tbfend = rownr;
    for (uInt currrow = rownr + 1; currrow - rownr < nif && currrow < nrow; ++currrow) {
        if (intimec(currrow) <= maxTime && inant1(currrow) == startant1
                && inant2(currrow) == startant2 && (!asMultiSource
                || infieldid(currrow) == startfld))
            tbfend = currrow;
        else
            break;
    }
    return tbfend;
}

// Define a FITS random group parameter with default scaling and offset
static void defineRandomParam(Record& ek, Int n, const String& name) {
    String ptype = "ptype" + String::toString(n);
    String pscal = "pscal" + String::toString(n);
    String pzero = "pzero" + String::toString(n);
    ek.define(ptype, name);
    ek.define(pscal, 1.0);
    ek.define(pzero, 0.0);
}

static void defineRandomParam(Record& ek, Int n, const String& name,
        const String& comment) {
    String ptype = "ptype" + String::toString(n);
    defineRandomParam(ek, n, name);
    ek.setComment(ptype, comment);
}

std::shared_ptr<FitsOutput> MSFitsOutput::_writeMain(Int& refPixelFreq, Double& refFreq,
    Double& chanbw, const String &outFITSFile,
    const Block<Int>& spwidMap, Int nrspw,
    const Block<Int>& fieldidMap,
    Bool asMultiSource
) const {
    Int avgchan = _avgChan < 0 ? 1 : _avgChan;
    std::shared_ptr<FitsOutput> outfile(nullptr);
    LogIO os(LogOrigin("MSFitsOutput", __func__));
    const uInt nrow = _ms.nrow();
    if (nrow == 0) {
        os << LogIO::SEVERE << "Empty measurement set!" << LogIO::POST;
        return 0;
    }
    Record ek; // ek == extra keys
    Vector<Double> radec;
    String objectname;
    {
    	// field table info
    	MSField fieldTable(_ms.field());
    	MSFieldColumns msfc(fieldTable);
    	if (asMultiSource) {
    		// UVFITS expects zeros for multi-source
    		radec = Vector<Double> (2, 0.0);
    	}
    	else {
    		// Use the actual RA/Decl
    		radec = msfc.phaseDirMeas(_fieldNumber).getAngle().getValue();
    		radec *= 180.0 / C::pi; // convert to degrees for FITS
    		if (radec(0) < 0) {
    			radec(0) += 360.0;
    		}
    		objectname = msfc.name()(_fieldNumber);
    	}
    	uInt myfield = asMultiSource ? 0 : _fieldNumber;
    	Bool foundEpoch = False;
    	String dirtype = msfc.phaseDirMeas(myfield).getRefString();
    	if (dirtype.contains("2000")) {
    		ek.define("epoch", 2000.0);
    		foundEpoch = True;
    	}
    	else if (dirtype.contains("1950")) {
    		ek.define("epoch", 1950.0);
    		foundEpoch = True;
    	}
    	if (!foundEpoch) {
    		os << LogIO::SEVERE << "Cannot deduce MS epoch. Assuming J2000"
    			<< LogIO::POST;
    		ek.define("epoch", 2000.0);
    	}
    }
    // First scan the SPECTRAL_WINDOW table to make sure that the data
    // shape is constant, the correlation type is constant, and that the
    // frequencies can be represented as f = f0 + i*inc
    MSDataDescription ddTable = _ms.dataDescription();
    MSSpectralWindow spectralTable = _ms.spectralWindow();
    MSPolarization polTable = _ms.polarization();
    MSSource srcTable;
    uInt nsrc = 0;
    try {
        srcTable = _ms.source();
        nsrc = srcTable.nrow();
    }
    catch (const std::exception& x) {
        os << LogOrigin("MSFitsOutput", __func__)
           << LogIO::WARN << "No source table in MS. "
           << x.what() << LogIO::POST;
    }
    const uInt ndds = ddTable.nrow();
    const uInt nspec = spectralTable.nrow();
    const uInt npol = polTable.nrow();
    if (ndds == 0) {
        os << LogIO::SEVERE << "No data description table in MS" << LogIO::POST;
        return 0;
    }
    if (nspec == 0) {
        os << LogIO::SEVERE << "No spectral window table in MS" << LogIO::POST;
        return 0;
    }
    if (npol == 0) {
        os << LogIO::SEVERE << "No polarization table in MS" << LogIO::POST;
        return 0;
    }
    ScalarColumn<Int> spwId(ddTable, MSDataDescription::columnName(
            MSDataDescription::SPECTRAL_WINDOW_ID));
    ScalarColumn<Int> polId(ddTable, MSDataDescription::columnName(
            MSDataDescription::POLARIZATION_ID));
    ScalarColumn<Int> numcorr(polTable, MSPolarization::columnName(
            MSPolarization::NUM_CORR));
    ScalarColumn<Int> numchan(spectralTable, MSSpectralWindow::columnName(
            MSSpectralWindow::NUM_CHAN));
    ArrayColumn<Double> frequencies(spectralTable,
            MSSpectralWindow::columnName(MSSpectralWindow::CHAN_FREQ));
    ArrayColumn<Int> stokesTypes(polTable, MSPolarization::columnName(
            MSPolarization::CORR_TYPE));
    ScalarColumn<Double> totalbw(spectralTable, MSSpectralWindow::columnName(
            MSSpectralWindow::TOTAL_BANDWIDTH));

    ScalarColumn<Int> measFreq(spectralTable, MSSpectralWindow::columnName(
            MSSpectralWindow::MEAS_FREQ_REF));
    Double restFreq(0.0);
    if (nsrc > 0 && srcTable.isColumn(MSSource::REST_FREQUENCY)) {
        ArrayColumn<Double> restfreqcol(srcTable, MSSource::columnName(
            MSSource::REST_FREQUENCY));
        if (restfreqcol.isDefined(0) && restfreqcol(0).nelements() > 0) {
            IPosition ip = restfreqcol(0).shape();
            ip = 0;
            restFreq = restfreqcol(0)(ip);
        }
    }

    Vector<Int> antnumbers;
    _handleAntNumbers(_ms, antnumbers);
    Int maxant = max(antnumbers);

    // Also find out what the Stokes are and make sure that they are the same
    // throughout the MS. In principle we could handle the same stokes in
    // different order by transposing, but this may well never happen.
    Int numcorr0 = 0;
    Int numchan0 = 0;
    Double delta = 0;
    Double f0 = 0;
    Double bw0 = 0;
    Vector<Int> stokes;
    Int measFreq0 = -1;
    uInt i;
    Int nchan = _nchan;
    Int chanstep = _stepChan;
    Int chanstart = _startChan;
    for (i = 0; i < ndds; i++) {
        if (i < spwidMap.nelements() && spwidMap[i] >= 0) {
            const Int s = spwId(i);
            const Int p = polId(i);
            // Get channel width.
            Vector<Double> freqs = frequencies(s);
            if (freqs.nelements() > 1) {
                delta = freqs(1) - freqs(0);
            }
            else {
                delta = totalbw(0);
            }
            // If first time, set the various values.

            if (numcorr0 == 0) {
                numcorr0 = numcorr(p);
                numchan0 = numchan(s);
                if (numcorr0 <= 0 || numchan0 <= 0) {
                    os << LogIO::SEVERE
                            << "Number of correlations or channels is zero"
                            << LogIO::POST;
                    return 0;
                }
                f0 = freqs(0);
                bw0 = delta;
                chanbw = abs(delta);
                stokes = stokesTypes(p);
                if (
                    (_nchan > 0) && (_stepChan > 0) && (_startChan >= 0)
                    && ((_nchan * _stepChan + _startChan) <= numchan0)
                ) {
                    f0 = freqs(_startChan);
                    bw0 = delta * _stepChan;
                }
                else {
                    nchan = numchan0;
                    chanstep = 1;
                    chanstart = 0;
                }
                measFreq0 = measFreq(s);
            }

            // Check if values match.
            if (numcorr(p) != numcorr0) {
                os << LogIO::SEVERE
                        << "Number of correlations varies in the MS"
                        << LogIO::POST;
                return 0;
            }
            if (numchan(s) != numchan0) {
                os << LogIO::SEVERE << "Number of channels varies in the MS, i.e. the is more than one SPW shape!"
		   << endl << "Please split out SPWs of identical shape and export them separately."
                        << LogIO::POST;
                return 0;
            }
            if (!allEQ(stokes, stokesTypes(p))) {
                os << LogIO::SEVERE
                        << "Stokes types vary for different spectral windows"
                        << LogIO::POST;
                return 0;
            }
            if (!near(abs(delta), chanbw, 1.0e-5)) {
                os << LogIO::SEVERE
                        << "Bandwidth varies across spectral windows"
                        << LogIO::POST;
                return 0;
            }
            if (nchan > 1) {
                Vector<Double> selChans(nchan);
                for (uInt j = 0; j < (uInt)nchan; ++j) {
                    uInt k = chanstart + j * chanstep;
                    selChans(j) = freqs(k);
                }
                delta = selChans(1) - selChans(0);
                for (uInt j = 1; j < selChans.nelements(); ++j) {
                    if (!near(delta, selChans(j) - selChans(j - 1), 1.0e-5)) {
                        os << LogIO::SEVERE
                            << "Channel width varies across the band"
                            << LogIO::POST;
                        return 0;
                    }
                }
            }
            if (measFreq(s) != measFreq0) {
                os << LogIO::SEVERE << "Frequency frame varies in the MS"
                        << LogIO::POST;
                return 0;
            }
        }
    }

    Int f0RefPix(0);
    f0RefPix = 1 + nchan / 2;
    if (f0RefPix == 1) {
        // single-channel out
        refFreq = f0 + bw0 / 2.0 - delta / 2.0;
    }
    else {
        // multi-channel out  (f0RefPix is a *one* - based index!)
        refFreq = f0 + (f0RefPix - 1) * bw0;
    }
    refPixelFreq = f0RefPix;

    // OK, turn the stokes into FITS values.
    for (Int j = 0; j < numcorr0; j++) {
        stokes(j) = Stokes::FITSValue(Stokes::StokesTypes(stokes(j)));
    }

    // OK, get an index vector that sorts these in ascending order if
    // stokes(0) >= 0, or descending order if < 0.
    Vector<uInt> stokesIndex(numcorr0);
    if (stokes(0) >= 0) {
        GenSortIndirect<Int,uInt>::sort(stokesIndex, stokes);
    } else {
        GenSortIndirect<Int,uInt>::sort(stokesIndex, stokes, Sort::Descending);
    }

    // OK, make sure that we can represent the stokes in FITS
    if (stokes.nelements() > 2) {
        Int delta = stokes(stokesIndex(1)) - stokes(stokesIndex(0));
        for (i = 2; i < stokes.nelements(); i++) {
            if (stokes(stokesIndex(i)) - stokes(stokesIndex(i - 1)) != delta) {
                os << LogIO::SEVERE
                        << "These STOKES are not representable in FITS"
                        << LogIO::POST;
                return 0;
            }
        }
    }

    // DATA: COMPLEX(2)+WEIGHT, NUM_CORR, NUM_CHAN, IF, RA, DEC
    RecordDesc desc;
    String columnName;
    String col = _column;
    col.upcase();
    if (col == "OBSERVED" || col == MS::columnName(MS::DATA)) {
        columnName = MS::columnName(MS::DATA);
        os << "Writing DATA column" << LogIO::POST;
    } else if (col == "MODEL" || col == "MODEL_DATA") {
        if (_ms.tableDesc().isColumn("MODEL_DATA")) {
            columnName = "MODEL_DATA";
            os << "Writing MODEL_DATA column" << LogIO::POST;
        } else {
            columnName = MS::columnName(MS::DATA);
            os << LogIO::SEVERE << "MODEL_DATA does not exist, writing DATA"
                    << LogIO::POST;
        }
    } else if (col == "CORRECTED" || col == "CORRECTED_DATA") {
        if (_ms.tableDesc().isColumn("CORRECTED_DATA")) {
            columnName = "CORRECTED_DATA";
            os << "Writing CORRECTED_DATA column" << LogIO::POST;
        } else {
            columnName = MS::columnName(MS::DATA);
            os << LogIO::NORMAL
                    << "CORRECTED_DATA does not exist, writing DATA"
                    << LogIO::POST;
        }
    } else {
        columnName = MS::columnName(MS::DATA);
        os << LogIO::SEVERE << "Unrecognized column " << _column
                << ", writing DATA" << LogIO::POST;
    }

    // Does the MS have a WEIGHT_SPECTRUM?
    Bool hasWeightArray = _ms.tableDesc(). isColumn(MS::columnName(
            MS::WEIGHT_SPECTRUM));

    if (hasWeightArray) {
        MSMainColumns tempCols(_ms);
        if (!tempCols.weightSpectrum().isDefined(0))
            hasWeightArray = False;
    }

    IPosition dataShape(6, 3, numcorr0, nchan, 1, 1, 1);
    if (_combineSpw) {
        dataShape(3) = nrspw;
    }
    desc.addField("data", TpArrayFloat, dataShape);

    // Random Parameters
    // UU VV WW
    desc.addField("u", TpFloat);
    desc.addField("v", TpFloat);
    desc.addField("w", TpFloat);
    // DATE
    desc.addField("date1", TpFloat);
    desc.addField("date2", TpFloat);
    // BASELINE if maximum antenna number < 256
    // SUBARRAY, ANTENNA1 and ANTENNA2 otherwise
    // (see AIPS memo 117, section 3.1.2)
    if (maxant < 256) {
        desc.addField("baseline", TpFloat);
    } else {
        desc.addField("subarray", TpFloat);
        desc.addField("antenna1", TpFloat);
        desc.addField("antenna2", TpFloat);
    }
    // FREQSEL
    ScalarColumn<Int> inddid(_ms, MS::columnName(MS::DATA_DESC_ID));
    desc.addField("freqsel", TpFloat);
    // SOURCE and INTTIM only in multi-source table
    if (asMultiSource) {
        desc.addField("source", TpFloat);
        desc.addField("inttim", TpFloat);
    }

    // "Optional" keywords

    // BSCALE BZERO BUNIT
    ek.define("bscale", 1.0);
    ek.define("bzero", 0.0);
    String bunit = "UNCALIB";
    {
        TableColumn indata(_ms, columnName);
        if (indata.keywordSet().isDefined("QuantumUnit")
                && indata.keywordSet().dataType("QuantumUnit") == TpString) {
            indata.keywordSet().get("QuantumUnit", bunit);
            bunit.upcase();
        }
    }
    ek.define("bunit", bunit);

    // CTYPE CRVAL CDELT CRPIX  CROTA
    ek.define("ctype2", "COMPLEX");
    ek.define("crval2", 1.0);
    ek.define("cdelt2", 1.0);
    ek.define("crpix2", 1.0);
    ek.define("crota2", 0.0);

    ek.define("ctype3", "STOKES");
    ek.define("crval3", stokes(stokesIndex(0)) * 1.0);
    if (stokes.nelements() > 1) {
        ek.define("cdelt3", (stokes(stokesIndex(1)) - stokes(stokesIndex(0)))
                * 1.0);
    } else {
        ek.define("cdelt3", 1.0);
    }
    ek.define("crpix3", 1.0);
    ek.define("crota3", 0.0);

    ek.define("ctype4", "FREQ");
    ek.define("crval4", refFreq);
    ek.define("cdelt4", bw0);
    ek.define("crpix4", Double(refPixelFreq));
    ek.define("crota4", 0.0);

    ek.define("ctype5", "IF");
    ek.define("crval5", 1.0);
    ek.define("cdelt5", 1.0);
    ek.define("crpix5", 1.0);
    ek.define("crota5", 0.0);

    ek.define("ctype6", "RA");
    ek.define("crval6", radec(0));
    ek.define("cdelt6", 1.0);
    ek.define("crpix6", 1.0);
    ek.define("crota6", 0.0);

    ek.define("ctype7", "DEC");
    ek.define("crval7", radec(1));
    ek.define("cdelt7", 1.0);
    ek.define("crpix7", 1.0);
    ek.define("crota7", 0.0);

    // PTYPE PSCALE PZERO
    Int idx = 1;
    defineRandomParam(ek, idx++, "UU");
    defineRandomParam(ek, idx++, "VV");
    defineRandomParam(ek, idx++, "WW");
    defineRandomParam(ek, idx++, "DATE", "Day number");
    defineRandomParam(ek, idx++, "DATE", "Day fraction");
    if (maxant < 256) {
        defineRandomParam(ek, idx++, "BASELINE");
    } else {
        defineRandomParam(ek, idx++, "SUBARRAY");
        defineRandomParam(ek, idx++, "ANTENNA1");
        defineRandomParam(ek, idx++, "ANTENNA2");
    }
    defineRandomParam(ek, idx++, "FREQSEL");
    if (asMultiSource) {
        defineRandomParam(ek, idx++, "SOURCE");
        defineRandomParam(ek, idx++, "INTTIM");
    }

    // EXTEND - already written by FITSGroupWriter
    //  ek.define("extend", True);

    // BLOCKED - already written by FITSGroupWriter
    //  ek.define("blocked", True);

    // OBJECT
    if (asMultiSource) {
        ek.define("object", "MULTI");
    } else {
        ek.define("object", objectname);
    }

    // OBS-TIME
    {
        ScalarColumn<Double> intm(_ms, MS::columnName(MS::TIME));
        ek.define("date-obs", toFITSDate(intm(0) / C::day)); // First time entry
    }

    // TELESCOP INSTRUME
    MSObservationColumns obsC(_ms.observation());
    if (obsC.nrow() == 0) {
        os << LogIO::SEVERE << "No Observation info!" << LogIO::POST;
        return 0;
    }
    ek.define("telescop", obsC.telescopeName()(0));
    ek.define("instrume", obsC.telescopeName()(0));
    ek.define("observer", obsC.observer()(0));
    ek.define("sortord", "TB");

    // Write a WCS keyword to indicate the frequency frame
    // in which the Freq axis (and FQ table) is defined
    String fframe = MFrequency::showType(measFreq0);
    if (fframe == "TOPO" || fframe == "BARY")
        fframe = fframe + "CENT";
    else if (fframe == "GEO")
        fframe = fframe + "CENTR";
    else if (fframe == "GALACTO")
        fframe = fframe + "C";
    else if (fframe == "REST")
        fframe = "SOURCE";

    os << LogIO::NORMAL << "Frequency reference frame is " << fframe
            << LogIO::POST;
    ek.define("specsys", fframe);

    if (restFreq > 0.0)
        ek.define("restfreq", restFreq);

    // TBD: NEED TO WRITE VELREF AND ALT* KEYWORDS HERE?!

    // Miriad needs a weight scale factor (otherwise all weights get 0).
    // It is the proper AIPS way to do it as a history record.
    ek.define("history", "AIPS WTSCAL = 1.0");

    // Similarly, record the sort order (the following didn't work....)
    //  ek.define("history aips sort order", "TB");

    Bool deleteIptr;
    Matrix<Complex> indatatmp(IPosition(2, numcorr0, numchan0));
    const Complex *iptr = indatatmp.getStorage(deleteIptr);

    Bool deleteWtPtr;
    Matrix<Float> inwttmp(numcorr0, numchan0);
    const Float *wptr = inwttmp.getStorage(deleteWtPtr);

    Bool deleteFlagPtr;
    Matrix<Bool> inflagtmp(IPosition(2, numcorr0, numchan0));
    const Bool *fptr = inflagtmp.getStorage(deleteFlagPtr);

    Bool deleteIndPtr;
    const uInt *indptr = stokesIndex.getStorage(deleteIndPtr);

    // Do we need to check units? I think the MS rules are that units cannot
    // be changed.

    Vector<Double> uvw(3);
    Int day;
    Double dayFraction;

    const Double oneOverC = 1.0 / C::c;

    // Sort the table in order of TIME, ANTENNA1, ANTENNA2, FIELDID, SPWID.
    // Iterate through the table on the first 4 fields.
    Block<String> sortNames(_combineSpw ? 4 : 5);
    sortNames[0] = MS::columnName(MS::TIME_CENTROID);
    sortNames[1] = MS::columnName(MS::ANTENNA1);
    sortNames[2] = MS::columnName(MS::ANTENNA2);
    sortNames[3] = MS::columnName(MS::FIELD_ID);
    Vector<uInt> sortIndex;
    if (_combineSpw) {
        // combineSpw will do its own
        // DATA_DESC_ID sorting.
        sortIndex.resize(nrow);
    }
    else {
        sortNames[4] = MS::columnName(MS::DATA_DESC_ID);
    }
    Table sortTable = _ms.sort(sortNames);

    // Make objects for the various columns.
    ArrayColumn<Complex> indata(sortTable, columnName);
    ArrayColumn<Float> inweightscalar(sortTable, MS::columnName(MS::WEIGHT));
    ArrayColumn<Float> inweightarray;
    if (hasWeightArray) {
        inweightarray.attach(sortTable, MS::columnName(MS::WEIGHT_SPECTRUM));
    }
    ScalarColumn<Bool> inrowflag(sortTable, MS::columnName(MS::FLAG_ROW));
    ArrayColumn<Bool> indataflag(sortTable, MS::columnName(MS::FLAG));
    ArrayColumn<Double> inuvw(sortTable, MS::columnName(MS::UVW));
    ScalarColumn<Double>
            intimec(sortTable, MS::columnName(MS::TIME_CENTROID));
    ScalarColumn<Int> inant1(sortTable, MS::columnName(MS::ANTENNA1));
    ScalarColumn<Int> inant2(sortTable, MS::columnName(MS::ANTENNA2));
    ScalarColumn<Int> inarray(sortTable, MS::columnName(MS::ARRAY_ID));
    ScalarColumn<Int> inspwinid(sortTable, MS::columnName(MS::DATA_DESC_ID));

    ScalarColumn<Double> inexposure;
    ScalarColumn<Int> infieldid;
    if (asMultiSource) {
        infieldid.attach(sortTable, MS::columnName(MS::FIELD_ID));
        // Why is exposure only done for multisource files?
        inexposure.attach(sortTable, MS::columnName(MS::EXPOSURE));
    }

    uInt nif = 1;
    Bool padWithFlags = _padWithFlags;
    if (_combineSpw) {
        nif = nrspw;
    }
    if (nif < 2) {
        padWithFlags = False;
    }

    ScalarColumn<Double> ininterval;
    if (padWithFlags)
        ininterval.attach(sortTable, MS::columnName(MS::INTERVAL));

    // (another) check whether the SPWs naturally fit the IF paradigm.  Do this
    // before creating the writer so that a partial UVFITS file isn't left on
    // disk if this exits.
    Vector<Int> expectedDDIDs;

    uInt nOutRow = nrow;
    Vector<uInt> tbfends;

    if (_combineSpw) {
        // Prepare a list of the expected DDIDs as a function of rownr % nif.
        // If inspwinid(rownr) != expectedDDIDs[rownr % nif], something has gone
        // wrong (probably combinespw && multiple tunings, CAS-2048).
        expectedDDIDs.resize(nif);
        expectedDDIDs.set(0); // Default, but would catching errors with -1 be better?

        uInt ifnum = 0;
        for (uInt i = 0; i < ndds; ++i) {
            if (i < spwidMap.nelements() && spwidMap[i] >= 0) {
                if (ifnum < nif) {
                    expectedDDIDs[ifnum] = i;
                    ++ifnum;
                } else {
                    os << LogIO::WARN
                            << "spwidMap selects more spws than there are IFs.  Expect problems."
                            << LogIO::POST;
                }
            }
        }

        if (nif > 1) { // Don't bother counting all the inspwinids
            Vector<uInt> nperIF; // unless there is > 1 kind.
            nperIF.resize(nif);
            nperIF.set(0);

            tbfends.resize(nrow);

            uInt rownr = 0;
            while (rownr < nrow) {
                uInt tbfend = rownr + nif - 1;

                if (padWithFlags) {
                    tbfend = get_tbf_end(rownr, nrow, nif, intimec,
                            asMultiSource ? inexposure : ininterval, inant1,
                            inant2, asMultiSource, infieldid);
                    nOutRow += nif - (tbfend + 1 - rownr); // Increment by # of padded rows.
                } else if (tbfend >= nrow)
                    tbfend = nrow - 1;

                Vector<Int> miniDDIDs;
                Vector<uInt> miniSort;
                uInt nrowsThisTBF = tbfend + 1 - rownr;

                miniDDIDs.resize(nrowsThisTBF);
                miniSort.resize(nrowsThisTBF);
                for (uInt rowInTBF = 0; rowInTBF < nrowsThisTBF; ++rowInTBF) {
                    miniDDIDs[rowInTBF] = spwidMap[inspwinid(rownr + rowInTBF)];
                    ++nperIF[miniDDIDs[rowInTBF]];
                }
                GenSortIndirect<Int,uInt>::sort(miniSort, miniDDIDs);
                for (uInt rowInTBF = 0; rowInTBF < nrowsThisTBF; ++rowInTBF) {
                    sortIndex[rownr] = rownr + miniSort[rowInTBF] - rowInTBF;
                    tbfends[rownr] = tbfend;
                    ++rownr;
                }
            }
            os << LogIO::DEBUG1 << "rownr         = " << rownr << LogIO::POST;
            os << LogIO::DEBUG1 << "nrow          = " << nrow << LogIO::POST;
            os << LogIO::DEBUG1 << "nOutRow * nif = " << nOutRow << LogIO::POST;

            if (nOutRow % nif) {
                os << LogIO::SEVERE << "The expected # of output rows, "
                        << nOutRow
                        << " is not a multiple of the number of IFs, " << nif
                        << ".\n" << "Expect problems." << LogIO::POST;
                // Commented out for now.
                // return 0;
            }
            nOutRow /= nif;

            if (!padWithFlags) {
                Bool haveProblem = false;
                for (uInt m = 1; m < nif; ++m) {
                    if (nperIF[m] != nperIF[0]) {
                        haveProblem = true;
                        break;
                    }
                }

                if (haveProblem) {
                    os << LogIO::SEVERE
                            << "The number of rows per spectral window varies:\n"
                            << " Output SpW   # of rows\n";
                    for (uInt m = 0; m < nif; ++m)
                        os << "    " << m << "       " << nperIF[m] << "\n";
                    os
                            << " the spectral windows cannot be combined without padwithflags."
                            << LogIO::POST;
                    return 0;
                }
            } else {
                os << LogIO::NORMAL << outFITSFile << " will be " << 100.0
                        * (1.0 - nrow / static_cast<Float> (nOutRow * nif))
                        << "% padded by flags to fit into IFs." << LogIO::POST;
            }
        }
    }

    // Finally, make the writer.  If it breaks past this point, the user gets to
    // look at the pieces.
    FITSGroupWriter writer(outFITSFile, desc, nOutRow, ek, False);
    outfile.reset(writer.writer());

    // DATA - out
    RecordFieldPtr<Array<Float> > odata(writer.row(), "data");
    Bool deleteOptr;
    Float *optr = (*odata).getStorage(deleteOptr);

    os << LogIO::DEBUG1 << "output data shape = " << writer.row().asArrayFloat(
            writer.row().fieldNumber("data")).shape() << " "
            << writer.row().asArrayFloat("data").shape() << " "
            << writer.row().asArrayFloat(writer.row().fieldNumber("data")).data()
            << " " << LogIO::POST;

    RecordFieldPtr<Float> ouu(writer.row(), "u");
    RecordFieldPtr<Float> ovv(writer.row(), "v");
    RecordFieldPtr<Float> oww(writer.row(), "w");
    RecordFieldPtr<Float> odate1(writer.row(), "date1");
    RecordFieldPtr<Float> odate2(writer.row(), "date2");
    RecordFieldPtr<Float> obaseline;
    RecordFieldPtr<Float> osubarray;
    RecordFieldPtr<Float> oantenna1;
    RecordFieldPtr<Float> oantenna2;
    if (maxant < 256) {
        obaseline = RecordFieldPtr<Float> (writer.row(), "baseline");
    } else {
        osubarray = RecordFieldPtr<Float> (writer.row(), "subarray");
        oantenna1 = RecordFieldPtr<Float> (writer.row(), "antenna1");
        oantenna2 = RecordFieldPtr<Float> (writer.row(), "antenna2");
    }
    RecordFieldPtr<Float> ofreqsel(writer.row(), "freqsel");
    RecordFieldPtr<Float> osource;
    RecordFieldPtr<Float> ointtim;
    if (asMultiSource) {
        osource = RecordFieldPtr<Float> (writer.row(), "source");
        ointtim = RecordFieldPtr<Float> (writer.row(), "inttim");
    }

    // Check if first cell has a WEIGHT of correct shape.
    if (hasWeightArray) {
        IPosition shp = inweightarray.shape(0);
        if (shp.nelements() > 0 && !shp.isEqual(inwttmp.shape())) {
            hasWeightArray = False;
            os << LogIO::WARN << "WEIGHT_SPECTRUM is ignored (incorrect shape)"
                    << LogIO::POST;
        }
    }

    // Loop through all rows.
    ProgressMeter meter(0.0, nOutRow * 1.0, "UVFITS Writer", "Rows copied", "",
            "", True, nOutRow / 100);

    uInt tbfrownr = 0; // Input row # of (time, baseline, field).
    uInt outrownr = 0; // Output row #.

    //Double lasttime(0.0);
    Vector<Float> realcorr(numcorr0);
    Vector<Float> imagcorr(numcorr0);
    Vector<Float> wgtaver(numcorr0);
    Vector<Float> realcorrf(numcorr0);
    Vector<Float> imagcorrf(numcorr0);
    Vector<Float> wgtaverf(numcorr0);
    Int old_nspws_found = -1; // Just for debugging curiosity.
    while (tbfrownr < nrow) {
        if (outrownr >= nOutRow) { // Shouldn't happen, but just in case...
            os << LogIO::WARN
                    << "The loop over output rows failed to stop when expected...stopping it now."
                    << LogIO::POST;
            break;
        }

        // Will only write a record if some non-flagged data found
        //    Bool dowrite(True);   // temporarily disable, because FITSGroupWriter chokes

        Float* outptr = optr; // reset for each spectral-window

        // Loop over the IFs, whether or not the corresponding spws are present for
        // this (time, baseline, field).
        // rownr should only be used inside this loop; use tbfrownr outside.
        uInt rawrownr = tbfrownr; // Essentially tbfrownr + m - # of missing spws
        // so far.
        uInt rownr = rawrownr;
        uInt tbfend = tbfrownr + nif - 1;
        if (_combineSpw && nif > 1) {
            tbfend = tbfends[rownr];
            rownr = sortIndex[rawrownr];
        }

        for (uInt m = 0; m < nif; ++m) {
            Bool rowFlag; // FLAG_ROW

            if (_combineSpw && (rownr >= nrow // flag remaining IFs in tbfrownr
                    || inspwinid(rownr) != expectedDDIDs[m])) {
                if (padWithFlags) {
                    // Save this row for the next one, and fill in with flagged junk.

                    indatatmp.set(0.0); // DATA matrix
                    //indata.get(rownr, indatatmp);   // DATA matrix
                    rowFlag = true;
                    inflagtmp.set(true);
                    inwttmp.set(0.0);
                    // Don't update lasttime.
                } else {
                    os << LogIO::SEVERE
                            << "A DATA_DESC_ID appeared out of the expected order.\n"
                            << "MSes with multiple tunings (i.e. spw varies with time) cannot"
                            << "\nbe exported with combinespw.  Export each tuning separately."
                            << LogIO::POST;
                    return 0;
                }
            } else { // The spw is present, use it.
                if (rownr >= nrow) { // Shouldn't happen, but just in case...
                    os << LogIO::WARN
                            << "The loop over input rows failed to stop when expected...stopping it now."
                            << LogIO::POST;
                    break;
                }

                indata.get(rownr, indatatmp); // DATA matrix
                rowFlag = inrowflag(rownr);
                indataflag.get(rownr, inflagtmp); // FLAG

                // WEIGHT_SPECTRUM (defaults to WEIGHT)
                Bool getwt = True;
                if (hasWeightArray) {
                    IPosition shp = inweightarray.shape(rownr);
                    if (shp.isEqual(inwttmp.shape())) {
                        inweightarray.get(rownr, inwttmp);
                        getwt = False;
                    }
                }
                if (getwt) {
                    //weight_spectrum may not exist but flag and data always will.
                    IPosition shp = indatatmp.shape();
                    Int nchan = shp(1); // either num of channels of num of lags
                    //cout << "shp1=" << shp << " shp2=" << inflagtmp.shape()
                    //     << " nchan=" << nchan << endl;
                    if (nchan < 1) nchan = 1;
                    const Vector<Float> wght = inweightscalar(rownr);
                    for (Int p = 0; p < numcorr0; p++) {
                        inwttmp.row(p) = wght(p) / nchan;
                    }
                }
                /*
                 Double rtime(86400.0*floor(intimec(0)/86400.0));
                 cout << "rtime = " << rtime << " " << "nrows = " << intimec.nrow()
                 << endl;
                 cout << "time = " << intimec(rownr)-rtime;
                 if (intimec(rownr)!=lasttime)
                 cout << " ***NEW*** ";
                 cout << endl;
                 */
                // lasttime = intimec(rownr);

                if (! padWithFlags || rawrownr <= tbfend) {
                    ++rawrownr; // register that the spw was present.
                    rownr = _combineSpw && nif > 1
                        ? sortIndex[rawrownr] : rawrownr;
                }
            }

            // We should optimize this loop more, probably do frequency as
            // the inner loop?
            realcorr.set(0);
            imagcorr.set(0);
            wgtaver.set(0);
            realcorrf.set(0);
            imagcorrf.set(0);
            wgtaverf.set(0);
            Int chancounter = 0;
            Vector<Int> flagcounter(numcorr0);
            flagcounter.set(0);
            //cout << "chanstart=" << chanstart << " nchan=" << nchan
            //     << " chanstep=" << chanstep << " avgchan=" << avgchan << endl;
            for (Int k = chanstart; k < (nchan * chanstep + chanstart); k += chanstep) {
                //cout << "row = " << tbfrownr << " "
                //   << outptr << " " << optr << " " << outptr-optr << " ";
                //   << "ddi = " << m << " (" << inspwinid(i) << ") "
                //   << "chan = " << k << " / "
                //   << boolalpha
                //   << "hasWeightArray = " << hasWeightArray << " "
                //   << "wt(chan) = " << inwttmp.column(k) << "; "
                //   << "flag(chan) = " << inflagtmp.column(k) << " "
                //   << "shapes: "
                //   << indatatmp.shape() << " "
                //   << inwttmp.shape()  << " "
                //   << inflagtmp.shape()  << " "
                //   << endl;
                /*
                if (chancounter != avgchan) {
                    for (Int j = 0; j < numcorr0; j++) {
                        Int offset = indptr[j] + k * numcorr0;
                        //cout << "j=" << j << " real=" << iptr[offset].real()
                        //     << " imag=" << iptr[offset].imag() << endl;
                        if (!fptr[offset]) {
                            realcorr[j] += iptr[offset].real() * wptr[offset];
                            imagcorr[j] += iptr[offset].imag() * wptr[offset];
                            wgtaver[j] += wptr[offset];
                            flagcounter++;
                        }
                        else {
                            realcorrf[j] += iptr[offset].real() * wptr[offset];
                            imagcorrf[j] += iptr[offset].imag() * wptr[offset];
                            wgtaverf[j] += wptr[offset];
                        }
                        //cout << "j=" << j << " k=" << k
                        //     << " real=" << realcorr[j] << " image=" << imagcorr[j]
                        //     << " offset=" << offset << " chancounter=" << chancounter << endl;
                    }
                    ++chancounter;
                }
                if (chancounter == avgchan) {
                    for (Int j = 0; j < numcorr0; j++) {
                        if (wgtaver[j] > 0) {
                            outptr[0] = realcorr[j] / wgtaver[j];
                            outptr[1] = imagcorr[j] / wgtaver[j];
                            outptr[2] = wgtaver[j] / flagcounter * numcorr0;
                        }
                        else if (wgtaverf[j] > 0) {
                            outptr[0] = realcorrf[j] / wgtaverf[j];
                            outptr[1] = imagcorrf[j] / wgtaverf[j];
                            outptr[2] = -wgtaverf[j] / avgchan;
                        }
                        else {
                            outptr[0] = realcorrf[j] / avgchan;
                            outptr[1] = imagcorrf[j] / avgchan;
                            outptr[2] = 0;
                        }
                        if (rowFlag) {
                            //calculate the average even if row flagged, just in case
                            //unflag the row and it has some reasonable data there
                            outptr[2] = -abs(outptr[2]);
                        }
                        outptr += 3;
                    }
                    realcorr.set(0);
                    imagcorr.set(0);
                    wgtaver.set(0);
                    realcorrf.set(0);
                    imagcorrf.set(0);
                    wgtaverf.set(0);
                    chancounter = 0;
                    flagcounter = 0;
                }
                */
                if (chancounter != avgchan) {
                    for (Int j = 0; j < numcorr0; j++) {
                        Int offset = indptr[j] + k * numcorr0;
                        //cout << "j=" << j << " real=" << iptr[offset].real()
                        //     << " imag=" << iptr[offset].imag() << endl;
                        if (!fptr[offset]) {
                            realcorr[j] += iptr[offset].real();
                            imagcorr[j] += iptr[offset].imag();
                            wgtaver[j] += wptr[offset];
                            flagcounter[j]++;
                        }
                        else {
                            realcorrf[j] += iptr[offset].real();
                            imagcorrf[j] += iptr[offset].imag();
                            wgtaverf[j] += wptr[offset];
                        }
                        //cout << "j=" << j << " k=" << k
                        //     << " real=" << realcorr[j] << " image=" << imagcorr[j]
                        //     << " offset=" << offset << " chancounter=" << chancounter << endl;
                    }
                    ++chancounter;
                }
                if (chancounter == avgchan) {
                    for (Int j = 0; j < numcorr0; j++) {
                        if (flagcounter[j] > 0) {
                            outptr[0] = realcorr[j] / flagcounter[j];
                            outptr[1] = imagcorr[j] / flagcounter[j];
                            outptr[2] = wgtaver[j] / flagcounter[j];
                        }
                        else if (wgtaverf[j] > 0) {
                            outptr[0] = realcorrf[j] / avgchan;
                            outptr[1] = imagcorrf[j] / avgchan;
                            outptr[2] = -wgtaverf[j] / avgchan;
                        }
                        else {
                            outptr[0] = realcorrf[j] / avgchan;
                            outptr[1] = imagcorrf[j] / avgchan;
                            outptr[2] = 0;
                        }
                        if (rowFlag) {
                            //calculate the average even if row flagged, just in case
                            //unflag the row and it has some reasonable data there
                            outptr[2] = -abs(outptr[2]);
                        }
                        outptr += 3;
                    }
                    realcorr.set(0);
                    imagcorr.set(0);
                    wgtaver.set(0);
                    realcorrf.set(0);
                    imagcorrf.set(0);
                    wgtaverf.set(0);
                    chancounter = 0;
                    flagcounter.set(0);
                }
            }

            // if(nOutRow - outtbfrownr < 5)
            //   os << LogIO::DEBUG1 << "(outtbfrownr, m) = (" << outtbfrownr
            //      << ", " << m << ")" << LogIO::POST;

        } // Ends loop over IFs.

        // If found data at this timestamp, write it out
        //    if (dowrite) {
        // Random parameters
        // UU VV WW
        inuvw.get(tbfrownr, uvw);
        *ouu = uvw(0) * oneOverC;
        *ovv = uvw(1) * oneOverC;
        *oww = uvw(2) * oneOverC;

        // TIME
        timeToDay(day, dayFraction, intimec(tbfrownr));
        *odate1 = day;
        *odate2 = dayFraction;

        // BASELINE
        if (maxant < 256) {
            *obaseline = antnumbers(inant1(tbfrownr)) * 256 +
                    antnumbers(inant2(tbfrownr)) +
                    inarray(tbfrownr) * 0.01;
        } else {
            *osubarray = inarray(tbfrownr) + 1;
            *oantenna1 = antnumbers(inant1(tbfrownr));
            *oantenna2 = antnumbers(inant2(tbfrownr));
        }

        // FREQSEL (in the future it might be FREQ_GRP+1)
        //    *ofreqsel = inddid(i) + 1;
        *ofreqsel = _combineSpw ? 1 : 1 + spwidMap[inspwinid(tbfrownr)];

        // SOURCE
        // INTTIM
        if (asMultiSource) {
            *osource = 1 + fieldidMap[infieldid(tbfrownr)];
            *ointtim = inexposure(tbfrownr);
        }

        writer.write();
        ++outrownr;
        meter.update(outrownr);

        // How many spws showed up for this (time_centroid, ant1, ant2, field)?
        if (rawrownr == tbfrownr) {
            os << LogIO::WARN << "No spectral windows were present for row # "
                    << tbfrownr << "\n"
                    << " input (time_centroid, ant1, ant2, field) =\n" << "  ("
                    << intimec(tbfrownr) << ", " << inant1(tbfrownr) << ", "
                    << inant2(tbfrownr) << ", " << infieldid(tbfrownr) << ")"
                    << LogIO::POST;
        } else {
            Int nspws_found = rawrownr - tbfrownr; // Just for debugging curiosity.

            if (nspws_found != old_nspws_found) {
                old_nspws_found = nspws_found;
                os << LogIO::DEBUG1 << "Beginning with row # " << tbfrownr
                        << LogIO::POST;
                os << LogIO::DEBUG1
                        << " input (time_centroid, ant1, ant2, field) ="
                        << LogIO::POST;

                // intimec is in modified julian day seconds, but Time::Time() takes
                // julian days.
                Double mjd_in_s = intimec(tbfrownr);
                Time juldate(2400000.5 + mjd_in_s / 86400.0);
                os << LogIO::DEBUG1 << "  (" << juldate.year() << "-";
                if (juldate.month() < 10)
                    os << "0";
                os << juldate.month() << "-";
                if (juldate.dayOfMonth() < 10)
                    os << "0";
                os << juldate.dayOfMonth() << "-";

                if (juldate.hours() < 10) // Time stores things internally as days.
                    os << "0"; // Do we really want to use it for sub-day units
                os << juldate.hours() << ":"; // when we start with intimec in s?
                if (juldate.minutes() < 10)
                    os << "0";
                os << juldate.minutes() << ":";
                mjd_in_s -= 60.0 * static_cast<Int> (mjd_in_s / 60.0);
                os << mjd_in_s;

                os << ", " << inant1(tbfrownr) << ", " << inant2(tbfrownr)
                        << ", "
                // infieldid is unattached and segfaultable if !asMultiSource.
                        << (asMultiSource ? infieldid(tbfrownr) : 0) << "):"
                        << LogIO::POST;
                os << LogIO::DEBUG1 << nspws_found << " spws present out of "
                        << nif << " IFs." << LogIO::POST;
            }

            tbfrownr = rawrownr; // Increment it by the # of spws found.
        }

        //    } // dowrite
    }
    os << LogIO::DEBUG1 << "tbfrownr = " << tbfrownr << LogIO::POST;
    os << LogIO::DEBUG1 << "outrownr = " << outrownr << LogIO::POST;
    os << LogIO::DEBUG1 << "nrow     = " << nrow << LogIO::POST;
    os << LogIO::DEBUG1 << "nOutRow  = " << nOutRow << LogIO::POST;

    // changing chanbw to output one
    chanbw = bw0;

    return outfile;
}

Bool MSFitsOutput::_writeFQ(std::shared_ptr<FitsOutput> output, const MeasurementSet &ms,
        const Block<Int>& spwidMap, Int nrspw, Double refFreq,
        Int refPixelFreq, Double chanbw, Bool combineSpw,
        Int chanstart, Int nchan, Int chanstep, Int avgchan) {
    LogIO os(LogOrigin("MSFitsOutput", "writeFQ"));
    MSSpectralWindow specTable(ms.spectralWindow());
    ArrayColumn<Double> inchanfreq(specTable, MSSpectralWindow::columnName(
            MSSpectralWindow::CHAN_FREQ));
    ScalarColumn<Double> intotbw(specTable, MSSpectralWindow::columnName(
            MSSpectralWindow::TOTAL_BANDWIDTH));
    ScalarColumn<Int> insideband(specTable, MSSpectralWindow::columnName(
            MSSpectralWindow::NET_SIDEBAND));
    String telescopeName;
    {
        MSObservation obsTable(ms.observation());
        if (obsTable.nrow() > 0) {
            ScalarColumn<String> inarrayname(
                obsTable,
                MSObservation::columnName(MSObservation::TELESCOPE_NAME)
            );
            telescopeName = inarrayname(0);
        }
    }
    // ##### Header
    Record header;
    // NO_IF
    const uInt nwin = specTable.nrow();
    os << LogIO::NORMAL << "Found " << nrspw << " spectral windows "
            << LogIO::POST;

    // If all spw's are combined, we have a single freq group.
    // Otherwise each spectral-window is a group.
    IPosition shape(1, 1);
    Int nentr = nrspw;
    if (combineSpw) {
        shape(0) = nrspw;
        nentr = 1;
    }

    header.define("EXTNAME", "AIPS FQ"); // EXTNAME
    header.define("EXTVER", 1); // EXTVER
    header.define("NO_IF", Int(shape(0))); // NO_IF

    // Table description
    RecordDesc desc;
    Record stringLengths; // no strings
    Record units;
    desc.addField("FRQSEL", TpInt); // FRQSEL
    desc.addField("IF FREQ", TpArrayDouble, shape); // IF FREQ
    units.define("IF FREQ", "HZ");
    desc.addField("CH WIDTH", TpArrayFloat, shape); // CH WIDTH
    units.define("CH WIDTH", "HZ");
    desc.addField("TOTAL BANDWIDTH", TpArrayFloat, shape); // TOTAL BANDWIDTH
    units.define("TOTAL BANDWIDTH", "HZ");
    desc.addField("SIDEBAND", TpArrayInt, shape); // SIDEBAND

    FITSTableWriter writer(output.get(), desc, stringLengths, nentr, header, units,
            False);
    RecordFieldPtr<Int> freqsel(writer.row(), "FRQSEL");
    RecordFieldPtr<Array<Double> > iffreq(writer.row(), "IF FREQ");
    RecordFieldPtr<Array<Float> > ifwidth(writer.row(), "CH WIDTH");
    RecordFieldPtr<Array<Float> > totbw(writer.row(), "TOTAL BANDWIDTH");
    RecordFieldPtr<Array<Int> > sideband(writer.row(), "SIDEBAND");

    if (avgchan < 1)
        avgchan = 1;
    if (avgchan > nchan)
        avgchan = nchan;

    IPosition inx(1, 0);
    for (uInt i = 0; i < nwin; i++) {
        if (i < spwidMap.nelements() && spwidMap[i] >= 0) {
            *freqsel = 1 + spwidMap[i];
            Vector<Double> freqs = inchanfreq(i);
            if (telescopeName == "IRAM PDB" || telescopeName == "IRAM_PDB") {
                (*iffreq)(inx) = 0.0;
            } else {
                os << LogIO::DEBUG2 << "refPixelFreq=" << refPixelFreq << " refFreq=" << refFreq
                   //<< "\nfreqs=" << freqs
                   << LogIO::POST;
                Int chancounter = 0;
                Int nselectedchan = 0;
                for (Int k = chanstart; k < (nchan * chanstep + chanstart); k += chanstep) {
                    if (++chancounter == avgchan) {
                       nselectedchan++;
                       chancounter = 0;
                    }
                }
                if (nselectedchan == 0)
                    nselectedchan = 1;
                Vector<Double> sfreq(nselectedchan);
                Double frq = 0;
                chancounter = 0;
                nselectedchan = 0;
                for (Int k = chanstart; k < (nchan * chanstep + chanstart); k += chanstep) {
                    frq += freqs(k);
                    chancounter++;
                    if (chancounter == avgchan) {
                       sfreq(nselectedchan) = frq / avgchan;
                       nselectedchan++;
                       chancounter = 0;
                       frq = 0;
                    }
                }
                os << LogIO::DEBUG2 << "\nsfreq=" << sfreq
                   << LogIO::POST;
                (*iffreq)(inx) = sfreq(refPixelFreq - 1) - refFreq;
            }
            if (freqs.nelements() > 1) {
                if (telescopeName == "ALMA") {
                    if (freqs(1) < freqs(0)) {
                        (*ifwidth)(inx) = -abs(chanbw);
                    } else {
                        (*ifwidth)(inx) = abs(chanbw);
                    }
                }
                else {
                    (*ifwidth)(inx) = (chanbw);
                }
            } else {
                (*ifwidth)(inx) = intotbw(i);
            }
            (*totbw)(inx) = intotbw(i);
            if (telescopeName == "ALMA") {
                if (freqs(1) < freqs(0)) {
                    (*sideband)(inx) = -1;
                } else {
                    (*sideband)(inx) = 1;
                }
            } else {
                (*sideband)(inx) = insideband(i);
            }
            // Write the current row if not combined.
            if (combineSpw) {
                inx(0)++;
            } else {
                writer.write();
            }
        }
    }
    // Write the row if everything is combined.
    if (combineSpw) {
        *freqsel = 1;
        writer.write();
    }
    return True;
}

Bool MSFitsOutput::_writeAN(std::shared_ptr<FitsOutput> output, const MeasurementSet &ms,
        Double refFreq, Bool writeStation) {
    LogIO os(LogOrigin("MSFitsOutput", "writeAN"));
    MSObservation obsTable(ms.observation());
    ScalarColumn<String> inarrayname(obsTable, MSObservation::columnName(
            MSObservation::TELESCOPE_NAME));

    const uInt narray = obsTable.nrow();
    if (narray == 0) {
        os << LogIO::SEVERE << "No Observation info!" << LogIO::POST;
        return False;
    }

    // Calculate GSTIA0, DEGPDY, UT1UTC, and IATUTC.

    MEpoch measTime = MSColumns(ms).timeMeas()(0);

    MEpoch utctime = MEpoch::Convert(measTime, MEpoch::UTC)();
    MEpoch iattime = MEpoch::Convert(measTime, MEpoch::IAT)();
    MEpoch ut1time = MEpoch::Convert(measTime, MEpoch::UT1)();
    Double utcsec = utctime.get("s").getValue();
    Double ut1sec = ut1time.get("s").getValue();
    Double iatsec = iattime.get("s").getValue();
    // Use the beginning of the IAT day to calculate the GMST.
    Double utcday = floor(utctime.get("d").getValue());
    Double iatday = floor(iattime.get("d").getValue());
    Double gstday, gstday1;
    {
        // Use IAT=0 to get GST:
        Quantum<Double> itime(iatday, "d");
        MEpoch ia0time(itime, MEpoch::UTC);
        MEpoch gsttime = MEpoch::Convert(ia0time, MEpoch::GMST)();
        gstday = gsttime.get("d").getValue();
    }
    Double gstdeg = 360 * (gstday - floor(gstday));
    {
        // #degrees/IATday is the difference between this and the next day.
        Quantum<Double> itime(iatday + 1, "d");
        MEpoch ia0time(itime, MEpoch::UTC);
        MEpoch gsttime = MEpoch::Convert(ia0time, MEpoch::GMST)();
        gstday1 = gsttime.get("d").getValue();
    }
    Double degpdy = 360 * (gstday1 - gstday);
    // PolarMotion gives -x and -y.
    // Need to be multiplied by earth radius to get them in meters.
    const Euler& polarMotion = MeasTable::polarMotion(utcday);

    // Each array gets its own antenna table
    for (uInt arraynum = 0; arraynum < narray; ++arraynum) {
        // Get the observatory's position and convert to ITRF.
        String obsName = inarrayname(arraynum);
        MPosition pos;
        MeasTable::Observatory(pos, obsName);
        MPosition itrfpos = MPosition::Convert(pos, MPosition::ITRF)();
        MVPosition mvpos = itrfpos.getValue();
        MSAntennaColumns antCols(ms.antenna());
        // Nominally arraypos+antpos will be ITRF (see below),
        //   unless we tinker with it, in which case it is
        //   a local convention
        String posref("ITRF");
        // #### Header
        Record header;
        header.define("EXTNAME", "AIPS AN"); // EXTNAME
        header.define("EXTVER", Int(arraynum + 1)); // EXTVER
        // we are now writing antenna positions in ITRF, so the
        // corresponding array center is always the origin of this
        // coordinate system
        header.define("ARRAYX", 0);
        header.define("ARRAYY", 0);
        header.define("ARRAYZ", 0);
        header.define("GSTIA0", gstdeg); // GSTIA0
        header.define("DEGPDY", degpdy); // DEGPDY
        header.define("FREQ", refFreq); // FREQ
        header.define("RDATE", toFITSDate(measTime.get("s"))); // RDATE
        header.define("POLARX", -polarMotion(0) * 6356752.31); // POLARX
        header.define("POLARY", -polarMotion(1) * 6356752.31); // POLARY
        header.define("UT1UTC", ut1sec - utcsec); // UT1UTC
        header.define("IATUTC", iatsec - utcsec); // IATUTC
        header.define("TIMSYS", measTime.getRefString()); // TIMSYS
        header.define("ARRNAM", inarrayname(arraynum)); // ARRNAM
        header.define("NUMORB", 0); // NUMORB
        header.define("NOPCAL", 0); // NOPCAL
        header.define("POLTYPE", "        "); // POLTYPE
        header.define("XYZHAND", "RIGHT"); // handedness of antenna coord system

        // Added Nov 2009, following AIPS addition
        header.define("FRAME", posref); // FRAME
        os << LogIO::NORMAL // Requested by CAS-437.
            << "Using " << posref << " frame for antenna positions."
            << LogIO::POST;

        // NOT in going aips
        // header.define("DATUTC", 0.0);
        // header.define("P_REFANT", 15);
        // header.define("P_DIFF01", 0.0);


        // #### Row description
        RecordDesc desc;
        Record strlengths, units;
        desc.addField("ANNAME", TpString); // ANNAME
        strlengths.define("ANNAME", 8);
        desc.addField("STABXYZ", TpArrayDouble, // STABXYZ
                IPosition(1, 3));
        units.define("STABXYZ", "METERS");
        desc.addField("ORBPARM", TpArrayDouble, // ORBPARM
                IPosition(1, 0));
        desc.addField("NOSTA", TpInt); // NOSTA
        desc.addField("MNTSTA", TpInt); // MNTSTA
        desc.addField("STAXOF", TpFloat); // STAXOF
        units.define("STAXOF", "METERS");
        desc.addField("POLTYA", TpString); // POLTYA
        strlengths.define("POLTYA", 1);
        desc.addField("POLAA", TpFloat); // POLAA
        units.define("POLAA", "DEGREES");
        ///    desc.addField("POLCALA", TpArrayFloat,           // POLCALA
        ///          IPosition(1,0));
        desc.addField("POLCALA", TpFloat); // POLCALA
        desc.addField("POLTYB", TpString); // POLTYB
        strlengths.define("POLTYB", 1);
        desc.addField("POLAB", TpFloat); // POLAB
        units.define("POLAB", "DEGREES");
        ///    desc.addField("POLCALB", TpArrayFloat,           // POLCALB
        ///          IPosition(1,0));
        desc.addField("POLCALB", TpFloat); // POLCALB
        desc.addField("DIAMETER", TpFloat);
        MSAntenna antennaTable = ms.antenna();
        MSAntennaColumns antennaCols(antennaTable);

        // SELECT antennas for the current sub-array
        //    MSAntenna antennaTable = ms.antenna()
        //(ms.antenna().col(MSAntenna::columnName(MSAntenna::ARRAY_ID)) ==
        //                  Int(arraynum));

        ScalarColumn<String> inantname(antennaCols.station());
        ScalarColumn<String> antid(antennaCols.name());
        ScalarColumn<String> inantmount(antennaCols.mount());
        MPosition::ScalarColumn inantposition(antennaCols.positionMeas());
        ArrayColumn<Double> inantoffset(antennaCols.offset());
        const uInt nant = antennaTable.nrow();
        os << LogIO::NORMAL << "Found " << nant << " antennas in array #"
                << arraynum + 1 << LogIO::POST;

        MSFeed feedTable = ms.feed();
        MSFeedColumns feedCols(feedTable);
        ArrayColumn<String> inpoltype(feedCols.polarizationType());
        ScalarColumn<Int> inantid(feedCols.antennaId());
        ScalarColumn<Int> spwids(feedCols.spectralWindowId());
        MSMetaData msmd(&ms, 100);
        std::set<uInt> uSpws = msmd.getUniqueSpwIDs();
        ArrayQuantColumn<Double> receptorAngle(feedCols.receptorAngleQuant());

        FITSTableWriter writer(output.get(), desc, strlengths, nant, header, units, False);
        RecordFieldPtr<String> anname(writer.row(), "ANNAME");
        RecordFieldPtr<Array<Double> > stabxyz(writer.row(), "STABXYZ");
        RecordFieldPtr<Array<Double> > orbparm(writer.row(), "ORBPARM");
        RecordFieldPtr<Int> nosta(writer.row(), "NOSTA");
        RecordFieldPtr<Int> mntsta(writer.row(), "MNTSTA");
        RecordFieldPtr<Float> staxof(writer.row(), "STAXOF");
        RecordFieldPtr<String> poltya(writer.row(), "POLTYA");
        RecordFieldPtr<Float> polaa(writer.row(), "POLAA");
        RecordFieldPtr<Float> polcala(writer.row(), "POLCALA");
        RecordFieldPtr<String> poltyb(writer.row(), "POLTYB");
        RecordFieldPtr<Float> polab(writer.row(), "POLAB");
        RecordFieldPtr<Float> polcalb(writer.row(), "POLCALB");
        RecordFieldPtr<Float> diam(writer.row(), "DIAMETER");

        // Set the ones we're not going to change once
        *orbparm = 0.0;
        *poltya = " ";
        *polaa = 0.0;
        *polcala = 0.0;
        *poltyb = " ";
        *polab = 0.0;
        *polcalb = 0.0;

        Vector<Int> id;
        _handleAntNumbers(ms, id);

        // A hack for old WSRT observations which stored the antenna name
        // in the STATION column instead of the NAME column.
        // So if all NAMES are equal use STATIONS (unless they are all equal).
        // Also: if writeStation==True use station names instead of antenna names
        // for the output fits file (input fits file tends to have this).
        Vector<String> anames = antid.getColumn();
        Vector<Double> antDiams = msmd.getAntennaDiameters().getValue("m");
        if (anames.nelements() > 0) {
            if (writeStation || allEQ(anames, anames(0))) {
                Vector<String> stations = inantname.getColumn();
                if (!allEQ(stations, stations(0))) {
                    anames = stations;
                }
            }
        }
        // antenna -> receptor angles
        std::map<uInt, Vector<Quantity> > antToRA;
        for (uInt antnum = 0; antnum < nant; ++antnum) {
            *anname = anames(antnum);

            // Get antenna position in ITRF coordinates.
            // Take difference with array position.
            MPosition antpos = inantposition.convert(antnum, MPosition::ITRF);
            Vector<Double> corstabxyz = antpos.getValue().getValue();
            *stabxyz = corstabxyz;
            *nosta = id[antnum];
            String mount = upcase(inantmount(antnum));
            // MS has "EQUATORIAL", "ALT-AZ", "X-Y",  "SPACE-HALCA"
            if (mount.contains("ALT-AZ+NASMYTH-R")) {
                *mntsta = 4;
            }
            else if (mount.contains("ALT-AZ+NASMYTH-L")) {
                *mntsta = 5;
            }
	    else if (mount.contains("ALT-AZ")) {
                *mntsta = 0;
            }
            else if (mount.contains("EQUATORIAL")) {
                *mntsta = 1;
            }
            else if (mount.contains("ORBIT")) {
                *mntsta = 2;
            }
            else if (mount.contains("X-Y")) {
                *mntsta = 3;
            }
            else if (mount.contains("SPACE-HALCA")) {
                *mntsta = 7;
            }
            else if (mount.contains("BIZARRE")) {
                *mntsta = 6;
            }
            else {
                *mntsta = 7; // fits does not use anyway, put it 7
            }
            *staxof = inantoffset(antnum)(IPosition(1, 0));
            // OK, try to find if we're L/R or X/Y
            // This probably breaks down when we have more than one
            // polarization type on different feeds (unlikely) or
            // different spectral windows (more likely).
            const uInt nmax = feedTable.nrow();
            Bool found = False;
            *poltya = " ";
            *poltyb = " ";
            for (uInt i = 0; i < nmax; ++i) {
                // filter out irrelevant spws. spw = -1 in the FEED table
                // inicates that row applies for all spectral windows
                if (
                    Int(antnum) == inantid(i)
                      && (
                          spwids(i) == -1
                          || uSpws.find(spwids(i)) != uSpws.end()
                      )
                    ) {
                    found = True;
                    Vector<String> poltypes = inpoltype(i);
                    Vector<Quantity> ra;
                    receptorAngle.get(i, ra);
                    if (antToRA.find(antnum) == antToRA.end()) {
                        if (poltypes.nelements() >= 1) {
                            *poltya = poltypes(0);
                            *polaa = ra[0].getValue("deg");
                        }
                        if (poltypes.nelements() >= 2) {
                            *poltyb = poltypes(1);
                            *polab = ra[1].getValue("deg");
                        }
                        antToRA[antnum] = ra;
                    }
                    else {
                        try {
                            _checkReceptorAngles(ra, antToRA[antnum], antnum);
                        }
                        catch (const std::exception& x) {
                            os << LogOrigin("MSFitsOutput", __func__)
                               << LogIO::WARN << "Receptor Angles in FEED table are not constant for antenna " << antnum
                               << ". Will try to ignore this. If this data is not yet calibrated, a future calibration of the uvfits data may fail: "
                               << x.what() << LogIO::POST;
                        }
                    }
                }
            }

            if (!found) {
                os << LogIO::SEVERE
                        << "Could not find polarization types for antenna "
                        << antnum << LogIO::POST;
            }
            *diam = antDiams[antnum];
            writer.write();
        }
    }
    return True;
}

void MSFitsOutput::_checkReceptorAngles(
    const Vector<Quantity>& ra0, Vector<Quantity>& ra1, Int antnum
) {
    if (ra0.size() != ra1.size()) {
        ostringstream oss;
        oss << "Varying number of receptor angles found for "
            << "specified spectral windows for antenna "
            << antnum << " is not supported by uvfits";
        ThrowCc(oss.str());
    }
    uInt nra = ra0.size();
    for (uInt j=0; j<nra; ++j) {
        if (
            ! nearAbs(ra0[j].getValue("deg"), ra1[j].getValue("deg"), 0.001)
        ) {
            ostringstream oss;
            oss << "Receptor angle " << j << " for antenna " << antnum
                << " varies with the selected spectral windows "
                << "which is not supported by uvfits.";
            ThrowCc(oss.str());
        }
    }
}

Bool MSFitsOutput::_writeSU(std::shared_ptr<FitsOutput> output, const MeasurementSet &ms,
        const Block<Int>& fieldidMap, Int nrfield,
        const Block<Int>& /*spwidMap*/, Int nrspw) {
    LogIO os(LogOrigin("MSFitsOutput", "writeSU"));
    // Basically we make the FIELD_ID the source ID.
    MSField fieldTable(ms.field());
    MSFieldColumns msfc(fieldTable);
    const ScalarColumn<Int>& insrcid = msfc.sourceId();
    const ScalarColumn<String>& inname = msfc.name();

    // If source table exists, access it

    // This is for case where SOURCE guaranteed to exist:
    //  MSSource sourceTable(ms.source());
    //  MSSourceColumns sourceColumns(sourceTable);
    //  ColumnsIndex srcInx(sourceTable, "SOURCE_ID");
    //  RecordFieldPtr<Int> srcInxFld(srcInx.accessKey(), "SOURCE_ID");

    // This is for case where SOURCE may not exist:
    //   (doesn't work yet!)
    MSSource* sourceTable = 0;
    MSSourceColumns* sourceColumns = 0;
    ColumnsIndex* srcInx = 0;
    RecordFieldPtr<Int>* srcInxFld = 0;
    if (!ms.source().isNull()) {
        sourceTable = new MSSource(ms.source());
        sourceColumns = new MSSourceColumns(*sourceTable);
        // Create an index for the SOURCE table.
        // Make a RecordFieldPtr for the SOURCE_ID field in the index key record.
        srcInx = new ColumnsIndex(*sourceTable, "SOURCE_ID");
        srcInxFld = new RecordFieldPtr<Int> (srcInx->accessKey(), "SOURCE_ID");
    }

    MSSpectralWindow spectralTable(ms.spectralWindow());

    const uInt nrow = fieldTable.nrow();
    if (nrow == 0) {
        os << LogIO::SEVERE << "No field table!" << LogIO::POST;
        return False;
    }
    if (spectralTable.nrow() == 0) {
        os << LogIO::SEVERE << "No spectral window table!" << LogIO::POST;
        return False;
    }
    ScalarColumn<Double> totalbw(spectralTable, MSSpectralWindow::columnName(
            MSSpectralWindow::TOTAL_BANDWIDTH));
    Double totalBandwidth = totalbw(0);
    //    const uInt nsource = sourceTable.nrow(); // this is allowed to be 0

    // #### Header
    Record header;
    header.define("EXTNAME", "AIPS SU"); // EXTNAME
    header.define("EXTVER", 1); // EXTVER
    header.define("NO_IF", nrspw);
    header.define("FREQID", 1);
    String velDef;
    String velType;
    if (spectralTable.tableDesc().isColumn("NFRA_VELOCDEFINITION")) {
        ScalarColumn<String> velDefCol(spectralTable, "NFRA_VELOCDEFINITION");
        header.define("VELTYP", "");
        header.define("VELDEF", "");
    } else {
        os << LogIO::NORMAL << "Not setting velocity types" << LogIO::POST;
    }

    // #### Row description
    RecordDesc desc;
    Record strlengths, units;
    desc.addField("ID. NO.", TpInt);
    desc.addField("SOURCE", TpString);
    strlengths.define("SOURCE", 20);
    desc.addField("QUAL", TpInt);
    desc.addField("CALCODE", TpString);
    strlengths.define("CALCODE", 4);
    desc.addField("IFLUX", TpArrayFloat, IPosition(1, nrspw));
    units.define("IFLUX", "JY");
    desc.addField("QFLUX", TpArrayFloat, IPosition(1, nrspw));
    units.define("QFLUX", "JY");
    desc.addField("UFLUX", TpArrayFloat, IPosition(1, nrspw));
    units.define("UFLUX", "JY");
    desc.addField("VFLUX", TpArrayFloat, IPosition(1, nrspw));
    units.define("VFLUX", "JY");
    desc.addField("FREQOFF", TpArrayDouble, IPosition(1, nrspw));
    units.define("FREQOFF", "HZ");
    desc.addField("BANDWIDTH", TpDouble);
    units.define("BANDWIDTH", "HZ");
    desc.addField("RAEPO", TpDouble);
    units.define("RAEPO", "DEGREES");
    desc.addField("DECEPO", TpDouble);
    units.define("DECEPO", "DEGREES");
    desc.addField("EPOCH", TpDouble);
    units.define("EPOCH", "YEARS");
    desc.addField("RAAPP", TpDouble);
    units.define("RAAPP", "DEGREES");
    desc.addField("DECAPP", TpDouble);
    units.define("DECAPP", "DEGREES");
    desc.addField("LSRVEL", TpArrayDouble, IPosition(1, nrspw));
    units.define("LSRVEL", "M/SEC");
    desc.addField("RESTFREQ", TpArrayDouble, IPosition(1, nrspw));
    units.define("RESTFREQ", "HZ");
    desc.addField("PMRA", TpDouble);
    units.define("PMRA", "DEG/DAY");
    desc.addField("PMDEC", TpDouble);
    units.define("PMDEC", "DEG/DAY");

    FITSTableWriter writer(output.get(), desc, strlengths, nrfield, header, units,
            False);

    RecordFieldPtr<Int> idno(writer.row(), "ID. NO.");
    RecordFieldPtr<String> source(writer.row(), "SOURCE");
    RecordFieldPtr<Int> qual(writer.row(), "QUAL");
    RecordFieldPtr<String> calcode(writer.row(), "CALCODE");
    RecordFieldPtr<Array<Float> > iflux(writer.row(), "IFLUX");
    RecordFieldPtr<Array<Float> > qflux(writer.row(), "QFLUX");
    RecordFieldPtr<Array<Float> > uflux(writer.row(), "UFLUX");
    RecordFieldPtr<Array<Float> > vflux(writer.row(), "VFLUX");
    RecordFieldPtr<Array<Double> > freqoff(writer.row(), "FREQOFF");
    RecordFieldPtr<Double> bandwidth(writer.row(), "BANDWIDTH");
    RecordFieldPtr<Double> raepo(writer.row(), "RAEPO");
    RecordFieldPtr<Double> decepo(writer.row(), "DECEPO");
    RecordFieldPtr<Double> epoch(writer.row(), "EPOCH");
    RecordFieldPtr<Double> raapp(writer.row(), "RAAPP");
    RecordFieldPtr<Double> decapp(writer.row(), "DECAPP");
    RecordFieldPtr<Array<Double> > lsrvel(writer.row(), "LSRVEL");
    RecordFieldPtr<Array<Double> > restfreq(writer.row(), "RESTFREQ");
    RecordFieldPtr<Double> pmra(writer.row(), "PMRA");
    RecordFieldPtr<Double> pmdec(writer.row(), "PMDEC");

    // Default them all, then we can gradually add more in the loop without
    // worrying about it.
    *idno = 0;
    *source = "                ";
    *qual = 0;
    *calcode = "    ";
    *iflux = 0.0;
    *qflux = 0.0;
    *uflux = 0.0;
    *vflux = 0.0;
    *freqoff = 0.0;
    *bandwidth = totalBandwidth;
    *raepo = 0.0;
    *decepo = 0.0;
    *epoch = 2000.0;
    *raapp = 0.0;
    *decapp = 0.0;
    *lsrvel = 0.0;
    *restfreq = 0.0;
    *pmra = 0.0;
    *pmdec = 0.0;

    MDirection dir;

    Vector<String> fnames(nrow);
    for(uInt fieldnum = 0; fieldnum < nrow; fieldnum++) {
      ostringstream oss;
      oss.fill(' ');
      oss.flags(std::ios::left);
      oss.width(20);
      oss << inname(fieldnum);
      fnames(fieldnum) = oss.str();
    }

    // Only take those fields which are part of the fieldidMap
    // (which represents the fields written in the main table).
    for (uInt fieldnum = 0; fieldnum < nrow; fieldnum++) {
        if (fieldnum < fieldidMap.nelements() && fieldidMap[fieldnum] >= 0) {
            *idno = 1 + fieldidMap[fieldnum];
            dir = msfc.phaseDirMeas(fieldnum);

            *source = fnames(fieldnum);
            // check if name is unique
            *qual = 0;
            for(uInt ifld=0; ifld<nrow; ifld++){
                if(ifld!=fieldnum && *source==fnames(ifld)) {
                    *qual = fieldnum; // not unique, set qual such that name+qual is unique
                    break;
                }
            }

            if (dir.getRef().getType() == MDirection::B1950) {
                *epoch = 1950.;
            }

            // Use info from SOURCE table if available.
            // Try to find the SOURCE_ID in the SOURCE table.
            // If multiple rows found, use the first one.
            // Use the first spectral line.

            //  Optional access to SOURCE table
            if (sourceTable) {
                **srcInxFld = insrcid(fieldnum);
                Vector<rownr_t> rownrs = srcInx->getRowNumbers();
                if (rownrs.nelements() > 0) {
                    uInt rownr = rownrs(0);
                    if (!sourceColumns->sysvel().isNull()
                            && sourceColumns->sysvel().isDefined(rownr)) {
                        Vector<Double> sv(sourceColumns->sysvel()(rownr));
                        if (sv.nelements() > 0) {
                            *lsrvel = sv(0);
                        }
                    }
                    if (sourceTable->isColumn(MSSource::REST_FREQUENCY)
                        && sourceColumns->restFrequency().isDefined(rownr)
                    ) {
                        Vector<Double>
                                rf(sourceColumns->restFrequency()(rownr));
                        if (rf.nelements() > 0) {
                            *restfreq = rf(0);
                        }
                    }
                    if (sourceColumns->properMotion().isDefined(rownr)) {
                        Vector<Double> pm = sourceColumns->properMotion()(rownr);
			String unit="UNCALIB";
			unit = sourceColumns->properMotion().keywordSet().asArrayString("QuantumUnits").tovector()[0];
			unit.upcase();
			if (unit=="RAD/S"){ // convert to deg/day
			    *pmra = pm(0)*C::day/C::degree;
			    *pmdec = pm(1)*C::day/C::degree;
			}
			else{
			    *pmra = pm(0);
			    *pmdec = pm(1);
			    if (unit.at(0,5)!="DEG/D"){
			       os << LogIO::WARN
				  << "Proper motion for source in SOURCE table row #"<<rownr<<" has unfamiliar units: "
				  << unit << " .\n Value in uv fits table AIPS SU needs to be checked."
				  << LogIO::POST;
			    }
			}

                    }
                    *calcode = sourceColumns->code()(rownr) + "    ";

                    // Directions have to be converted from radians to degrees.
                    //if (sourceColumns->direction().isDefined(rownr)) {
                    //    dir = sourceColumns->directionMeas()(rownr);
                    //}
                    //if (dir.type() == MDirection::B1950) {
                    //    *epoch = 1950.;
                    //}
                }
            }

            // Write ra/dec as epoch and apparent (in degrees).
            // Use the time in the field table to calculate apparent.
            {
                *raepo = dir.getAngle("deg").getValue()(0);
                *decepo = dir.getAngle("deg").getValue()(1);
                MeasFrame frame;
                frame.set(msfc.timeMeas()(fieldnum));
                MDirection::Ref typeout(MDirection::APP, frame);
                MDirection dirout = MDirection::Convert(dir, typeout)();
                *raapp = dirout.getAngle("deg").getValue()(0);
                *decapp = dirout.getAngle("deg").getValue()(1);
            }
            writer.write();
        }
    }
    os << LogIO::NORMAL << "writing " << nrfield << " sources" << LogIO::POST;

    // Delete dynamic memory, if nec:
    if (sourceTable)
        delete sourceTable;
    if (sourceColumns)
        delete sourceColumns;
    if (srcInx)
        delete srcInx;
    if (srcInxFld)
        delete srcInxFld;

    return True;
}

Bool MSFitsOutput::_writeTY(std::shared_ptr<FitsOutput> output, const MeasurementSet &ms,
        const Table& syscal, const Block<Int>& spwidMap, uInt nrif,
        Bool combineSpw) {
    LogIO os(LogOrigin("MSFitsOutput", "writeTY"));
    const MSSysCal subtable(syscal);
    MSSysCalColumns sysCalColumns(subtable);
    const uInt nrow = syscal.nrow();
    if (nrow == 0 || sysCalColumns.tsys().isNull()) {
        os << LogIO::SEVERE << "No SysCal TY info!" << LogIO::POST;
        return False;
    }
    // Get #pol by taking shape of first tsys from the column.
    const Int npol = sysCalColumns.tsys().shape(0)(0);
    if (!combineSpw) {
        nrif = 1;
    }
    IPosition ifShape(1, nrif);
    const uInt nentries = nrow / nrif;

    os << LogIO::NORMAL << "Found " << nentries << " TY table entries ("
            << nrif << " IFs)" << LogIO::POST;

    // Get reference time (i.e. start time) from the main table.
    Double refTime;
    { // get starttime (truncated to days)
        MSColumns mscol(ms);
        refTime = floor(mscol.time()(0) / C::day) * C::day;
    }
    // ##### Header
    Record header;
    header.define("EXTNAME", "AIPS TY"); // EXTNAME
    header.define("EXTVER", 1); // EXTVER
    header.define("NO_IF", Int(nrif)); // NO_IF
    header.define("NO_POL", npol); // NO_POL
    header.define("REVISION", 10); // REVISION

    // Table description
    RecordDesc desc;
    Record stringLengths; // no strings
    Record units;
    desc.addField("TIME", TpFloat);
    units.define("TIME", "DAYS");
    desc.addField("TIME INTERVAL", TpFloat);
    units.define("TIME INTERVAL", "DAYS");
    desc.addField("SOURCE ID", TpInt);
    desc.addField("ANTENNA NO.", TpInt);
    desc.addField("SUBARRAY", TpInt);
    desc.addField("FREQ ID", TpInt);
    desc.addField("TSYS 1", TpArrayFloat, ifShape);
    units.define("TSYS 1", "KELVINS");
    desc.addField("TANT 1", TpArrayFloat, ifShape);
    units.define("TANT 1", "KELVINS");
    if (npol == 2) {
        desc.addField("TSYS 2", TpArrayFloat, ifShape);
        units.define("TSYS 2", "KELVINS");
        desc.addField("TANT 2", TpArrayFloat, ifShape);
        units.define("TANT 2", "KELVINS");
    }

    FITSTableWriter writer(output.get(), desc, stringLengths, nentries, header,
            units, False);
    RecordFieldPtr<Float> time(writer.row(), "TIME");
    RecordFieldPtr<Float> interval(writer.row(), "TIME INTERVAL");
    RecordFieldPtr<Int> sourceId(writer.row(), "SOURCE ID");
    RecordFieldPtr<Int> antenna(writer.row(), "ANTENNA NO.");
    RecordFieldPtr<Int> arrayId(writer.row(), "SUBARRAY");
    RecordFieldPtr<Int> spwId(writer.row(), "FREQ ID");
    RecordFieldPtr<Array<Float> > tsys1(writer.row(), "TSYS 1");
    RecordFieldPtr<Array<Float> > tant1(writer.row(), "TANT 1");
    RecordFieldPtr<Array<Float> > tsys2;
    RecordFieldPtr<Array<Float> > tant2;
    if (npol == 2) {
        tsys2 = RecordFieldPtr<Array<Float> > (writer.row(), "TSYS 2");
        tant2 = RecordFieldPtr<Array<Float> > (writer.row(), "TANT 2");
    }

    Vector<Int> antnums;
    _handleAntNumbers(ms, antnums);

    Vector<Float> tsysval;
    for (uInt i = 0; i < nrow; i += nrif) {
        Double tim = sysCalColumns.time()(i);
        *time = (tim - refTime) / C::day;
        *interval = sysCalColumns.interval()(i) / C::day;
        *sourceId = 1;
        //    *antenna = 1 + sysCalColumns.antennaId()(i);
        *antenna = antnums(sysCalColumns.antennaId()(i));
        *arrayId = 1;
        *spwId = 1 + spwidMap[sysCalColumns.spectralWindowId()(i)];
        sysCalColumns.tsys().get(i, tsysval);
        Vector<Float> ts1(nrif);
        Vector<Float> ts2(nrif);
        Vector<Float> ta(nrif);
        ta = 0.;
        for (uInt j = 0; j < nrif; j++) {
            sysCalColumns.tsys().get(i + j, tsysval);
            ts1(j) = tsysval(0);
            if (npol == 2) {
                ts2(j) = tsysval(1);
            }
            if (j > 0) {
                if (sysCalColumns.time()(i + j) != tim) {
                    throw(AipsError("Irregularity in times in SYSCAL subtable"));
                }
            }
        }
        *tsys1 = ts1;
        *tant1 = ta;
        ;
        if (npol == 2) {
            *tsys2 = ts2;
            *tant2 = ta;
        }
        // Write the current row
        writer.write();
    }
    return True;
}

Bool MSFitsOutput::_writeGC(std::shared_ptr<FitsOutput> output, const MeasurementSet &ms,
        const Table& syscal, const Block<Int>& /*spwidMap*/, uInt nrif,
        Bool combineSpw, Double sensitivity, Int refPixelFreq, Double refFreq,
        Double chanbw) {
    LogIO os(LogOrigin("MSFitsOutput", "writeGC"));

    // We need to write an entry per antenna (and spw if !combineSpw).
    // So sort the SYSCAL table in that order and skip duplicate
    // spectral-windows. Use insertion sort, since the table is already in order.
    Block<String> sortNames(2);
    sortNames[0] = MSSysCal::columnName(MSSysCal::ANTENNA_ID);
    sortNames[1] = MSSysCal::columnName(MSSysCal::TIME);
    Table sorcal = syscal.sort(sortNames, Sort::Ascending, Sort::InsSort
            + Sort::NoDuplicates);
    // Sort again (without duplicates) to get the nr of antennas.
    // Remove TIME from the sort columns.
    // Use insertion sort, because the table is already in order.
    Int nrant;
    sortNames.resize(1, True, True);
    {
        Table sorcal2 = sorcal.sort(sortNames, Sort::Ascending, Sort::InsSort
                + Sort::NoDuplicates);
        nrant = sorcal2.nrow();
    }
    if (nrant == 0) {
        os << LogIO::SEVERE << "No SysCal GC info!" << LogIO::POST;
        return False;
    }
    // Find nr of IF's or SPW's.
    Int nrspw = 1;
    if (!combineSpw) {
        nrspw = nrif;
        nrif = 1;
    }
    // Get #pol from 1st row in FEED table.
    const Int npol = MSFeedColumns(ms.feed()).numReceptors()(0);
    IPosition ifShape(1, nrif);
    const uInt nentries = nrant * nrspw;

    os << LogIO::NORMAL << "Found " << nentries << " GC table entries ("
            << nrif << " IFs, " << npol << " polarizations)" << LogIO::POST;

    // Get some info from the main table.
    Int nchan, nstk;
    Double startTime, startHA;
    {
        MSColumns mscol(ms);
        IPosition shp = mscol.data().shape(0);
        nstk = shp(0);
        nchan = shp(1);
        // Find the start time and HA (from the first row).
        getStartHA(startTime, startHA, ms, 0);
    }

    // Create an iterator (on antenna) for the already sorted table.
    // Use the first chunk to create the hourangle vector.
    TableIterator tabiter(sorcal, sortNames, TableIterator::Ascending,
            TableIterator::NoSort);
    Vector<Float> havec;
    {
        Table tableChunk(tabiter.table());
        uInt n = tableChunk.nrow();
        MSSysCal syscal(tableChunk);
        MSSysCalColumns sysCalColumns(syscal);
        // Fill the hourangle vector (which is the same for all subsets).
        // Its unit is degrees; startHA is in fractions of a circle.
        // The time is in seconds, so convert that to a full day (circle).
        // Start the hourangle in degrees.
        havec.resize(n);
        Double factor = (Double(366.25) / 365.25) / (24 * 3600);
        for (uInt i = 0; i < n; i++) {
            havec(i) = 360 * (startHA + factor * (sysCalColumns.time()(i)
                    - startTime));
        }
    }
    // For the time being write only 2 values (first and last HA).
    // Until we know how to calculate the gain factor resulting
    // from the deformation of the mirror at given hourangles.
    IPosition shape(1, 2);
    Vector<Float> havec2(2 * nrif, 0.);
    for (uInt i = 0; i < nrif; i++) {
        havec2(2 * i) = havec(0);
        havec2(2 * i + 1) = havec(havec.nelements() - 1);
    }

    // Write the data for each antenna.
    // ##### Header
    Record header;
    header.define("EXTNAME", "AIPS GC"); // EXTNAME
    header.define("EXTVER", 1); // EXTVER
    header.define("OBSCODE", ""); // OBSCODE
    header.define("NO_POL", npol); // NO_POL
    header.define("NO_STKD", nstk); // NO_STKD
    header.define("STK_1", -5); // STK_1  (XX = -5)
    header.define("NO_BAND", Int(nrif)); // NO_BAND
    header.define("NO_CHAN", nchan); // NO_CHAN
    header.define("REF_FREQ", refFreq); // REF_FREQ
    header.define("CHAN_BW", abs(chanbw)); // CHAN_BW
    header.define("REF_PIXL", Double(1 + refPixelFreq)); // REF_PIXL (==CRPIX4)
    header.define("NO_TABS", Int(shape(0))); // NO_TABS
    header.define("TABREV", 2); // TABREV

    // Table description
    RecordDesc desc;
    Record stringLengths; // no strings
    Record units; // default to Hz
    desc.addField("ANTENNA_NO", TpInt);
    desc.addField("SUBARRAY", TpInt);
    desc.addField("FREQ ID", TpInt);
    desc.addField("TYPE_1", TpArrayInt, ifShape);
    desc.addField("NTERM_1", TpArrayInt, ifShape);
    desc.addField("X_TYP_1", TpArrayInt, ifShape);
    desc.addField("Y_TYP_1", TpArrayInt, ifShape);
    desc.addField("X_VAL_1", TpArrayFloat, ifShape);
    desc.addField("Y_VAL_1", TpArrayFloat, shape * nrif);
    units.define("Y_VAL_1", "DEGREES");
    desc.addField("GAIN_1", TpArrayFloat, shape * nrif);
    desc.addField("SENS_1", TpArrayFloat, ifShape);
    units.define("SENS_1", "K/JY");
    if (npol == 2) {
        desc.addField("TYPE_2", TpArrayInt, ifShape);
        desc.addField("NTERM_2", TpArrayInt, ifShape);
        desc.addField("X_TYP_2", TpArrayInt, ifShape);
        desc.addField("Y_TYP_2", TpArrayInt, ifShape);
        desc.addField("X_VAL_2", TpArrayFloat, ifShape);
        desc.addField("Y_VAL_2", TpArrayFloat, shape * nrif);
        units.define("Y_VAL_2", "DEGREES");
        desc.addField("GAIN_2", TpArrayFloat, shape * nrif);
        desc.addField("SENS_2", TpArrayFloat, ifShape);
        units.define("SENS_2", "K/JY");
    }

    FITSTableWriter writer(output.get(), desc, stringLengths, nentries, header,
            units, False);
    RecordFieldPtr<Int> antenna(writer.row(), "ANTENNA_NO");
    RecordFieldPtr<Int> arrayId(writer.row(), "SUBARRAY");
    RecordFieldPtr<Int> spwId(writer.row(), "FREQ ID");
    RecordFieldPtr<Array<Int> > type1(writer.row(), "TYPE_1");
    RecordFieldPtr<Array<Int> > nterm1(writer.row(), "NTERM_1");
    RecordFieldPtr<Array<Int> > xtype1(writer.row(), "X_TYP_1");
    RecordFieldPtr<Array<Int> > ytype1(writer.row(), "Y_TYP_1");
    RecordFieldPtr<Array<Float> > xval1(writer.row(), "X_VAL_1");
    RecordFieldPtr<Array<Float> > yval1(writer.row(), "Y_VAL_1");
    RecordFieldPtr<Array<Float> > gain1(writer.row(), "GAIN_1");
    RecordFieldPtr<Array<Float> > sens1(writer.row(), "SENS_1");
    RecordFieldPtr<Array<Int> > type2;
    RecordFieldPtr<Array<Int> > nterm2;
    RecordFieldPtr<Array<Int> > xtype2;
    RecordFieldPtr<Array<Int> > ytype2;
    RecordFieldPtr<Array<Float> > xval2;
    RecordFieldPtr<Array<Float> > yval2;
    RecordFieldPtr<Array<Float> > gain2;
    RecordFieldPtr<Array<Float> > sens2;
    if (npol == 2) {
        type2 = RecordFieldPtr<Array<Int> > (writer.row(), "TYPE_2");
        nterm2 = RecordFieldPtr<Array<Int> > (writer.row(), "NTERM_2");
        xtype2 = RecordFieldPtr<Array<Int> > (writer.row(), "X_TYP_2");
        ytype2 = RecordFieldPtr<Array<Int> > (writer.row(), "Y_TYP_2");
        xval2 = RecordFieldPtr<Array<Float> > (writer.row(), "X_VAL_2");
        yval2 = RecordFieldPtr<Array<Float> > (writer.row(), "Y_VAL_2");
        gain2 = RecordFieldPtr<Array<Float> > (writer.row(), "GAIN_2");
        sens2 = RecordFieldPtr<Array<Float> > (writer.row(), "SENS_2");
    }

    // The antenna numbers
    Vector<Int> antnums;
    _handleAntNumbers(ms, antnums);

    // Iterate through the table.
    // Each chunk should have the same size.
    while (!tabiter.pastEnd()) {
        Table tableChunk(tabiter.table());
        MSSysCal syscal(tableChunk);
        MSSysCalColumns sysCalColumns(syscal);
        //    *antenna = sysCalColumns.antennaId()(0) + 1;
        *antenna = antnums(sysCalColumns.antennaId()(0));
        //    *arrayId = sysCalColumns.arrayId()(0) + 1;
        *arrayId = 1;
        if (tableChunk.nrow() != havec.nelements()) {
            os << LogIO::SEVERE << "SysCal table is irregular!"
                    << " Mismatching #rows for antenna " << *antenna
                    << LogIO::POST;
            return False;
        }
        for (Int spw = 0; spw < nrspw; spw++) {
            *spwId = spw + 1;
            *type1 = 1; // tabulated values
            *nterm1 = shape(0);
            *xtype1 = 0; // none
            *ytype1 = 3; // hourangle
            *xval1 = 0;
            *yval1 = havec2;
            *gain1 = 1.0;
            *sens1 = sensitivity;
            if (npol == 2) {
                *type2 = 1; // tabulated values
                *nterm2 = shape(0);
                *xtype2 = 0;
                *ytype2 = 3;
                *xval2 = 0;
                *yval2 = havec2;
                *gain2 = 1.0;
                *sens2 = sensitivity;
            }
            // Write the current row
            writer.write();
        }
        tabiter++;
    }
    return True;
}

Bool MSFitsOutput::_writeWX(std::shared_ptr<FitsOutput> output, const MeasurementSet &ms) {
    LogIO os(LogOrigin("MSFitsOutput", __func__));
    const MSWeather subtable(ms.weather());
    MSWeatherColumns weatherColumns(subtable);
    const uInt nrow = subtable.nrow();

    if (nrow == 0) {
        os << LogIO::WARN << "No weather info" << LogIO::POST;
        return False;
    }
    // Get reference time (i.e. start time) from the main table.
    Double refTime;
    //{                                // get starttime (truncated to days)
    MSColumns mscol(ms);
    refTime = floor(mscol.time()(0) / C::day) * C::day;
    //}
    //MEpoch measTime = MSColumns(ms).timeMeas()(0);
    MEpoch measTime = mscol.timeMeas()(0);
    // ##### Header
    Record header;
    header.define("EXTNAME", "AIPS WX"); // EXTNAME
    header.define("EXTVER", 1); // EXTVER
    header.define("OBSCODE", ""); // PROGRAM CODE???
    header.define("RDATE", toFITSDate(measTime.get("s"))); // OBSERVING DATE (YYYYMMDD)
    header.define("TABREV", 3); // REVISION

    // Table description
    RecordDesc desc;
    Record stringLengths; // no strings
    Record units;

    desc.addField("TIME", TpDouble);
    units.define("TIME", "DAYS");
    desc.addField("TIME_INTERVAL", TpFloat);
    units.define("TIME_INTERVAL", "DAYS");
    desc.addField("ANTENNA_NO", TpInt);
    desc.addField("SUBARRAY", TpInt);
    desc.addField("TEMPERATURE", TpFloat);
    units.define("TEMPERATURE", "CENTIGRADE");
    desc.addField("PRESSURE", TpFloat);
    units.define("PRESSURE", "MILLIBAR");
    desc.addField("DEWPOINT", TpFloat);
    units.define("DEWPOINT", "CENTIGRADE");
    desc.addField("WIND_VELOCITY", TpFloat);
    units.define("WIND_VELOCITY", "M/SEC");
    desc.addField("WIND_DIRECTION", TpFloat);
    units.define("WIND_DIRECTION", "DEGREES"); // east from north
    desc.addField("WVR_H2O", TpFloat); // sometimes labeled as H2O COLUMN
    units.define("WVR_H2O", "m-2");
    desc.addField("IONOS_ELECTRON", TpFloat); //sometimes labeled as ELECTRON COL.
    units.define("IONOS_ELECTRON", "m-2");

    FITSTableWriter writer(output.get(), desc, stringLengths, nrow, header, units,
            False);
    RecordFieldPtr<Double> time(writer.row(), "TIME");
    RecordFieldPtr<Float> interval(writer.row(), "TIME_INTERVAL");
    RecordFieldPtr<Int> antenna(writer.row(), "ANTENNA_NO");
    RecordFieldPtr<Int> arrayId(writer.row(), "SUBARRAY");
    RecordFieldPtr<Float> temperature(writer.row(), "TEMPERATURE");
    RecordFieldPtr<Float> pressure(writer.row(), "PRESSURE");
    RecordFieldPtr<Float> dewpoint(writer.row(), "DEWPOINT");
    RecordFieldPtr<Float> windvelocity(writer.row(), "WIND_VELOCITY");
    RecordFieldPtr<Float> winddirection(writer.row(), "WIND_DIRECTION");
    RecordFieldPtr<Float> wvrh2o(writer.row(), "WVR_H2O");
    RecordFieldPtr<Float> ionoselectron(writer.row(), "IONOS_ELECTRON");

    Vector<Int> antnums;
    _handleAntNumbers(ms, antnums);
    //check optional columns
    Bool hasTemperature = !(weatherColumns.temperature().isNull());
    Bool hasPressure = !(weatherColumns.pressure().isNull());
    Bool hasDewPoint = !(weatherColumns.dewPoint().isNull());
    Bool hasWindVelocity = !(weatherColumns.windSpeed().isNull());
    Bool hasWindDirection = !(weatherColumns.windDirection().isNull());
    Bool hasWVRH2O = !(weatherColumns.H2O().isNull());
    Bool hasIonosElectron = !(weatherColumns.ionosElectron().isNull());

    for (uInt i = 0; i < nrow; i++) {
        Double tim = weatherColumns.time()(i);
        *time = (tim - refTime) / C::day;
        *interval = weatherColumns.interval()(i) / C::day;
        //*antenna = antnums( weatherColumns.antennaId()(i) );
        *antenna = (weatherColumns.antennaId()(i) == -1 ? 0 : antnums(
                weatherColumns.antennaId()(i)));
        //read optional columns
        // default 0.0
        // temperature, dewpoint in MS should be kelvin but
        // current WIDAR data looks like in C!
        if (hasTemperature) {
            *temperature = weatherColumns.temperature()(i) - 273.15;
        } else {
            *temperature = 0.0;
        }
        if (hasPressure) {
            //covert whatever units to mbar
            *pressure = weatherColumns.pressureQuant()(i).getValue(Unit("mbar"));
        } else {
            *pressure = 0.0;
        }
        if (hasDewPoint) {
            *dewpoint = weatherColumns.dewPoint()(i) - 273.15;
        } else {
            *dewpoint = 0.0;
        }
        if (hasWindVelocity) {
            *windvelocity = weatherColumns.windSpeed()(i);
        } else {
            *windvelocity = 0.0;
        }
        // direction in MS should be in rad but looks like deg...
        if (hasWindDirection) {
            *winddirection = weatherColumns.windDirectionQuant()(i).getValue(
                    "deg");
        } else {
            *winddirection = 0.0;
        }
        if (hasWVRH2O) {
            *wvrh2o = weatherColumns.H2O()(i);
        } else {
            *wvrh2o = 0.0;
        }
        if (hasIonosElectron) {
            *ionoselectron = weatherColumns.ionosElectron()(i);
        } else {
            *ionoselectron = 0.0;
        }
        writer.write();
    }
    return True;
}

// TODO uncommoment nspw when multiple IFs are supported
Bool MSFitsOutput::_writeSY(
    std::shared_ptr<FitsOutput> output, const MeasurementSet &ms, Table& syspower,
    Int /*nspw*/, const Block<Int>& spwIDMap, Bool combineSpw
) {
    LogIO os(LogOrigin("MSFitsOutput", __func__));
    static const String TIME = "TIME";
    static const String ANTENNA_ID = "ANTENNA_ID";
    static const String FEED_ID = "FEED_ID";
    static const String SPECTRAL_WINDOW_ID = "SPECTRAL_WINDOW_ID";
    //Table subtable(syspower.path().originalName());
    const auto td = syspower.tableDesc();
    const auto nrows = syspower.nrow();
    if (nrows == 0) {
        os << LogIO::WARN << "SYSPOWER table is empty." << LogIO::POST;
        return False;
    }
    static const std::vector<String> expColNames {
        ANTENNA_ID, FEED_ID, SPECTRAL_WINDOW_ID, TIME, "INTERVAL",
        "SWITCHED_DIFF", "SWITCHED_SUM", "REQUANTIZER_GAIN"
    };
    for (const String& cname: expColNames) {
        if (! td.isColumn(cname)) {
            os << LogIO::WARN << "Required column " << cname
                << " not found in SYSPOWER table" << LogIO::POST;
            return False;
        }
    }
    const ScalarColumn<Int> feedID(syspower, FEED_ID);
    const auto fv = feedID.getColumn();
    if (! allEQ(fv[0], fv)) {
        os << LogIO::WARN << "All FEED_IDs in SYSPOWER table are not identical"
            << LogIO::POST;
        return False;
    }
    // What to do based on the value of combineSpw follows the pattern
    // in _writeTy()
    // TODO currently not supporting writing multiple IFs. This code has been written in
    // a way that should make supporting multiple IFs fairly straight forward (the issue will
    // be adding support to MSFitsInput.cc, and in fact the reason I'm not supporting writing multiple
    // IFs to the uvfits table currently is because the only way to test that is to read the uvfits file
    // back in which requires support for multiple IFs in MSFitsInput.cc. The current requirement is to
    // write the SYSPOWER table to uvfits, not read it from uvfits, so the current implementation is
    // sufficient CAS-11860 )
    Int nrif = 1;
    if (combineSpw) {
        os << LogIO::WARN << "Combining spectral windows (multiple IFs) is currently not supported "
            << "for the SYSPOWER table" << LogIO::POST;
        combineSpw = False;
    }
    /*
    // this code is for when multiple IFs are supported
    if (combineSpw) {
        if (nrows % nspw == 0) {
            nrif = nspw;
        }
        else {
            nrif = 1;
            combineSpw = False;
            os << LogIO::WARN << "Number of rows (" << nrows << ") in SYSPOWER "
                << "table is not a multiple of the number of spectral windows ("
                << nspw << "). Cannot combine spectral windows in the UVFITS "
                << "SY table. Will instead write one spw per row."
                << LogIO::POST;
        }
    }
    */
    IPosition ifShape(1, nrif);
    const uInt nentries = nrows/nrif;
    os << LogIO::NORMAL << "Found " << nentries << " SY table entries ("
        << nrif << " IFs)" << LogIO::POST;
    if (combineSpw) {
        // ensure that the table is sorted correctly
        Block<String> sortNames(3);
        sortNames[0] = TIME;
        sortNames[1] = ANTENNA_ID;
        sortNames[2] = SPECTRAL_WINDOW_ID;
        syspower = syspower.sort(sortNames);
    }
    const ArrayColumn<Float> switchedDiff (syspower, "SWITCHED_DIFF");
    const Int npol = switchedDiff.shape(0)[0];
    Record header;
    header.define("EXTNAME", "AIPS SY");
    header.define("NO_IF", nrif);
    header.define("NO_POL", npol);
    header.define("NO_ANT", (Int)ms.antenna().nrow());
    RecordDesc desc;
    Record stringLengths; // no strings
    Record units;
    desc.addField(TIME, TpDouble);
    units.define(TIME, "DAYS");
    desc.addField("TIME INTERVAL", TpFloat);
    units.define("TIME INTERVAL", "DAYS");
    desc.addField("SOURCE ID", TpInt);
    desc.addField("ANTENNA NO.", TpInt);
    desc.addField("SUBARRAY", TpInt);
    desc.addField("FREQ ID", TpInt);
    desc.addField("POWER DIF1", TpArrayFloat, ifShape);
    units.define("POWER DIF1", "counts");
    desc.addField("POWER SUM1", TpArrayFloat, ifShape);
    units.define("POWER SUM1", "counts");
    desc.addField("POST GAIN1", TpArrayFloat, ifShape);
    if (npol == 2) {
        desc.addField("POWER DIF2", TpArrayFloat, ifShape);
        units.define("POWER DIF2", "counts");
        desc.addField("POWER SUM2", TpArrayFloat, ifShape);
        units.define("POWER SUM2", "counts");
        desc.addField("POST GAIN2", TpArrayFloat, ifShape);
    }
    FITSTableWriter writer(
        output.get(), desc, stringLengths, nentries, header,
        units, False
    );
    RecordFieldPtr<Double> time(writer.row(), TIME);
    RecordFieldPtr<Float> interval(writer.row(), "TIME INTERVAL");
    RecordFieldPtr<Int> sourceId(writer.row(), "SOURCE ID");
    RecordFieldPtr<Int> antenna(writer.row(), "ANTENNA NO.");
    RecordFieldPtr<Int> arrayId(writer.row(), "SUBARRAY");
    RecordFieldPtr<Int> freqId(writer.row(), "FREQ ID");
    RecordFieldPtr<Array<Float>> powerDif1(writer.row(), "POWER DIF1");
    RecordFieldPtr<Array<Float>> powerSum1(writer.row(), "POWER SUM1");
    RecordFieldPtr<Array<Float>> postGain1(writer.row(), "POST GAIN1");
    RecordFieldPtr<Array<Float>> powerDif2;
    RecordFieldPtr<Array<Float>> powerSum2;
    RecordFieldPtr<Array<Float>> postGain2;
    if (npol == 2) {
        powerDif2 = RecordFieldPtr<Array<Float>>(writer.row(), "POWER DIF2");
        powerSum2 = RecordFieldPtr<Array<Float>>(writer.row(), "POWER SUM2");
        postGain2 = RecordFieldPtr<Array<Float>>(writer.row(), "POST GAIN2");
    }

    Vector<Int> antnums;
    _handleAntNumbers(ms, antnums);
    MSMetaData md(&ms, 100);
    const ScalarColumn<Double> timeCol(syspower, TIME);
    const ScalarColumn<Double> intervalCol(syspower, "INTERVAL");
    const ScalarColumn<Int> antCol(syspower, ANTENNA_ID);
    const ScalarColumn<Int> spwCol(syspower, SPECTRAL_WINDOW_ID);
    const ArrayColumn<Float> switchedSum (syspower, "SWITCHED_SUM");
    const ArrayColumn<Float> qGDiff (syspower, "REQUANTIZER_GAIN");
    Vector<Float> pdv, psv, pgv;
    for (uInt i = 0; i < nrows; i += nrif) {
        const auto myTime = timeCol(i);
        *time = myTime/C::day;
        const auto myInterval = intervalCol(i);
        *interval = myInterval/(float)C::day;
        const auto fields = md.getFieldsForTimes(*time, *interval/2);
        const auto nfields = fields.size();
        if (nfields > 1) {
            os << LogIO::SEVERE << "Multiple fields found for time " << myTime
                << " and interval " << myInterval << ". Please file a bug report "
                << LogIO::POST;
            return False;
        }
        else if (nfields == 0) {
            os << LogIO::SEVERE << "No fields found for time " << myTime
                << " and interval " << myInterval << ". Please file a bug report "
                << LogIO::POST;
            return False;
        }
        *sourceId = *(fields.cbegin()) + 1;
        *antenna = antnums(antCol(i));
        // FIXME arrayid
        *arrayId = 1;
        *freqId = combineSpw ? 1 : 1 + spwIDMap[spwCol(i)];
        Vector<Float> pd1(nrif);
        Vector<Float> pd2(nrif);
        Vector<Float> ps1(nrif);
        Vector<Float> ps2(nrif);
        Vector<Float> pg1(nrif);
        Vector<Float> pg2(nrif);
        std::set<Int> spwSet;
        for (Int j = 0; j < nrif; ++j) {
            if (j > 0) {
                if (timeCol(i + j) != myTime) {
                    os << LogIO::SEVERE << "Irregularities in time values "
                        << "in the SYSPOWER subtable, so spectral window "
                        << "data cannot be combined when writing UVFITS "
                        << "SY table. Perhaps try combineSpw=False"
                        << LogIO::POST;
                    return False;
                }
                else if (antnums(antCol(i + j)) != *antenna) {
                    os << LogIO::SEVERE << "Irregularities in antenna_id "
                        << "values in the SYSPOWER subtable, so spectral "
                        << "window data cannot be combined when writing "
                        << "UVFITS SY table. Perhaps try combineSpw=False"
                        << LogIO::POST;
                    return False;
                }
            }
            switchedDiff.get(i + j, pdv);
            pd1[j] = pdv[0];
            switchedSum.get(i + j, psv);
            ps1[j] = psv[0];
            qGDiff.get(i + j, pgv);
            pg1[j] = pgv[0];
            if (npol == 2) {
                pd2[j] = pdv[1];
                ps2[j] = psv[1];
                pg2[j] = pgv[1];
            }
        }
        *powerDif1 = pd1;
        *powerSum1 = ps1;
        *postGain1 = pg1;
        if (npol == 2) {
            *powerDif2 = pd2;
            *powerSum2 = ps2;
            *postGain2 = pg2;
        }
        writer.write();
    }
    return True;
}

void MSFitsOutput::getStartHA(Double& startTime, Double& startHA,
        const MeasurementSet& ms, uInt rownr) {
    MSColumns mscol(ms);
    startTime = mscol.time()(rownr);
    MEpoch stTime = mscol.timeMeas()(rownr);
    Int fieldId = mscol.fieldId()(rownr);
    Int obsId = mscol.observationId()(rownr);
    // Get RA and DEC with their unit.
    MDirection delay(mscol.field().delayDirMeas(fieldId));

    // Get the observatory's position.
    String obsName = mscol.observation().telescopeName()(obsId);
    MPosition pos;
    MeasTable::Observatory(pos, obsName);

    // Use this position in a frame
    MeasFrame frame(pos);
    frame.set(stTime);
    MDirection out = MDirection::Convert(delay, MDirection::Ref(
            MDirection::HADEC, frame))();
    startHA = out.getAngle().getBaseValue()(0) / C::circle;
}

Table MSFitsOutput::handleSysCal(const MeasurementSet& ms,
        const Vector<Int>& spwids, Bool isSubset) {
    LogIO os(LogOrigin("MSFitsOutput", "handleSysCal"));
    Table syscal(ms.sysCal());
    // Only take the antennas found in the main table.
    // This is better and also solves an NFRA problem where incorrect
    // antennas were written in the SYSCAL table.
    Block<Bool> antFlag;
    {
        // Find the maximum antenna number.
        // Assure that the minimum >= 0.
        ScalarColumn<Int> ant1col(ms, MS::columnName(MS::ANTENNA1));
        ScalarColumn<Int> ant2col(ms, MS::columnName(MS::ANTENNA2));
        Vector<Int> ant1 = ant1col.getColumn();
        Vector<Int> ant2 = ant2col.getColumn();
        Int minant1, minant2, maxant1, maxant2;
        minMax(minant1, maxant1, ant1);
        minMax(minant2, maxant2, ant2);
        if (minant1 < 0 || minant2 < 0) {
            throw(AipsError("Antenna1 or antenna2 < 0 in MS " + ms.tableName()));
        }
        // Make an array which contains a flag True for all antennas in the
        // main table.
        Int nrant = 1 + max(maxant1, maxant2);
        antFlag.resize(nrant);
        antFlag = False;
        Bool delAnt1, delAnt2;
        const Int* ant1ptr = ant1.getStorage(delAnt1);
        const Int* ant2ptr = ant2.getStorage(delAnt2);
        uInt nrrow = ant1.nelements();
        for (uInt i = 0; i < nrrow; i++) {
            antFlag[ant1ptr[i]] = True;
            antFlag[ant2ptr[i]] = True;
        }
        ant1.freeStorage(ant1ptr, delAnt1);
        ant2.freeStorage(ant2ptr, delAnt2);
    }
    {
        // Now skip all antennas in SYSCAL not present in the main table.
        ScalarColumn<Int> antcol(syscal, MSSysCal::columnName(
                MSSysCal::ANTENNA_ID));
        Vector<Int> ant = antcol.getColumn();
        Int minant, maxant;
        minMax(minant, maxant, ant);
        if (minant < 0) {
            throw(AipsError("Antenna_id < 0 in SYSCAL " + syscal.tableName()));
        }
        uInt nrrow = ant.nelements();
        Block<Bool> rowFlag(nrrow);
        rowFlag = True;
        Bool flagged = False;
        Bool delAnt;
        const Int* antptr = ant.getStorage(delAnt);
        for (uInt i = 0; i < nrrow; i++) {
            if (!antFlag[antptr[i]]) {
                rowFlag[i] = False;
                flagged = True;
            }
        }
        ant.freeStorage(antptr, delAnt);
        if (flagged) {
            syscal = syscal(rowFlag);
            os << LogIO::NORMAL << "Skipped unused antennas in SYSCAL table ("
                    << nrrow - syscal.nrow() << " entries)" << LogIO::POST;
        }
    }
    // Skip first rows which maybe contain an average for each antenna.
    // This is an old WSRT feature/problem.
    {
        MSSysCalColumns sysCalColumns(ms.sysCal());
        Double sttim = sysCalColumns.time()(0);
        uInt nrow = sysCalColumns.time().nrow();
        for (uInt i = 0; i < nrow; i++) {
            Double tim = sysCalColumns.time()(i);
            if (tim != sttim) {
                if (tim < sttim) {
                    os << LogIO::NORMAL << "First time in SYSCAL table is "
                        "an average and will be skipped" << LogIO::POST;
                    syscal = syscal(syscal.nodeRownr() >= Int(i));
                }
                break;
            }
        }
    }
    // If the table is a subset, select the spectral-windows found in
    // the MS.
    if (isSubset) {
        syscal = syscal(syscal.col(MSSysCal::columnName(
                MSSysCal::SPECTRAL_WINDOW_ID)) .in(TableExprNode(spwids)));
    }
    // Sort the SYSCAL table in order of antenna, time, spectral-window.
    Block<String> sortNames(3);
    sortNames[0] = MSSysCal::columnName(MSSysCal::ANTENNA_ID);
    sortNames[1] = MSSysCal::columnName(MSSysCal::TIME);
    sortNames[2] = MSSysCal::columnName(MSSysCal::SPECTRAL_WINDOW_ID);
    return syscal.sort(sortNames);
}

/*
 allids: (input)  IDs to consider
 map:    (output) map from allids to 0,1,...,nr
 selids: (output) inverse of map

 returns: nr, number of selected IDs in allids
 */
Int MSFitsOutput::_makeIdMap(Block<Int>& map, Vector<Int>& selids, const Vector<
        Int>& allids) {
    // Determine the number of ids and make a mapping of
    // id number in the table to id number in fits.
    // Even if the MS is not a subset (by selection), we have to
    // determine this mapping explicitly (because then some ids
    // might be left out).

    Int nrid = 1 + max(allids);
    map.resize(nrid, True, True);
    map = -1;

    // Find out which fields are actually used, because only those
    // fields need to be written from the FIELD table.
    Bool deleteIt;
    const Int* data = allids.getStorage(deleteIt);
    Block<Bool> idUsed(nrid, False);
    Int nrow = allids.nelements();
    for (Int i = 0; i < nrow; i++) {
        idUsed[data[i]] = True;
    }
    allids.freeStorage(data, deleteIt);
    Int nr = 0;
    for (Int i = 0; i < nrid; i++) {
        if (idUsed[i]) {
            map[i] = nr++; // form the mapping
        }
    }
    selids.resize(nr);
    nr = 0;
    for (Int i = 0; i < nrid; i++) {
        if (idUsed[i]) {
            selids(nr++) = i; // determine which ids are selected
        }
    }

    return nr;
}

void MSFitsOutput::_handleAntNumbers(const MeasurementSet& ms,
        Vector<Int>& antnumbers) {

    // This method parses the MS ANTENNA NAME into a antenna
    //  number appropriate for the UVFITS output
    // For VLA antennas, the names are nominally numbers, and
    //  may be prepended with EA or VA.  These prefixes are
    //  properly stripped before the remaining string is parsed
    //  as a number.
    // For other telescopes, the name is used if it is a pure
    //  integer; otherwise the index + 1 is used (NB: AIPS demands
    //  one-basedness.)

    // Discern if which telescope
    MSObservationColumns obscol(ms.observation());
    String arrayName;
    if (obscol.nrow() > 0)
        arrayName = obscol.telescopeName()(0);

    MSAntennaColumns antcol(ms.antenna());
    ScalarColumn<String> antname(antcol.name());
    Int nAnt = antcol.nrow();

    antnumbers.resize(nAnt);

    for (Int iant = 0; iant < nAnt; ++iant) {
        String name;
        if (arrayName.contains("VLA"))
            // Trim leading EA/VA, if present
            name = antname(iant).from(RXint);
        else
            name = antname(iant);

        if (name.matches(RXint))
            antnumbers(iant) = atoi(name.chars());
        else {
            // at least one name isn't a number, so use use index+1 for ALL
            indgen(antnumbers);
            antnumbers += 1;
            break;
        }
    }

    //  cout << "antnumbers = " << antnumbers << endl;

}

// Local Variables:
// compile-command: "gmake MSFitsOutput"
// End:

} //# NAMESPACE CASACORE - END
