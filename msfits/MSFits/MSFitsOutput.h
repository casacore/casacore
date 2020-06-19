//# MSFitsOutput.h:  Write a MeasurementSet to a random group uvfits file
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2003
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
//# $Id: MSFitsOutput.h 21521 2014-12-10 08:06:42Z gervandiepen $

#ifndef MS_MSFITSOUTPUT_H
#define MS_MSFITSOUTPUT_H

#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>

#include <casacore/casa/aips.h>

namespace casacore {

//# Forward Declarations
class FitsOutput;
template<class T> class ScalarColumn;
class Table;
template<class T> class Block;
template<class T> class Vector;

// <summary>
// Write a MeasurementSet to a random group uvfits file.
// </summary>

class MSFitsOutput {
public:
    //  @param fitsfile      Output filename
    //  @param ms            input
    //  @param column        specifies which "data" column to write
    //                       ("observed", "calibrated", "model")
    MSFitsOutput(
        const String& fitsfile, const MeasurementSet& ms,
        const String& column
    );

    //  @param startChan     1st channel
    //  @param nchan         # of channels
    //  @param stepChan      # of channels to stride by
    //  @param avgChan       average every N channels
    void setChannelInfo(
        Int startChan, Int nchan, Int stepChan, Int avgChan
    );

    //  @param writeSysCal   whether to write the system calibration table
    void setWriteSysCal(Bool writeSysCal);

    //  @param asMultiSource If true a multi-source UVFits file is written.
    void setAsMultiSource(Bool asMultiSource);

    //  @param combineSpw    If true it attempts to write the spectral windows as
    //                       IFs.  This is necessary for many aips tasks, and
    //                       for difmap.
    void setCombineSpw(Bool combineSpw);

    //  @param writeStation  If true uses pad instead of antenna names.
    void setWriteStation(Bool writeStation);

    void setSensitivity(Double sensitivity);

    //  @param padWithFlags  If true and combineSpw==true, fill spws with flags
    //                       as needed to fit the IF structure.  Does not yet
    //                       support spws with different shapes.
    void setPadWitFlags(Bool padWithFlags);

    void setFieldNumber(uInt fieldNumber);

    //  @param overwrite     overwrite existing file?
    void setOverwrite(Bool overwrite);

    // write the uvfits file.
    void write() const;

    // Convert a MeasurementSet to random group UVFITS.
    //  @param fitsfile      Output filename
    //  @param ms            input
    //  @param column        specifies which "data" column to write
    //                       ("observed", "calibrated", "model")
    //  @param startchan     1st channel
    //  @param nchan         # of channels
    //  @param stepchan      # of channels to stride by
    //  @param writeSysCal   whether to write the system calibration table
    //  @param asMultiSource If true a multi-source UVFits file is written.
    //  @param combineSpw    If true it attempts to write the spectral windows as
    //                       IFs.  This is necessary for many aips tasks, and
    //                       for difmap.
    //  @param writeStation  If true uses pad instead of antenna names.
    //  @param sensitivity
    //  @param padWithFlags  If true and combineSpw==true, fill spws with flags
    //                       as needed to fit the IF structure.  Does not yet
    //                       support spws with different shapes.
    //  @param avgchan       average every N channels
    //  @param fieldNumber   field number
    //  @param overwrite     overwrite existing file?
    static Bool writeFitsFile(
        const String& fitsfile, const MeasurementSet& ms,
        const String& column, Int startchan=0,
        Int nchan=1, Int stepchan=1,
        Bool writeSysCal = False,
        Bool asMultiSource = False, Bool combineSpw=False,
        Bool writeStation=False, Double sensitivity=1.0,
        const Bool padWithFlags=false, Int avgchan=1,
        uInt fieldNumber=0, Bool overwrite=False
    );

private:
    const String _fitsfile, _column;
    const MeasurementSet _ms;
    Int _startChan, _nchan, _stepChan, _avgChan;
    Bool _writeSysCal, _asMultiSource, _combineSpw,
        _writeStation, _padWithFlags, _overwrite;
    Double _sensitivity;
    uInt _fieldNumber;


    // Write the main table.
    //    @param refPixelFreq
    //    @param refFreq
    //    @param chanbw
    //    @param outFITSFile
    //    @param spwidMap       spwidMap[inp_spw] = output_spw, if inp_spw is selected
    //                                              -1 otherwise.
    //    @param nrspw          # of selected spws.
    //    @param fieldidMap     fieldidMap[inp_fld] = output_fld, if inp_fld is selected
    //                                                -1 otherwise.
    //    @param asMultiSource  If true, write a multisource UVFITS file.
    std::shared_ptr<FitsOutput> _writeMain(
        Int& refPixelFreq, Double& refFreq,
        Double& chanbw, const String& outFITSFile,
        const Block<Int>& spwidMap, Int nrspw,
        const Block<Int>& fieldidMap,
        Bool asMultiSource
    ) const;

    // Write the FQ table.
    // If combineSpw is True, all spectral-windows are written in one
    // row of the FITS table.
    static Bool _writeFQ(std::shared_ptr<FitsOutput> output, const MeasurementSet& ms,
        const Block<Int>& spwidMap, Int nrspw,
        Double refFreq, Int refPixelFreq,
        Double chanbw, Bool combineSpw,
        Int chanstart = 0, Int nchan = -1, Int chanstep = 1,
        Int avgchan = 1
    );

    // Write the AN table.
    static Bool _writeAN(
        std::shared_ptr<FitsOutput> output, const MeasurementSet& ms,
        Double refFreq, Bool writeStation
    );

    // Write the SU table.
    static Bool _writeSU(
        std::shared_ptr<FitsOutput> output, const MeasurementSet& ms,
        const Block<Int>& fieldidMap, Int nrfield,
		const Block<Int>& spwidMap, Int nrspw
    );

    // Write the TY table.
    static Bool _writeTY(
        std::shared_ptr<FitsOutput> output, const MeasurementSet& ms,
        const Table& syscal, const Block<Int>& spwidMap,
        uInt nrif, Bool combineSpw
    );

    // Write the GC table.
    static Bool _writeGC(
        std::shared_ptr<FitsOutput> output, const MeasurementSet& ms,
        const Table& syscal, const Block<Int>& spwidMap,
        uInt nrif, Bool combineSpw, Double sensitivity,
        Int refPixelFreq, Double refFreq, Double chanbw
    );

    // Write the WX table.
    static Bool _writeWX(std::shared_ptr<FitsOutput> output, const MeasurementSet& ms);

    // Convert time to day and fraction.
    static void timeToDay(Int& day, Double& dayFraction, Double time);

    // Get the time and hourangle from the MS at the given row.
    // It uses the field-id and observation-id to calculate the hourangle.
    static void getStartHA (
        Double& startTime, Double& startHA,
        const MeasurementSet& ms, uInt rownr
    );

    // Discern the antenna numbers that go into UVFITS
    static void handleAntNumbers(const MeasurementSet& ms,Vector<Int>& antnumbers);

    // Handle the SYSCAL table.
    // It skips the entries not needed and sorts it in the correct order.
    static Table handleSysCal (
        const MeasurementSet& ms,
        const Vector<Int>& spwids, Bool isSubset
    );

    // Determine which ids are selected in the main table
    // (used for fields and spectral-window).
    //    @param map    (Really an output here, not an input.)
    //                  spwidMap[inp_id] = output_id, if inp_id is selected
    //                                     -1 otherwise.
    //    @param selids (Really an output here, not an input.)
    //                  A list of the selected input IDs.
    //    @param allids (Really is an input, not an output!)
    //                  IDs to consider.
    //    @return number of selected IDs in allids

    static Int _makeIdMap(
        Block<Int>& map, Vector<Int>& selids,
        const Vector<Int>& allids
    );

    // Find the end of a group of rows with the same
    // time(_centroid) (within 0.25 * ininterval(rownr)),
    // baseline #,
    // and, if asMultiSource, field ID.
    //    @param rownr          Row # to start from.
    //    @param nrow           # of rows in the columns.
    //    @param nif            # of IFs
    //    @param timec          time(_centroid) col
    //    @param ininterval     used to set tolerance on changes in timec.
    //    @param ant1           ID of baseline's antenna 1.
    //    @param ant2           ID of baseline's antenna 2.
    //    @param asMultiSource  If false, treat fieldid as unattached + prone to segfault
    //    @param fieldid
    //    @return Last row # with the same time, baseline, and apparent field as rownr.
    //    @warning Assumes that the columns are sorted by time(_centroid), ant1,
    //             ant2 (, field, DDID).
    static uInt get_tbf_end(
        const uInt rownr, const uInt nrow, const uInt nif,
        const ScalarColumn<Double>& timec,
        const ScalarColumn<Double>& ininterval,
        const ScalarColumn<Int>& ant1,
        const ScalarColumn<Int>& ant2,
        const Bool asMultiSource,
        const ScalarColumn<Int>& fieldid
    );

    static void _checkReceptorAngles(
        const Vector<Quantity>& ra0, Vector<Quantity>& ra1, Int antnum
    );
};

} //# NAMESPACE CASACORE - END

#endif
