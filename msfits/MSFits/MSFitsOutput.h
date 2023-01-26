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

#ifndef MS_MSFITSOUTPUT_H
#define MS_MSFITSOUTPUT_H

#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>

#include <casacore/casa/aips.h>

namespace casacore {

//# Forward Declarations
class File;
class FitsOutput;
template<class T> class ScalarColumn;
class Table;
template<class T> class Block;
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
        int32_t startChan, int32_t nchan, int32_t stepChan, int32_t avgChan
    );

    //  @param writeSysCal   whether to write the system calibration table
    void setWriteSysCal(bool writeSysCal);

    //  @param asMultiSource If true a multi-source UVFits file is written.
    void setAsMultiSource(bool asMultiSource);

    //  @param combineSpw    If true it attempts to write the spectral windows as
    //                       IFs.  This is necessary for many aips tasks, and
    //                       for difmap.
    void setCombineSpw(bool combineSpw);

    //  @param writeStation  If true uses pad instead of antenna names.
    void setWriteStation(bool writeStation);

    void setSensitivity(double sensitivity);

    //  @param padWithFlags  If true and combineSpw==true, fill spws with flags
    //                       as needed to fit the IF structure.  Does not yet
    //                       support spws with different shapes.
    void setPadWitFlags(bool padWithFlags);

    void setFieldNumber(uint32_t fieldNumber);

    //  @param overwrite     overwrite existing file?
    void setOverwrite(bool overwrite);

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
    static bool writeFitsFile(
        const String& fitsfile, const MeasurementSet& ms,
        const String& column, int32_t startchan=0,
        int32_t nchan=1, int32_t stepchan=1,
        bool writeSysCal = false,
        bool asMultiSource = false, bool combineSpw=false,
        bool writeStation=false, double sensitivity=1.0,
        const bool padWithFlags=false, int32_t avgchan=1,
        uint32_t fieldNumber=0, bool overwrite=false
    );

private:
    const String _fitsfile, _column;
    const MeasurementSet _ms;
    int32_t _startChan, _nchan, _stepChan, _avgChan;
    bool _writeSysCal, _asMultiSource, _combineSpw,
        _writeStation, _padWithFlags, _overwrite;
    double _sensitivity;
    uint32_t _fieldNumber;


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
        int32_t& refPixelFreq, double& refFreq,
        double& chanbw, const String& outFITSFile,
        const Block<int32_t>& spwidMap, int32_t nrspw,
        const Block<int32_t>& fieldidMap,
        bool asMultiSource
    ) const;

    // Write the FQ table.
    // If combineSpw is true, all spectral-windows are written in one
    // row of the FITS table.
    static bool _writeFQ(std::shared_ptr<FitsOutput> output, const MeasurementSet& ms,
        const Block<int32_t>& spwidMap, int32_t nrspw,
        double refFreq, int32_t refPixelFreq,
        double chanbw, bool combineSpw,
        int32_t chanstart = 0, int32_t nchan = -1, int32_t chanstep = 1,
        int32_t avgchan = 1
    );

    // Write the AN table.
    static bool _writeAN(
        std::shared_ptr<FitsOutput> output, const MeasurementSet& ms,
        double refFreq, bool writeStation
    );

    // Write the SU table.
    static bool _writeSU(
        std::shared_ptr<FitsOutput> output, const MeasurementSet& ms,
        const Block<int32_t>& fieldidMap, int32_t nrfield,
		const Block<int32_t>& spwidMap, int32_t nrspw
    );

    // Write the TY table.
    static bool _writeTY(
        std::shared_ptr<FitsOutput> output, const MeasurementSet& ms,
        const Table& syscal, const Block<int32_t>& spwidMap,
        uint32_t nrif, bool combineSpw
    );

    // Write the GC table.
    static bool _writeGC(
        std::shared_ptr<FitsOutput> output, const MeasurementSet& ms,
        const Table& syscal, const Block<int32_t>& spwidMap,
        uint32_t nrif, bool combineSpw, double sensitivity,
        int32_t refPixelFreq, double refFreq, double chanbw
    );

    // Write the WX table.
    static bool _writeWX(std::shared_ptr<FitsOutput> output, const MeasurementSet& ms);

    // Write the SY table.
    static bool _writeSY(
        std::shared_ptr<FitsOutput> output, const MeasurementSet& ms,
        Table& syspower, int32_t nspw, const Block<int32_t>& spwIDMap,
        bool combineSpw
    );

    // Convert time to day and fraction.
    static void timeToDay(int32_t& day, double& dayFraction, double time);

    // Get the time and hourangle from the MS at the given row.
    // It uses the field-id and observation-id to calculate the hourangle.
    static void getStartHA (
        double& startTime, double& startHA,
        const MeasurementSet& ms, uint32_t rownr
    );

    // Discern the antenna numbers that go into UVFITS
    static void _handleAntNumbers(const MeasurementSet& ms,Vector<int32_t>& antnumbers);

    // Handle the SYSCAL table.
    // It skips the entries not needed and sorts it in the correct order.
    static Table handleSysCal (
        const MeasurementSet& ms,
        const Vector<int32_t>& spwids, bool isSubset
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

    static int32_t _makeIdMap(
        Block<int32_t>& map, Vector<int32_t>& selids,
        const Vector<int32_t>& allids
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
    static uint32_t get_tbf_end(
        const uint32_t rownr, const uint32_t nrow, const uint32_t nif,
        const ScalarColumn<double>& timec,
        const ScalarColumn<double>& ininterval,
        const ScalarColumn<int32_t>& ant1,
        const ScalarColumn<int32_t>& ant2,
        const bool asMultiSource,
        const ScalarColumn<int32_t>& fieldid
    );

    static void _checkReceptorAngles(
        const Vector<Quantity>& ra0, Vector<Quantity>& ra1, int32_t antnum
    );
};

} //# NAMESPACE CASACORE - END

#endif
