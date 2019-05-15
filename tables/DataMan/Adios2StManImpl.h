//# Adios2StManImpl.h: Implementation class definition of the ADIOS2 Storage Manager
//
//# ICRAR - International Centre for Radio Astronomy Research
//# (c) UWA - The University of Western Australia, 2018
//# Copyright by UWA (in the framework of the ICRAR)
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
//# Inc., 675 Massachusettes Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#

#ifndef ADIOS2STMANIMPL_H
#define ADIOS2STMANIMPL_H

#include <adios2.h>

#include "Adios2StMan.h"

namespace casacore
{

class Adios2StManColumn;

class Adios2StMan::impl
{
public:
    impl(Adios2StMan &parent, MPI_Comm mpiComm = MPI_COMM_WORLD);
    impl(Adios2StMan &parent, MPI_Comm mpiComm, std::string engineType,
            std::map<std::string, std::string> engineParams,
            std::vector<std::map<std::string, std::string>> transportParams);

    ~impl();

    DataManager *clone() const;
    String dataManagerType() const;
    String dataManagerName() const;
    void create(uInt aNrRows);
    void open(uInt aRowNr, AipsIO &ios);
    void resync(uInt aRowNr);
    Bool flush(AipsIO &ios, Bool doFsync);
    DataManagerColumn *makeColumnCommon(const String &aName, int aDataType,
                                        const String &aDataTypeID);
    DataManagerColumn *makeScalarColumn(const String &aName,
                                        int aDataType,
                                        const String &aDataTypeID);
    DataManagerColumn *makeDirArrColumn(const String &aName,
                                        int aDataType,
                                        const String &aDataTypeID);
    DataManagerColumn *makeIndArrColumn(const String &aName,
                                        int aDataType,
                                        const String &aDataTypeID);
    void deleteManager();
    void addRow(uInt aNrRows);
    static DataManager *makeObject(const String &aDataManType,
                                   const Record &spec);
    uInt getNrRows();

private:
    Adios2StMan &parent;
    String itsDataManName = "Adios2StMan";
    uInt itsRows;
    int itsStManColumnType;
    PtrBlock<Adios2StManColumn *> itsColumnPtrBlk;

    std::shared_ptr<adios2::ADIOS> itsAdios;
    std::shared_ptr<adios2::IO> itsAdiosIO;
    std::shared_ptr<adios2::Engine> itsAdiosEngine;

    char itsOpenMode;

    static std::string itsAdiosEngineType;
    static adios2::Params itsAdiosEngineParams;
    static std::vector<adios2::Params> itsAdiosTransportParamsVec;

    static MPI_Comm itsMpiComm;

    uInt ncolumn() const { return parent.ncolumn(); }
    String fileName() const { return parent.fileName(); }
};

} // namespace casacore

#endif // ADIOS2STMANIMPL_H
