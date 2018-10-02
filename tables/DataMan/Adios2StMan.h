//# Adios2StMan.h: Base class of the ADIOS2 Storage Manager
//# Copyright (C) 2018
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

#ifndef ADIOS2STMAN_H
#define ADIOS2STMAN_H

#include <adios2.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/tables/Tables/Table.h>

namespace casacore
{

class Adios2StManColumn;

class Adios2StMan : public DataManager
{
public:
    Adios2StMan();
    Adios2StMan(
        std::string engineType, std::map<std::string, std::string> engineParams,
        std::vector<std::map<std::string, std::string>> transportParams);
#ifdef HAVE_MPI
    Adios2StMan(MPI_Comm mpiComm);
    Adios2StMan(
        MPI_Comm mpiComm, std::string engineType,
        std::map<std::string, std::string> engineParams,
        std::vector<std::map<std::string, std::string>> transportParams);
#endif
    void Adios2StManCommon(
        const std::string &engineType,
        const std::map<std::string, std::string> &engineParams,
        const std::vector<std::map<std::string, std::string>> &transportParams);
    virtual ~Adios2StMan();
    virtual DataManager *clone() const;
    virtual String dataManagerType() const;
    virtual String dataManagerName() const;
    virtual void create(uInt aNrRows);
    virtual void open(uInt aRowNr, AipsIO &);
    virtual void resync(uInt aRowNr);
    virtual Bool flush(AipsIO &, Bool doFsync);
    DataManagerColumn *makeColumnCommon(const String &aName, int aDataType,
                                        const String &aDataTypeID);
    virtual DataManagerColumn *makeScalarColumn(const String &aName,
                                                int aDataType,
                                                const String &aDataTypeID);
    virtual DataManagerColumn *makeDirArrColumn(const String &aName,
                                                int aDataType,
                                                const String &aDataTypeID);
    virtual DataManagerColumn *makeIndArrColumn(const String &aName,
                                                int aDataType,
                                                const String &aDataTypeID);
    virtual void deleteManager();
    virtual void addRow(uInt aNrRows);
    static DataManager *makeObject(const String &aDataManType,
                                   const Record &spec);
    uInt getNrRows();

private:
    String itsDataManName = "Adios2StMan";
    uInt itsRows;
    uInt itsRowsPerVariable = 100;
    int itsStManColumnType;
    PtrBlock<Adios2StManColumn *> itsColumnPtrBlk;

    std::shared_ptr<adios2::ADIOS> itsAdios;
    std::shared_ptr<adios2::IO> itsAdiosIO;
    std::shared_ptr<adios2::Engine> itsAdiosEngine;

    char itsOpenMode;

    static std::string itsAdiosEngineType;
    static adios2::Params itsAdiosEngineParams;
    static std::vector<adios2::Params> itsAdiosTransportParamsVec;

    static bool itsUsingMpi;
#ifdef HAVE_MPI
    static MPI_Comm itsMpiComm;
#endif

}; // end of class Adios2StMan

extern "C" void register_adios2stman();
} // end of namespace casa

#endif
