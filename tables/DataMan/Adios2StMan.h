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

#include <map>
#include <memory>
#include <string>
#include <vector>

#include <mpi.h>

#include <casacore/casa/IO/AipsIO.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/tables/Tables/Table.h>

namespace casacore
{

class Adios2StMan : public DataManager
{
    friend class Adios2StManColumn;
    template<typename T> friend class Adios2StManColumnT;
public:
    Adios2StMan(MPI_Comm mpiComm = MPI_COMM_WORLD);

    Adios2StMan(MPI_Comm mpiComm,
            std::string engineType,
            std::map<std::string, std::string> engineParams,
            std::vector<std::map<std::string, std::string>> transportParams,
            std::vector<std::map<std::string, std::string>> operatorParams);

    virtual ~Adios2StMan();

    virtual DataManager *clone() const;
    virtual String dataManagerType() const;
    virtual String dataManagerName() const;
    virtual void create64(rownr_t aNrRows);
    virtual rownr_t open64(rownr_t aRowNr, AipsIO &ios);
    virtual rownr_t resync64(rownr_t aRowNr);
    virtual Bool flush(AipsIO &, Bool doFsync);
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
    virtual void addRow64(rownr_t aNrRows);
    static DataManager *makeObject(const String &aDataManType,
                                   const Record &spec);
    Record dataManagerSpec() const;
    rownr_t getNrRows();

private:
    class impl;
    std::unique_ptr<impl> pimpl;
}; // end of class Adios2StMan

extern "C" void register_adios2stman();
} // end of namespace casa

#endif
