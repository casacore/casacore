//# Adios2StMan.cc: Base class of the ADIOS2 Storage Manager
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
//# Inc., 675 Massachusettes Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include "Adios2StManColumn.h"
#include <casacore/casa/Containers/Record.h>

namespace casacore
{

bool Adios2StMan::itsUsingMpi;
std::string Adios2StMan::itsAdiosEngineType;
adios2::Params Adios2StMan::itsAdiosEngineParams;
std::vector<adios2::Params> Adios2StMan::itsAdiosTransportParamsVec;

#ifdef HAVE_MPI
MPI_Comm Adios2StMan::itsMpiComm = MPI_COMM_WORLD;

Adios2StMan::Adios2StMan(MPI_Comm mpiComm) : DataManager()
{
    itsUsingMpi = true;
    itsMpiComm = mpiComm;
    std::string engineType;
    adios2::Params engineParams;
    std::vector<adios2::Params> transportParams;
    Adios2StManCommon(engineType, engineParams, transportParams);
}

Adios2StMan::Adios2StMan(
    MPI_Comm mpiComm, std::string engineType,
    std::map<std::string, std::string> engineParams,
    std::vector<std::map<std::string, std::string>> transportParams)
: DataManager()
{
    itsUsingMpi = true;
    itsMpiComm = mpiComm;
    Adios2StManCommon(engineType, engineParams, transportParams);
}
#endif

Adios2StMan::Adios2StMan() : DataManager()
{
    itsUsingMpi = false;
    std::string engineType;
    adios2::Params engineParams;
    std::vector<adios2::Params> transportParams;
    Adios2StManCommon(engineType, engineParams, transportParams);
}

Adios2StMan::Adios2StMan(
    std::string engineType, std::map<std::string, std::string> engineParams,
    std::vector<std::map<std::string, std::string>> transportParams)
: DataManager()
{
    itsUsingMpi = false;
    Adios2StManCommon(engineType, engineParams, transportParams);
}

Adios2StMan::~Adios2StMan()
{
    if (itsAdiosEngine)
    {
        itsAdiosEngine->EndStep();
        itsAdiosEngine->Close();
    }
}

void Adios2StMan::Adios2StManCommon(
    const std::string &engineType,
    const std::map<std::string, std::string> &engineParams,
    const std::vector<std::map<std::string, std::string>> &transportParams)
{

    itsAdiosEngineType = engineType;
    itsAdiosEngineParams = engineParams;
    itsAdiosTransportParamsVec = transportParams;

    if (Adios2StMan::itsUsingMpi)
    {
#ifdef HAVE_MPI
        itsAdios =
            std::make_shared<adios2::ADIOS>(Adios2StMan::itsMpiComm, true);
#else
        throw(std::runtime_error("Adios2StMan using MPI but HAVE_MPI is not "
                                 "defined. This should never happen"));
        itsAdios = std::make_shared<adios2::ADIOS>(true);
#endif
    }
    else
    {
        itsAdios = std::make_shared<adios2::ADIOS>(true);
    }

    itsAdiosIO =
        std::make_shared<adios2::IO>(itsAdios->DeclareIO("Adios2StMan"));

    if (itsAdiosEngineType.empty() == false)
    {
        itsAdiosIO->SetEngine(itsAdiosEngineType);
    }
    if (itsAdiosEngineParams.empty() == false)
    {
        itsAdiosIO->SetParameters(itsAdiosEngineParams);
    }
    for (size_t i = 0; i < itsAdiosTransportParamsVec.size(); ++i)
    {
        std::string transportName = std::to_string(i);
        auto j = itsAdiosTransportParamsVec[i].find("Name");
        if (j != itsAdiosTransportParamsVec[i].end())
        {
            transportName = j->second;
        }
        itsAdiosIO->AddTransport(transportName, itsAdiosTransportParamsVec[i]);
    }
}

DataManager *Adios2StMan::makeObject(const String &/*aDataManType*/,
                                     const Record &/*spec*/)
{
    if (Adios2StMan::itsUsingMpi)
    {
#ifdef HAVE_MPI
        return new Adios2StMan(itsMpiComm, itsAdiosEngineType,
                               itsAdiosEngineParams,
                               itsAdiosTransportParamsVec);
#else
        throw(std::runtime_error("Adios2StMan using MPI but HAVE_MPI is not "
                                 "defined. This should never happen"));
        return new Adios2StMan(itsAdiosEngineType, itsAdiosEngineParams,
                               itsAdiosTransportParamsVec);
#endif
    }
    else
    {
        return new Adios2StMan(itsAdiosEngineType, itsAdiosEngineParams,
                               itsAdiosTransportParamsVec);
    }
}

DataManager *Adios2StMan::clone() const
{
    Record tmp;
    return makeObject(itsDataManName, tmp);
}

String Adios2StMan::dataManagerType() const { return itsDataManName; }

void Adios2StMan::addRow(uInt /*aNrRows*/) {}

void Adios2StMan::create(uInt aNrRows)
{
    itsOpenMode = 'w';
    itsNrRows = aNrRows;
    itsAdiosEngine = std::make_shared<adios2::Engine>(
        itsAdiosIO->Open(fileName(), adios2::Mode::Write));
    for (uInt i = 0; i < ncolumn(); ++i)
    {
        itsColumnPtrBlk[i]->create(aNrRows, itsAdiosEngine, itsOpenMode);
    }
    itsAdiosEngine->BeginStep();
}

void Adios2StMan::open(uInt aNrRows, AipsIO &ios)
{
    itsOpenMode = 'r';
    itsNrRows = aNrRows;
    itsAdiosEngine = std::make_shared<adios2::Engine>(
        itsAdiosIO->Open(fileName(), adios2::Mode::Read));
    for (uInt i = 0; i < ncolumn(); ++i)
    {
        itsColumnPtrBlk[i]->create(aNrRows, itsAdiosEngine, itsOpenMode);
    }
    itsAdiosEngine->BeginStep();

    ios.getstart(itsDataManName);
    ios >> itsDataManName;
    ios >> itsStManColumnType;
    ios.getend();
    itsNrRows = aNrRows;
}

void Adios2StMan::deleteManager() {}

DataManagerColumn *Adios2StMan::makeScalarColumn(const String &name,
                                                 int aDataType,
                                                 const String &dataTypeId)
{
    return makeColumnCommon(name, aDataType, dataTypeId);
}

DataManagerColumn *Adios2StMan::makeDirArrColumn(const String &name,
                                                 int aDataType,
                                                 const String &dataTypeId)
{
    return makeColumnCommon(name, aDataType, dataTypeId);
}

DataManagerColumn *Adios2StMan::makeIndArrColumn(const String &name,
                                                 int aDataType,
                                                 const String &dataTypeId)
{
    return makeColumnCommon(name, aDataType, dataTypeId);
}

DataManagerColumn *Adios2StMan::makeColumnCommon(const String &name,
                                                 int aDataType,
                                                 const String &/*dataTypeId*/)
{
    if (ncolumn() >= itsColumnPtrBlk.nelements())
    {
        itsColumnPtrBlk.resize(itsColumnPtrBlk.nelements() + 32);
    }
    Adios2StManColumn *aColumn;
    switch (aDataType)
    {
        case TpBool:
        case TpArrayBool:
            aColumn = new Adios2StManColumnT<unsigned char>(this, aDataType, name, itsAdiosIO);
            break;
        case TpChar:
        case TpArrayChar:
            aColumn = new Adios2StManColumnT<char>(this, aDataType, name, itsAdiosIO);
            break;
        case TpUChar:
        case TpArrayUChar:
            aColumn = new Adios2StManColumnT<unsigned char>(this, aDataType, name, itsAdiosIO);
            break;
        case TpShort:
        case TpArrayShort:
            aColumn = new Adios2StManColumnT<short>(this, aDataType, name, itsAdiosIO);
            break;
        case TpUShort:
        case TpArrayUShort:
            aColumn = new Adios2StManColumnT<unsigned short>(this, aDataType, name, itsAdiosIO);
            break;
        case TpInt:
        case TpArrayInt:
            aColumn = new Adios2StManColumnT<int>(this, aDataType, name, itsAdiosIO);
            break;
        case TpUInt:
        case TpArrayUInt:
            aColumn = new Adios2StManColumnT<unsigned int>(this, aDataType, name, itsAdiosIO);
            break;
        case TpFloat:
        case TpArrayFloat:
            aColumn = new Adios2StManColumnT<float>(this, aDataType, name, itsAdiosIO);
            break;
        case TpDouble:
        case TpArrayDouble:
            aColumn = new Adios2StManColumnT<double>(this, aDataType, name, itsAdiosIO);
            break;
        case TpComplex:
        case TpArrayComplex:
            aColumn = new Adios2StManColumnT<std::complex<float>>(this, aDataType, name, itsAdiosIO);
            break;
        case TpDComplex:
        case TpArrayDComplex:
            aColumn = new Adios2StManColumnT<std::complex<double>>(this, aDataType, name, itsAdiosIO);
            break;
        case TpString:
        case TpArrayString:
            aColumn = new Adios2StManColumnT<std::string>(this, aDataType, name, itsAdiosIO);
            break;
    }
    itsColumnPtrBlk[ncolumn()] = aColumn;
    return aColumn;
}

uInt Adios2StMan::getNrRows() { return itsNrRows; }

void Adios2StMan::resync(uInt /*aNrRows*/) {}

Bool Adios2StMan::flush(AipsIO &ios, Bool /*doFsync*/)
{
    ios.putstart(itsDataManName, 2);
    ios << itsDataManName;
    ios << itsStManColumnType;
    ios.putend();
    return true;
}

String Adios2StMan::dataManagerName() const { return itsDataManName; }

void register_adios2stman()
{
    DataManager::registerCtor("Adios2StMan", Adios2StMan::makeObject);
}
}
