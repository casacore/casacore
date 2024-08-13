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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA


#include "Adios2StManImpl.h"
#include "Adios2StManColumn.h"
#include <casacore/casa/Containers/Record.h>
#include <casacore/tables/DataMan/DataManError.h>

namespace casacore
{

// Static objects in impl class
#ifdef HAVE_MPI
MPI_Comm Adios2StMan::impl::itsMpiComm = MPI_COMM_WORLD;
#endif

constexpr const char *Adios2StMan::impl::SPEC_FIELD_XML_FILE;
constexpr const char *Adios2StMan::impl::SPEC_FIELD_ENGINE_TYPE;
constexpr const char *Adios2StMan::impl::SPEC_FIELD_ENGINE_PARAMS;
constexpr const char *Adios2StMan::impl::SPEC_FIELD_TRANSPORT_PARAMS;
constexpr const char *Adios2StMan::impl::SPEC_FIELD_OPERATOR_PARAMS;

//
// Adios2StMan implementation in terms of the impl class
//
#ifdef HAVE_MPI
Adios2StMan::Adios2StMan(MPI_Comm mpiComm, std::string engineType,
    std::map<std::string, std::string> engineParams,
    std::vector<std::map<std::string, std::string>> transportParams,
    std::vector<std::map<std::string, std::string>> operatorParams
)
    : Adios2StMan(mpiComm, move(engineType), move(engineParams), move(transportParams), move(operatorParams), {})
{
}

Adios2StMan::Adios2StMan(MPI_Comm mpiComm, std::string configFile, from_config_t)
    : Adios2StMan(mpiComm, {}, {}, {}, {}, move(configFile))
{
}

Adios2StMan::Adios2StMan(MPI_Comm mpiComm, std::string engineType,
     std::map<std::string, std::string> engineParams,
     std::vector<std::map<std::string, std::string>> transportParams,
     std::vector<std::map<std::string, std::string>> operatorParams,
     std::string configFile)
  : DataManager(),
    pimpl(std::unique_ptr<impl>(new impl(
      *this, &mpiComm, move(engineType), move(engineParams),
      move(transportParams), move(operatorParams), move(configFile))))
{
}
#endif

Adios2StMan::Adios2StMan(std::string engineType,
    std::map<std::string, std::string> engineParams,
    std::vector<std::map<std::string, std::string>> transportParams,
    std::vector<std::map<std::string, std::string>> operatorParams
)
    : Adios2StMan(move(engineType), move(engineParams), move(transportParams), move(operatorParams), {})
{
}

Adios2StMan::Adios2StMan(std::string configFile, from_config_t)
    : Adios2StMan({}, {}, {}, {}, move(configFile))
{
}

Adios2StMan::Adios2StMan(std::string engineType,
     std::map<std::string, std::string> engineParams,
     std::vector<std::map<std::string, std::string>> transportParams,
     std::vector<std::map<std::string, std::string>> operatorParams,
     std::string configFile)
  : DataManager(),
    pimpl(std::unique_ptr<impl>(new impl(
      *this, nullptr, move(engineType), move(engineParams),
      move(transportParams), move(operatorParams), move(configFile))))
{
}

Adios2StMan::~Adios2StMan() = default;

DataManager *Adios2StMan::clone() const
{
    return pimpl->clone();
}

String Adios2StMan::dataManagerType() const
{
    return pimpl->dataManagerType();
}

String Adios2StMan::dataManagerName() const
{
    return pimpl->dataManagerName();
}

void Adios2StMan::create64(rownr_t aNrRows)
{
    pimpl->create64(aNrRows);
}

rownr_t Adios2StMan::open64(rownr_t aRowNr, AipsIO &ios)
{
    return pimpl->open64(aRowNr, ios);
}

rownr_t Adios2StMan::resync64(rownr_t aRowNr)
{
    return pimpl->resync64(aRowNr);
}

Bool Adios2StMan::flush(AipsIO &ios, Bool doFsync)
{
    return pimpl->flush(ios, doFsync);
}

DataManagerColumn *Adios2StMan::makeScalarColumn(
        const String &aName, int aDataType, const String &aDataTypeID)
{
    return pimpl->makeScalarColumn(aName, aDataType, aDataTypeID);
}

DataManagerColumn *Adios2StMan::makeDirArrColumn(
        const String &aName, int aDataType, const String &aDataTypeID)
{
    return pimpl->makeDirArrColumn(aName, aDataType, aDataTypeID);
}

DataManagerColumn *Adios2StMan::makeIndArrColumn(
        const String &aName, int aDataType, const String &aDataTypeID)
{
    return pimpl->makeIndArrColumn(aName, aDataType, aDataTypeID);
}

void Adios2StMan::deleteManager()
{
    pimpl->deleteManager();
}

void Adios2StMan::addRow64(rownr_t aNrRows)
{
    return pimpl->addRow64(aNrRows);
}

DataManager *Adios2StMan::makeObject(
        const String &aDataManType, const Record &spec)
{
    return impl::makeObject(aDataManType, spec);
}

Record Adios2StMan::dataManagerSpec() const
{
    return pimpl->dataManagerSpec();
}

rownr_t Adios2StMan::getNrRows()
{
    return pimpl->getNrRows();
}



//
// impl class implementation using ADIOS2
//

#ifdef HAVE_MPI
void Adios2StMan::impl::checkMPI() const
{
    int mpi_finalized;
    MPI_Finalized(&mpi_finalized);
    if(mpi_finalized)
    {
        throw(std::runtime_error("MPI has been finalized when initializing Adios2StMan"));
    }
    else
    {
        int mpi_initialized;
        MPI_Initialized(&mpi_initialized);
        if(!mpi_initialized)
        {
#ifdef USE_THREADS
            int provided;
            MPI_Init_thread(0,0,MPI_THREAD_MULTIPLE, &provided);
            if(provided != MPI_THREAD_MULTIPLE)
            {
                throw(std::runtime_error(
                            "Casacore is built with thread and MPI enabled, \
                            but the MPI installation does not support threads"));
            }
#else
            MPI_Init(0,0);
#endif
        }
    }
}
#endif // HAVE_MPI


Adios2StMan::impl::impl(
        Adios2StMan &parent,
        void *mpiComm,
        std::string engineType,
        std::map<std::string, std::string> engineParams,
        std::vector<std::map<std::string, std::string>> transportParams,
        std::vector<std::map<std::string, std::string>> operatorParams,
        std::string configFile)
    : parent(parent),
      itsAdiosEngineType(std::move(engineType)),
      itsAdiosEngineParams(std::move(engineParams)),
      itsAdiosTransportParamsVec(std::move(transportParams)),
      itsAdiosOperatorParamsVec(std::move(operatorParams)),
      itsAdiosConfigFile(std::move(configFile))
{
    auto configureWithFile = !itsAdiosConfigFile.empty();
#ifdef HAVE_MPI
    if (mpiComm) {
        itsMpiComm = *reinterpret_cast<MPI_Comm *>(mpiComm);
        checkMPI();
        if (configureWithFile) {
            itsAdios = std::make_shared<adios2::ADIOS>(itsAdiosConfigFile, itsMpiComm);
        }
        else {
            itsAdios = std::make_shared<adios2::ADIOS>(itsMpiComm);
        }
    }
    else
#endif // HAVE_MPI
    {
        // Using an explicit check here instead of an assert avoids a warning
        // in release builds due to mpiComm being unused
        if (mpiComm != nullptr) {
            throw std::invalid_argument("mpiComm should be null");
        }
        if (configureWithFile) {
            itsAdios = std::make_shared<adios2::ADIOS>(itsAdiosConfigFile);
        }
        else {
            itsAdios = std::make_shared<adios2::ADIOS>();
        }
    }
    itsAdiosIO = std::make_shared<adios2::IO>(itsAdios->DeclareIO("Adios2StMan"));
    if (!configureWithFile) {
        configureAdios();
    }
}

void Adios2StMan::impl::configureAdios()
{
    if (itsAdiosEngineType.empty() == false)
    {
        itsAdiosIO->SetEngine(itsAdiosEngineType);
    }
    if (itsAdiosEngineParams.empty() == false)
    {
        itsAdiosIO->SetParameters(itsAdiosEngineParams);
    }

    // transport is valid only when it has a valid ADIOS2 transport name
    // invalid transport name may cause an exception
    for (const auto &param: itsAdiosTransportParamsVec)
    {
        auto itName = param.find("Name");
        if(itName==param.end()) continue;
        itsAdiosIO->AddTransport(itName->second, param);
    }

    // auto param intended as it should not modify itsAdiosOperatorParamsVec
    for(auto param : itsAdiosOperatorParamsVec)
    {
        std::string var,op;

        auto itVar = param.find("Variable");
        if(itVar==param.end())  continue;
        else  var = itVar->second;

        auto itOp = param.find("Operator");
        if(itOp==param.end())  continue;
        else  op=itOp->second;

        param.erase(itVar);
        param.erase(itOp);

        itsAdiosIO->AddOperation(var, op, param);
    }
}

Adios2StMan::impl::~impl()
{
    if (itsAdiosEngine)
    {
        itsAdiosEngine->EndStep();
        itsAdiosEngine->Close();
    }
    for (uInt i = 0; i < ncolumn(); ++i)
    {
        delete itsColumnPtrBlk[i];
    }
}

static adios2::Params to_adios2_params(const Record &record)
{
    adios2::Params params;
    for (Int i = 0; i != Int(record.size()); i++)
    {
        params[record.name(i)] = record.asString(i);
    }
    return params;
}

static Record to_record(const adios2::Params &params)
{
    Record record;
    for (auto &kv : params)
    {
        record.define(kv.first, kv.second);
    }
    return record;
}

DataManager *Adios2StMan::impl::makeObject(const String &/*aDataManType*/,
                                     const Record &spec)
{
    std::string configFile;
    std::string engine;
    adios2::Params engine_params;
    std::vector<adios2::Params> transport_params;
    std::vector<adios2::Params> operator_params;
    if (spec.isDefined(SPEC_FIELD_XML_FILE)) {
        configFile = spec.asString(SPEC_FIELD_XML_FILE);
    }
    if (spec.isDefined(SPEC_FIELD_ENGINE_TYPE)) {
        engine = spec.asString(SPEC_FIELD_ENGINE_TYPE);
    }
    if (spec.isDefined(SPEC_FIELD_ENGINE_PARAMS)) {
        engine_params = to_adios2_params(spec.asRecord(SPEC_FIELD_ENGINE_PARAMS));
    }
    if (spec.isDefined(SPEC_FIELD_TRANSPORT_PARAMS)) {
        auto &record = spec.asRecord(SPEC_FIELD_TRANSPORT_PARAMS);
        for (Int i = 0; i != Int(record.size()); i++) {
            auto name = record.name(i);
            auto params = to_adios2_params(record.asRecord(i));
            params["Name"] = name;
            transport_params.emplace_back(std::move(params));
        }
    }
    if (spec.isDefined(SPEC_FIELD_OPERATOR_PARAMS)) {
        auto &record = spec.asRecord(SPEC_FIELD_OPERATOR_PARAMS);
        for (Int i = 0; i != Int(record.size()); i++) {
            auto variable = record.name(i);
            auto params = to_adios2_params(record.asRecord(i));
            params["Variable"] = variable;
            operator_params.emplace_back(std::move(params));
        }
    }
    return new Adios2StMan(
#ifdef HAVE_MPI
            itsMpiComm,
#endif
            engine, engine_params,
            transport_params, operator_params, configFile);
}

Record Adios2StMan::impl::dataManagerSpec() const
{
    Record record;
    if (!itsAdiosConfigFile.empty()) {
        record.define(SPEC_FIELD_XML_FILE, itsAdiosConfigFile);
    }
    if (!itsAdiosEngineType.empty()) {
        record.define(SPEC_FIELD_ENGINE_TYPE, itsAdiosEngineType);
    }
    if (!itsAdiosEngineParams.empty()) {
        record.defineRecord(SPEC_FIELD_ENGINE_PARAMS, to_record(itsAdiosEngineParams));
    }
    if (!itsAdiosTransportParamsVec.empty()) {
        Record transport_params_record;
        for (const auto &params : itsAdiosTransportParamsVec) {
            auto itName = params.find("Name");
            if (itName == params.end()) {
                continue;
            }
            transport_params_record.defineRecord(itName->second, to_record(params));
        }
        record.defineRecord(SPEC_FIELD_TRANSPORT_PARAMS, transport_params_record);
    }
    if (!itsAdiosOperatorParamsVec.empty()) {
        Record operator_params_record;
        for (const auto &params : itsAdiosOperatorParamsVec) {
            auto itVar = params.find("Variable");
            if (itVar == params.end()) {
                continue;
            }
            auto itOper = params.find("Operator");
            if (itOper == params.end()) {
                continue;
            }
            operator_params_record.defineRecord(itVar->second, to_record(params));
        }
        record.defineRecord(SPEC_FIELD_OPERATOR_PARAMS, operator_params_record);
    }
    return record;
}

DataManager *Adios2StMan::impl::clone() const
{
    return new Adios2StMan(
#ifdef HAVE_MPI
        itsMpiComm,
#endif
        itsAdiosEngineType,
        itsAdiosEngineParams,
        itsAdiosTransportParamsVec,
        itsAdiosOperatorParamsVec,
        itsAdiosConfigFile
    );
}

String Adios2StMan::impl::dataManagerType() const
{
    return DATA_MANAGER_TYPE;
}

void Adios2StMan::impl::addRow64(rownr_t aNrRows)
{
    itsRows += aNrRows;
}

void Adios2StMan::impl::create64(rownr_t  aNrRows)
{
    itsRows = aNrRows;
    itsAdiosEngine = std::make_shared<adios2::Engine>(
        itsAdiosIO->Open(fileName() + ".bp", adios2::Mode::Write));
    itsAdiosEngine->BeginStep();
    for (uInt i = 0; i < ncolumn(); ++i)
    {
        itsColumnPtrBlk[i]->create(itsAdiosEngine, 'w');
    }
}

rownr_t Adios2StMan::impl::open64(rownr_t aNrRows, AipsIO &ios)
{
    itsRows = aNrRows;
    itsAdiosEngine = std::make_shared<adios2::Engine>(
        itsAdiosIO->Open(fileName() + ".bp", adios2::Mode::Read));
    itsAdiosEngine->BeginStep();
    for (uInt i = 0; i < ncolumn(); ++i)
    {
        itsColumnPtrBlk[i]->create(itsAdiosEngine, 'r');
    }

    ios.getstart(DATA_MANAGER_TYPE);
    ios >> itsDataManName;
    {
      // see comment on flush()
      int dummy;
      ios >> dummy;
    }
    ios.getend();
    itsRows = aNrRows;
    return itsRows;
}

void Adios2StMan::impl::deleteManager() {}

DataManagerColumn *Adios2StMan::impl::makeScalarColumn(const String &name,
                                                 int aDataType,
                                                 const String &dataTypeId)
{
    return makeColumnCommon(name, aDataType, dataTypeId);
}

DataManagerColumn *Adios2StMan::impl::makeDirArrColumn(const String &name,
                                                 int aDataType,
                                                 const String &dataTypeId)
{
    return makeColumnCommon(name, aDataType, dataTypeId);
}

DataManagerColumn *Adios2StMan::impl::makeIndArrColumn(const String &name,
                                                 int aDataType,
                                                 const String &dataTypeId)
{
    return makeColumnCommon(name, aDataType, dataTypeId);
}

DataManagerColumn *Adios2StMan::impl::makeColumnCommon(const String &name,
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
        case TpInt64:
        case TpArrayInt64:
            aColumn = new Adios2StManColumnT<Int64>(this, aDataType, name, itsAdiosIO);
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
            aColumn = new Adios2StManColumnString(this, aDataType, name, itsAdiosIO);
            break;
        default:
            throw (DataManInvDT (name));
    }
    itsColumnPtrBlk[ncolumn()] = aColumn;
    return aColumn;
}

rownr_t Adios2StMan::impl::getNrRows() { return itsRows; }

rownr_t Adios2StMan::impl::resync64(rownr_t /*aNrRows*/) { return itsRows; }

Bool Adios2StMan::impl::flush(AipsIO &ios, Bool /*doFsync*/)
{
    ios.putstart(DATA_MANAGER_TYPE, 2);
    ios << itsDataManName;
    // Here we used to write itsStManColumnType (int), but that was an otherwise
    // unused member, so we are writing a dummy 0 instead to preserve backwards
    // compatibility
    ios << 0;
    ios.putend();
    return true;
}

String Adios2StMan::impl::dataManagerName() const { return itsDataManName; }

void register_adios2stman()
{
    DataManager::registerCtor("Adios2StMan", Adios2StMan::makeObject);
}
}
