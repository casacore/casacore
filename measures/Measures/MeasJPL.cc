// # MeasJPL.cc: Interface to JPL DE tables
// # Copyright (C) 1996,1997,1998,1999,2001,2002,2016
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

// # Includes
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/measures/Measures/MeasJPL.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/MVEpoch.h>
#include <casacore/measures/Measures/MeasIERS.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/System/Aipsrc.h>
#include <casacore/tables/Tables/TableDesc.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

// # Constants

// # Member functions
bool MeasJPL::get(Vector<double> &returnValue, MeasJPL::Files file, MeasJPL::Types type,
                  const MVEpoch &date) {
  returnValue = 0.0;
  // Open the file if needed.
  if (!initMeasOnce(file)) {
    return false;
  }
  // Get or read the correct data if needed.
  // Note that fillMeas uses locks to be thread-safe. The pointer returned
  // will never change, even if fillMeas has to extend the buffer.
  double intv;
  const double *dta = fillMeas(intv, file, date);
  if (!dta) {
    return false;
  }
  double res[6];
  double res1[6];
  for (unsigned int i = 0; i < 6; i++) res[i] = 0.0;
  // Interpolation fraction
  bool mulfr = true;
  if (type == MeasJPL::BARYSOLAR) {
    res[0] = 0.0;
  } else if (type == MeasJPL::BARYEARTH) {
    interMeas(res, file, intv, dmjd[file], idx[file][1][MeasJPL::EARTH - 1], 3,
              idx[file][2][MeasJPL::EARTH - 1], dta + idx[file][0][MeasJPL::EARTH - 1]);
  } else if (type == MeasJPL::EARTH || type == MeasJPL::MOON) {
    interMeas(res1, file, intv, dmjd[file], idx[file][1][MeasJPL::MOON - 1], 3,
              idx[file][2][MeasJPL::MOON - 1], dta + idx[file][0][MeasJPL::MOON - 1]);
    interMeas(res, file, intv, dmjd[file], idx[file][1][MeasJPL::EARTH - 1], 3,
              idx[file][2][MeasJPL::EARTH - 1], dta + idx[file][0][MeasJPL::EARTH - 1]);
    if (type == MeasJPL::EARTH) {
      for (unsigned int i = 0; i < 6; i++) res[i] -= res1[i] / emrat[file];
    } else {
      for (unsigned int i = 0; i < 6; i++) res[i] += res1[i];
    }
  } else if (type == MeasJPL::NUTATION) {
    if (idx[file][1][MeasJPL::BARYSOLAR - 1] == 0) return false;
    interMeas(res, file, intv, dmjd[file], idx[file][1][MeasJPL::BARYSOLAR - 1], 2,
              idx[file][2][MeasJPL::BARYSOLAR - 1], dta + idx[file][0][MeasJPL::BARYSOLAR - 1]);
    mulfr = false;
  } else if (type == MeasJPL::LIBRATION) {
    if (idx[file][1][MeasJPL::BARYEARTH - 1] == 0) return false;
    interMeas(res, file, intv, dmjd[file], idx[file][1][MeasJPL::BARYEARTH - 1], 3,
              idx[file][2][MeasJPL::BARYEARTH - 1], dta + idx[file][0][MeasJPL::BARYEARTH - 1]);
    mulfr = false;
  } else {
    interMeas(res, file, intv, dmjd[file], idx[file][1][type - 1], 3, idx[file][2][type - 1],
              dta + idx[file][0][type - 1]);
  }
  if (mulfr) {
    for (unsigned int i = 0; i < 6; i++) returnValue(i) = res[i] * aufac[file];
  } else {
    for (unsigned int i = 0; i < 6; i++) returnValue(i) = res[i];
  }

  return true;
}

bool MeasJPL::getConst(double &res, MeasJPL::Files which, MeasJPL::Codes what) {
  if (initMeasOnce(which)) {
    res = cn[which][what];
    return true;
  }
  return false;
}

bool MeasJPL::getConst(double &res, MeasJPL::Files which, const String &nam) {
  if (initMeasOnce(which)) {
    const TableRecord &tr = t[which].keywordSet();
    if (tr.isDefined(nam)) {
      res = tr.asDouble(nam);
      return true;
    }
  }
  return false;
}

bool MeasJPL::initMeasOnce(MeasJPL::Files which) {
  try {
    std::call_once(theirCallOnceFlags[which], doInitMeas, which);
  } catch (InitError &) {
    return false;
  }
  return true;
}

void MeasJPL::doInitMeas(MeasJPL::Files which) {
  static const String names[MeasJPL::N_Columns] = {"MJD", "x"};
  static const String tplc[N_Files] = {"measures.DE200.directory", "measures.DE405.directory"};

  TableRecord kws;
  TableRow row;
  RORecordFieldPtr<double> rfp[MeasJPL::N_Types];
  double dt;
  String vs;
  bool ok = true;
  if (!MeasIERS::getTable(MeasJPL::t[which], kws, row, rfp, vs, dt, 1, names, tp[which],
                          tplc[which], "ephemerides")) {
    ok = false;
  }
  if (ok) {
    MeasIERS::openNote(&MeasJPL::closeMeas);
    if (!kws.isDefined("MJD0") || kws.asDouble("MJD0") < 10000 || !kws.isDefined("dMJD") ||
        kws.asDouble("dMJD") < 8 || !kws.isDefined("AU") || kws.asDouble("AU") < 1e8 ||
        !kws.isDefined("CLIGHT") || kws.asDouble("CLIGHT") < 2e5 || !kws.isDefined("GMS") ||
        kws.asDouble("GMS") < 2e-4 ||
        !((kws.isDefined("RADS") && kws.asDouble("RADS") > 6e5) ||
          (kws.isDefined("ASUN") && kws.asDouble("ASUN") > 6e5)) ||
        !kws.isDefined("EMRAT") || kws.asDouble("EMRAT") < 10) {
      ok = false;
    }
  }
  if (ok) {
    mjd0[which] = int(kws.asDouble("MJD0"));
    dmjd[which] = int(kws.asDouble("dMJD"));
    cn[which][MeasJPL::AU] = kws.asDouble("AU");
    aufac[which] = 1. / cn[which][MeasJPL::AU];
    emrat[which] = 1. + kws.asDouble("EMRAT");
    cn[which][MeasJPL::CAU] = 86400 * kws.asDouble("CLIGHT") / cn[which][MeasJPL::AU];
    if (kws.isDefined("RADS")) {
      cn[which][MeasJPL::RADS] = kws.asDouble("RADS") / cn[which][MeasJPL::AU];
    } else {
      cn[which][MeasJPL::RADS] = kws.asDouble("ASUN") / cn[which][MeasJPL::AU];
    }
    cn[which][MeasJPL::GMS] =
        kws.asDouble("GMS") / cn[which][MeasJPL::CAU] / cn[which][MeasJPL::CAU];
    int n = t[which].nrow();
    row.get(n - 1);
    if (*(rfp[0]) != mjd0[which] + n * dmjd[which]) {
      ok = false;
    } else {
      mjdl[which] = mjd0[which] + n * dmjd[which];
    }
  }
  if (ok) {
    const TableRecord &tr = t[which].tableDesc().columnDesc("x").keywordSet();
    if (tr.asInt("Rows") != 3 || tr.asInt("Columns") != 13) {
      ok = false;
    } else {
      Array<int> xx = tr.asArrayInt("Description");
      unsigned int k = 0;
      for (unsigned int i = 0; i < 3; i++) {
        for (unsigned int j = 0; j < 13; j++) {
          idx[which][i][j] = xx(IPosition(1, k++));
          if (i == 0) idx[which][i][j] -= 3;
        }
      }
      acc[int(which)].attach(t[which], "x");
    }
  }
  if (!ok) {
    // Close table if open.
    t[which] = Table();
    LogIO os(LogOrigin("MeasJPL", "initMeas(MeasJPL::Files)", WHERE));
    os << "Corrupted JPL table " + tp[which] << LogIO::EXCEPTION;
  }

  if (t[which].isNull()) {
    throw InitError();
  }
}

void MeasJPL::closeMeas() {
  // Cannot get this fast & thread-safe without rewriting initMeas/closeMeas.
  // But this is only used to check for memory leaks at the end and possibly
  // to compare tables in tests, don't bother. Apply pray and HACK below...
  for (unsigned int i = 0; i < N_Files; ++i) {
    if (!t[i].isNull()) {
      mjd0[i] = 0;
      mjdl[i] = 0;
      dmjd[i] = 0;
      curDate[i].resize(0);
      dval[i].resize(0);
      t[i] = Table();
    }
#if defined(USE_THREADS)
    std::atomic_thread_fence(std::memory_order_release);  // pray
#endif
    new (&theirCallOnceFlags[i]) std::once_flag;  // HACK
  }
}

const double *MeasJPL::fillMeas(double &intv, MeasJPL::Files which, const MVEpoch &utf) {
  // Get UT day and check if within range.
  int ut = int(utf.getDay());
  if (ut < mjd0[which] + dmjd[which] || ut >= mjdl[which] + dmjd[which]) {
    return nullptr;
  }
  // Turn day into interval (intervals are dmjd wide) plus fraction
  ut = (ut - mjd0[which]) / dmjd[which];
  intv = ((utf.getDay() - (ut * dmjd[which] + mjd0[which])) + utf.getDayFraction()) / dmjd[which];
  // If needed, read the data of this interval.
  std::lock_guard<std::mutex> locker(theirMutex);
  for (size_t i = 0; i < curDate[which].size(); ++i) {
    if (ut == curDate[which][i]) {
      return dval[which][i].data();
    }
  }
  // Read the data for this date and add to the buffers.
  Array<double> data(acc[int(which)](ut - 1));
  dval[which].push_back(data);
  curDate[which].push_back(ut);
  return data.data();
}

void MeasJPL::interMeas(double res[], MeasJPL::Files, double intv, double ivf, int ncf, int ncm,
                        int na, const double buf[]) {
  double tc = 2.0 * (fmod(double(na) * intv, double(1.0)) + int(intv)) - 1.0;
  int l = int(double(na) * intv - int(intv));
  // Chebyshev coefficients
  double chc[18];
  chc[0] = 1;
  chc[1] = tc;
  double twot = 2 * tc;
  for (int i = 2; i < ncf; i++) {
    chc[i] = twot * chc[i - 1] - chc[i - 2];
  }
  double vfac = (2.0 * na) / ivf;
  double chcv[18];
  chcv[0] = 0;
  chcv[1] = 1;
  chcv[2] = 2.0 * twot;
  for (int i = 3; i < ncf; i++) {
    chcv[i] = twot * chcv[i - 1] + 2.0 * chc[i - 1] - chcv[i - 2];
  }
  {  // Position
    for (int i = 0; i < ncm; i++) {
      res[i] = 0;
      for (int j = ncf - 1; j >= 0; j--) {
        res[i] += chc[j] * buf[(l * ncm + i) * ncf + j];
      }
    }
  }
  {  // Velocity
    for (int i = 0; i < ncm; i++) {
      res[i + ncm] = 0;
      for (int j = ncf - 1; j > 0; j--) {
        res[i + ncm] += chcv[j] * buf[(l * ncm + i) * ncf + j];
      }
      res[i + ncm] *= vfac;
    }
  }
}

std::once_flag MeasJPL::theirCallOnceFlags[MeasJPL::N_Files];
std::mutex MeasJPL::theirMutex;
Table MeasJPL::t[MeasJPL::N_Files];
ArrayColumn<double> MeasJPL::acc[MeasJPL::N_Files];
int MeasJPL::mjd0[MeasJPL::N_Files] = {0, 0};
int MeasJPL::mjdl[MeasJPL::N_Files] = {0, 0};
int MeasJPL::dmjd[MeasJPL::N_Files] = {0, 0};
const String MeasJPL::tp[MeasJPL::N_Files] = {"DE200", "DE405"};
int MeasJPL::idx[MeasJPL::N_Files][3][13];
vector<int> MeasJPL::curDate[MeasJPL::N_Files];
vector<Vector<double>> MeasJPL::dval[MeasJPL::N_Files];
double MeasJPL::aufac[MeasJPL::N_Files];
double MeasJPL::emrat[MeasJPL::N_Files];
double MeasJPL::cn[MeasJPL::N_Files][MeasJPL::N_Codes];

}  // namespace casacore
