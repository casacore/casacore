//# tIAU2000App.cc: Regression test for J2000 -> APP against SOFA/ERFA

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/System/AipsrcValue.h>
#include <casacore/casa/Quanta/MVDirection.h>
#include <casacore/casa/Quanta/MVPosition.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <cmath>

#ifdef CASACORE_HAVE_SOFA
#include <sofa.h>
#define SOFA_OR_ERFA(erfa_fn, sofa_fn) sofa_fn
#elif defined(CASACORE_HAVE_ERFA)
#include <erfa.h>
#define SOFA_OR_ERFA(erfa_fn, sofa_fn) erfa_fn
#else
#error "tIAU2000App requires SOFA or ERFA"
#endif

#include <casacore/casa/iostream.h>

using namespace casacore;

namespace {

constexpr Double RAD_TO_MAS = 180.0 / M_PI * 3600.0 * 1000.0;

Double angularSepMas(const Double a[3], const Double b[3]) {
  const Double cross[3] = {
    a[1] * b[2] - a[2] * b[1],
    a[2] * b[0] - a[0] * b[2],
    a[0] * b[1] - a[1] * b[0]
  };
  const Double sinang = std::sqrt(cross[0] * cross[0] +
                                  cross[1] * cross[1] +
                                  cross[2] * cross[2]);
  const Double cosang = a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
  return std::atan2(sinang, cosang) * RAD_TO_MAS;
}

void matVec(const Double m[3][3], const Double x[3], Double y[3]) {
  for (Int i = 0; i < 3; ++i) {
    y[i] = m[i][0] * x[0] + m[i][1] * x[1] + m[i][2] * x[2];
  }
}

void enableIAU2000A() {
  (void)MeasTable::useIAU2000();
  (void)MeasTable::useIAU2000A();
  AipsrcValue<Bool>::set(
    AipsrcValue<Bool>::registerRC("measures.iau2000.b_use", False), True);
  AipsrcValue<Bool>::set(
    AipsrcValue<Bool>::registerRC("measures.iau2000.b_use2000a", False), True);
}

void disableIAU2000A() {
  AipsrcValue<Bool>::set(
    AipsrcValue<Bool>::registerRC("measures.iau2000.b_use", False), False);
  AipsrcValue<Bool>::set(
    AipsrcValue<Bool>::registerRC("measures.iau2000.b_use2000a", False), False);
}

void checkEpoch(const char* label, const Double mjdUtc) {
  const Double obsLon = -1.8782832;
  const Double obsLat = 0.5953703;
  const Double obsHeight = 2124.0;
  const Double dirLon = 1.0;
  const Double dirLat = 0.5;

  const MDirection dirJ2000(MVDirection(dirLon, dirLat), MDirection::J2000);
  const MPosition pos(MVPosition(Quantity(obsHeight, "m"),
                                 Quantity(obsLon, "rad"),
                                 Quantity(obsLat, "rad")),
                      MPosition::WGS84);
  const MEpoch epoch(MVEpoch(Quantity(mjdUtc, "d")), MEpoch::UTC);

  enableIAU2000A();
  const MeasFrame frame(epoch, pos);
  const MVDirection casaApp =
    MDirection::Convert(dirJ2000, MDirection::Ref(MDirection::APP, frame))()
      .getValue();
  disableIAU2000A();

  const Double tt1 = 2400000.5;
  const Double tt2 = mjdUtc + ((mjdUtc < 53736.0) ? 64.184 : 69.184) / 86400.0;

  Double bpn00a[3][3];
  SOFA_OR_ERFA(eraPnm00a, iauPnm00a)(tt1, tt2, bpn00a);

  const Double p[3] = {
    std::cos(dirLat) * std::cos(dirLon),
    std::cos(dirLat) * std::sin(dirLon),
    std::sin(dirLat)
  };
  Double pTrue[3];
  matVec(bpn00a, p, pTrue);

  Double pvh[2][3], pvb[2][3];
  SOFA_OR_ERFA(eraEpv00, iauEpv00)(tt1, tt2, pvh, pvb);
  const Double cAuPerDay = 173.14463267424034;
  const Double v[3] = {
    pvb[1][0] / cAuPerDay,
    pvb[1][1] / cAuPerDay,
    pvb[1][2] / cAuPerDay
  };
  Double vTrue[3];
  matVec(bpn00a, v, vTrue);

  const Double s = std::sqrt(pvh[0][0] * pvh[0][0] +
                             pvh[0][1] * pvh[0][1] +
                             pvh[0][2] * pvh[0][2]);
  const Double v2 = v[0] * v[0] + v[1] * v[1] + v[2] * v[2];
  const Double bm1 = std::sqrt(1.0 - v2);

  Double pApp[3];
  SOFA_OR_ERFA(eraAb, iauAb)(pTrue, vTrue, s, bm1, pApp);

  const Double casaVec[3] = {
    std::cos(casaApp.getLat()) * std::cos(casaApp.getLong()),
    std::cos(casaApp.getLat()) * std::sin(casaApp.getLong()),
    std::sin(casaApp.getLat())
  };
  const Double sepMas = angularSepMas(casaVec, pApp);

  cout << label << " separation to SOFA/ERFA reference: "
       << sepMas << " mas" << endl;

  // casacore still differs slightly because it uses full solar deflection and
  // a different Earth-velocity series. The frame-model discrepancy should be
  // comfortably larger than this tolerance.
  AlwaysAssert(sepMas < 5.0, AipsError);
}

}  // namespace

int main() {
  try {
    checkEpoch("J2000.0", 51544.5);
    checkEpoch("2020-01-01", 58849.0);
    cout << "OK" << endl;
    return 0;
  } catch (const std::exception& x) {
    cout << x.what() << endl;
    cout << "FAIL" << endl;
    return 1;
  }
}
