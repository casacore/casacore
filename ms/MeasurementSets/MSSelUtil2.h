#ifndef MS_MSSELUTIL2_H
#define MS_MSSELUTIL2_H

#include <casa/aips.h>
#include <casa/Arrays/Matrix.h>
#include <casa/BasicSL/Complex.h>

namespace casa { //# NAMESPACE CASA - BEGIN
template <class T> class MSSelUtil2
{
  public:
  // reorder data from 3d (corr,chan,row) to 4d (corr,chan,ifr,time)
  static void reorderData(Array<T>& data,
                          const Vector<Int>& ifrSlot,
                          Int nIfr, 
                          const Vector<Int>& timeSlot, 
                          Int nTime,
                          const T& defvalue);

  // reorder data from 4d (corr,chan,ifr,time) to 3d (corr,chan,row) 
  static void reorderData(Array<T>& data, 
                          const Matrix<Int>& rowIndex,
                          Int nRow);

  // average data (with flags & weights applied) over it's last axis (time or
  // row), return in data (overwritten), dataFlag gives new flags.
  static void timeAverage(Array<Bool>& dataFlag, Array<T>& data, 
                          const Array<Bool>& flag, const Array<Float>& weight);

};
} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <ms/MeasurementSets/MSSelUtil2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
