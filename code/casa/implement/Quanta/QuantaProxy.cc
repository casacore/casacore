#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/MVTime.h>
#include <casa/Quanta/MVFrequency.h>
#include <casa/Quanta/MVDoppler.h>
#include <casa/Utilities/MUString.h>
#include <casa/Quanta.h>
#include <casa/Arrays/Vector.h>
#include <casa/BasicSL/Complex.h>
#include <casa/Quanta/QLogical.h>
#include <casa/Quanta/QuantumHolder.h>
#include <casa/BasicSL/Constants.h>
#include <casa/Logging.h>
#include <casa/Exceptions/Error.h>
#include <casa/sstream.h>

#include <casa/Quanta/QuantaProxy.h>

#include <casa/namespace.h>
void QuantaProxy::mapInsert(Record &out, const String &type,
                            const map<String, UnitName> &mp)
{
  ostringstream osn;
  osn <<  mp.size();
  out.define(String("== ") + type + String(" =="),
          String("== ") + type + String(" ==== ") + String(osn)
+ String("===="));
  for (map<String, UnitName>::const_iterator i=mp.begin(); i != mp.end(); ++i) {
    ostringstream oss;
    oss << i->second;
    String str = oss.str();
    out.define(type + "_" + i->first, str);
  }
}

void QuantaProxy::fits()
{
  UnitMap::addFITS();
}

Record QuantaProxy::mapit(const String &tp)
{
  const uInt N = 5;
  Record tmp;
  String str;
  static String types[N] = { "all", "Prefix", "SI", "Custom", "User"};
  uInt p = MUString::minimaxNC(tp, N, types);
  if (p >= N) {
    tmp.define(String("Error"), String("Unknown mapping requested"));
  } else {
    if (p == 0 || p == 1)
      mapInsert(tmp, types[1], UnitMap::givePref());
    if (p == 0 || p == 2)
      mapInsert(tmp, types[2], UnitMap::giveSI());
    if (p == 0 || p == 3)
      mapInsert(tmp, types[3], UnitMap::giveCust());
    if (p == 0 || p == 4)
      mapInsert(tmp, types[4], UnitMap::giveUser());
  };
  return tmp;
}

// Get a constant
Record QuantaProxy::constants(const String &in) {
  const uInt N = 20;
  String str;
  static String types[N] = {
    "pi", "ee", "c", "G", "h", "HI", "R", "NA", "e", "mp",
    "mp_me", "mu0", "epsilon0", "k", "F", "me", "re", "a0",
    "R0", "k2"
  };
  static Quantity res[N] = {
    Quantity(C::pi,""), Quantity(C::e,""),
    QC::c, QC::G, QC::h, QC::HI, QC::R, QC::NA, QC::e, QC::mp,
    QC::mp_me, QC::mu0, QC::epsilon0, QC::k, QC::F, QC::me, QC::re, QC::a0,
    QC::R0, QC::k2
  };
  uInt p = MUString::minimaxNC(in, N, types);
  if (p >= N ) return quant2rec(Quantity(0.,""));
  return quant2rec(res[p]);
}

void QuantaProxy::define(const String& ustr, const Record& rec)
{
  QuantumHolder qh = rec2qh(rec);
  const Quantity& q = qh.asQuantity();
  UnitMap::putUser(ustr, UnitVal(q.getValue(), q.getUnit()), "User defined");
}


Record QuantaProxy::unit(const Vector<Double>& val, const String& ustr)
{
  if (val.nelements() == 1) {
    return quant2rec(Quantity(val[0], ustr));
  } else {
    return quant2rec(Quantum<Vector<Double> >(val, ustr));
  }
}


Record QuantaProxy::qfunc1(const Record& rec, Int form) {
  QuantumHolder qh = rec2qh(rec);
  Record out;
  switch (form) {
    // sin
  case 0:
    if (qh.isQuantity())
      return quant2rec(sin(qh.asQuantity()));
    else
      return quant2rec(sin(qh.asQuantumVectorDouble()));
    break;
    // cos
  case 1:
    if (qh.isQuantity())
      return quant2rec(cos(qh.asQuantity()));
    else
      return quant2rec(cos(qh.asQuantumVectorDouble()));
    break;
    // tan
  case 2:
    if (qh.isQuantity())
      return quant2rec(tan(qh.asQuantity()));
    else
      return quant2rec(tan(qh.asQuantumVectorDouble()));
    break;
    // asin
  case 3:
    if (qh.isQuantity())
      return quant2rec(asin(qh.asQuantity()));
    else
      return quant2rec(asin(qh.asQuantumVectorDouble()));
    break;
    // acos
  case 4:
    if (qh.isQuantity())
      return quant2rec(acos(qh.asQuantity()));
    else
      return quant2rec(acos(qh.asQuantumVectorDouble()));
    break;
    // atan
  case 5:
    if (qh.isQuantity())
      return quant2rec(atan(qh.asQuantity()));
    else
      return quant2rec(atan(qh.asQuantumVectorDouble()));
    break;
    // abs
  case 6:
    if (qh.isQuantity())
      return quant2rec(abs(qh.asQuantity()));
    else
      return quant2rec(abs(qh.asQuantumVectorDouble()));
    break;
    // ceil
  case 7:
    if (qh.isQuantity())
      return quant2rec(ceil(qh.asQuantity()));
    else
      return quant2rec(ceil(qh.asQuantumVectorDouble()));
    break;
    // floor
  case 8:
    if (qh.isQuantity())
      return quant2rec(floor(qh.asQuantity()));
    else
      return quant2rec(floor(qh.asQuantumVectorDouble()));
    break;
    // canon
  case 9:
    if (qh.isQuantity())
      return quant2rec(qh.asQuantity().get());
    else
      return quant2rec(qh.asQuantumVectorDouble().get());
    break;
    // log
  case 10:
    if (qh.isQuantity())
      return quant2rec(log(qh.asQuantity()));
    else
      return quant2rec(log(qh.asQuantumVectorDouble()));
    break;
    // log10
  case 11:
    if (qh.isQuantity())
      return quant2rec(log10(qh.asQuantity()));
    else
      return quant2rec(log10(qh.asQuantumVectorDouble()));
    break;
    // exp
  case 12:
    if (qh.isQuantity())
      return quant2rec(exp(qh.asQuantity()));
    else
      return quant2rec(exp(qh.asQuantumVectorDouble()));
    break;
    // sqrt
  case 13:
    if (qh.isQuantity())
      return quant2rec(sqrt(qh.asQuantity()));
    else
      return quant2rec(sqrt(qh.asQuantumVectorDouble()));
    break;
  default:
    throw AipsError("Unknown one argument Quantity function");
    break;
  }
  return out;
}
Record QuantaProxy::qfunc2(const Record& lrec, const Record& rrec, Int form) {

  QuantumHolder l = rec2qh(lrec);
  QuantumHolder r = rec2qh(rrec);
  switch (form) {
    // atan2
  case 0:
    if (l.isQuantity())
      return quant2rec(atan2(l.asQuantity(),r.asQuantity()));
    else {
    return
quant2rec(atan2(l.asQuantumVectorDouble(),r.asQuantumVectorDouble()));
    }
    break;
    // mul
  case 1:
    if (l.isQuantity())
      return quant2rec(l.asQuantity() * r.asQuantity());
    else {
      return
          quant2rec(l.asQuantumVectorDouble() * r.asQuantumVectorDouble());
    }
    break;
    // div
  case 2:
    if (l.isQuantity())
      return quant2rec(l.asQuantity() / r.asQuantity());
    else {
      return
          quant2rec(l.asQuantumVectorDouble() / r.asQuantumVectorDouble());
    }
    break;
    // sub
  case 3:
    if (l.isQuantity())
      return quant2rec(l.asQuantity() - r.asQuantity());
    else {
      return
          quant2rec(l.asQuantumVectorDouble() - r.asQuantumVectorDouble());
    }
    break;
    // add
  case 4:
    if (l.isQuantity())
      return quant2rec(l.asQuantity() + r.asQuantity());
    else {
      return
          quant2rec(l.asQuantumVectorDouble() + r.asQuantumVectorDouble());
    }
    break;
    // convert
  case 5:
    if (l.isQuantity())
      return quant2rec(r.asQuantity().getUnit().empty() ?
          l.asQuantity().get() : l.asQuantity().get(r.asQuantity()));
    else {
      return quant2rec(r.asQuantumVectorDouble().getUnit().empty() ?
          l.asQuantumVectorDouble().get() :
l.asQuantumVectorDouble().get(r.asQuantumVectorDouble()));
    }
    break;
  default:
    throw AipsError("Unknown two argument Quantity function");
    break;
  }
}

Record QuantaProxy::norm(const Record& rec, Double ang)
{
  QuantumHolder qh = rec2qh(rec);
  const Quantity& val = qh.asQuantity();
  Quantity q = Quantity(MVAngle(val)(ang).degree(), "deg");
  return quant2rec(q);
}

Bool QuantaProxy::compare(const Record& lrec, const Record& rrec)
{
  QuantumHolder qhl = rec2qh(lrec);
  QuantumHolder qhr = rec2qh(rrec);
  const Quantity& left = qhl.asQuantity();
  const Quantity &right = qhr.asQuantity();
  return (left.getFullUnit().getValue() == right.getFullUnit().getValue());
}

Bool QuantaProxy::check(const String& str)
{
  Quantity res;
  if (Quantity::read(res, str)) {
    return  True;
  } else {
    return False;
  }
}

Record QuantaProxy::pow(const Record& rec, Int powerof)
{
  QuantumHolder qh = rec2qh(rec);
  if (qh.isQuantity()) {
    return quant2rec(casa::pow(qh.asQuantity(), powerof));
  }
  // this needs the template defined
  /*
  else {
    return quant2rec(casa::pow(qh.asQuantumVectorDouble(), powerof));
  }
  */
}

Record QuantaProxy::toTime(const Record& rec)
{
  QuantumHolder qh = rec2qh(rec);
  const Quantity& val = qh.asQuantity();
  Quantity q;
  if (val.check(UnitVal::TIME)) {
    q = val;
  } else {
    q = MVTime(val).get();
  }
  return quant2rec(q);
}

Record QuantaProxy::toAngle(const Record& rec)
{
  QuantumHolder qh = rec2qh(rec);
  const Quantity& val = qh.asQuantity();
  Quantity q;
  if (val.check(UnitVal::ANGLE)) {
    q = val;
  } else {
    q = MVAngle(val).get();
  }
  return quant2rec(q);
}

Record QuantaProxy::dopcv(const Record& lrec, const Record& rrec)
{
  QuantumHolder qhl = rec2qh(lrec);
  QuantumHolder qhr = rec2qh(rrec);
  const Quantity& left = qhl.asQuantity();
  const Quantity &right = qhr.asQuantity();
  return quant2rec(MVDoppler(left).get(right.getFullUnit()));
}

Record QuantaProxy::frqcv(const Record& lrec, const Record& rrec)
{
  QuantumHolder qhl = rec2qh(lrec);
  QuantumHolder qhr = rec2qh(rrec);
  const Quantity& left = qhl.asQuantity();
  const Quantity &right = qhr.asQuantity();
  return quant2rec(MVFrequency(left).get(right.getFullUnit()));
}

Record QuantaProxy::splitDate(const Record& rec)
{
  QuantumHolder qh = rec2qh(rec);
  const Quantity& val = qh.asQuantity();
  Record tmp;
  MVTime x(val);
  tmp.define("mjd", x.day());
  tmp.define("year", x.year());
  tmp.define("yearday", static_cast<Int>(x.yearday()));
  tmp.define("month",static_cast<Int>(x.month()));
  tmp.define("monthday",static_cast<Int>(x.monthday()));
  tmp.define("week",static_cast<Int>(x.yearweek()));
  tmp.define("weekday",static_cast<Int>(x.weekday()));
  Double y(fmod(x.day(), 1.0));
  tmp.define("hour",static_cast<Int>(y*24.0));
  y = fmod(y*24.0, 1.0);
  tmp.define("min",static_cast<Int>(y*60.0));
  y = fmod(y*60.0, 1.0);
  tmp.define("sec",static_cast<Int>(y*60.0));
  tmp.define("s",static_cast<Double>(y*60.0));
  y = fmod(y*60.0, 1.0);
  tmp.define("msec",static_cast<Int>(y*1000.0));
  tmp.define("usec",static_cast<Int>(y*1.0e6));
  return tmp;
}

Record QuantaProxy::qlogical(const Record& lrec, const Record& rrec, int form)
{
  QuantumHolder qhl = rec2qh(lrec);
  QuantumHolder qhr = rec2qh(rrec);
  const Quantity& left = qhl.asQuantity();
  const Quantity &right = qhr.asQuantity();
  Quantity q;
  switch (form) {
        // le
    case 0:
      q = left <= right;
      break;
        // lt
    case 1:
      q = left < right;
      break;
        // eq
    case 2:
      q = left == right;
      break;
        // ne
    case 3:
      q = left != right;
      break;
        // gt
    case 4:
      q = left > right;
      break;
        // ge
    case 5:
      q = left >= right;
      break;
    default:
      throw AipsError("Unknown Quantity comparison function");
      break;
  }
  return quant2rec(q);
}


Record QuantaProxy::quant(const Vector<String>& vstr)
{
  Record out;
  if (vstr.nelements() == 1) {
    QuantumHolder qh;
    String err;
    if ( !qh.fromString(err, vstr[0]) ) {
      throw(AipsError(err));
    }
    if ( !qh.toRecord(err, out) ) {
      throw(AipsError(err));
    }
    return out;
  } else {
    for (uInt i=0; i<vstr.nelements(); ++i) {
      QuantumHolder qh;
      String err;
      if ( !qh.fromString(err, vstr[i]) ) {
        throw(AipsError(err));
      }
      Record tmp;
      if ( !qh.toRecord(err, tmp) ) {
        throw(AipsError(err));
      }
      ostringstream oss;
      oss << "q" << i;
      out.defineRecord(String(oss), tmp);
    }
  }
  return out;
}

Vector<String> QuantaProxy::time(const Record& rec, const Vector<String>& fmt,
                    Int prec, Bool form2)
{
  QuantumHolder qh = rec2qh(rec);
  Vector<String> out;
  Int fm = makeForm(fmt, "time");
  Quantum<Vector<Double> > val =  qh.asQuantumVectorDouble();
  Int nelem = val.getValue().nelements();
  IPosition shp = val.getValue().shape();
  if (nelem > 0) {
    Int nrow = shp(shp.nelements()-1);
    Int ncol = nelem/nrow;
    out.resize(nrow);
    Int k = 0;
    for (Int i=0; i<nrow; i++) {
      ostringstream oss;
      if (ncol > 1 && form2) oss << '[';
      for (Int j=0; j<ncol; j++) {
        if (j>0) {
          if (form2) oss << ", ";
          else oss << " ";
        };
        oss << MVTime(Quantity(val.getValue()(k),
                      val.getFullUnit())).
            string(fm, prec);
        k++;
      };
      if (ncol > 1 && form2) oss << ']';
      out(i) = oss.str();
    }
  } else {
    out.resize(0);
  }
  return out;
}

Vector<String> QuantaProxy::angle(const Record& rec, const Vector<String>& fmt,
                                 Int prec, Bool form2)
{
  QuantumHolder qh = rec2qh(rec);
  Vector<String> out;
  Int fm = makeForm(fmt, "angle");
  Quantum<Vector<Double> > val =  qh.asQuantumVectorDouble();
  Int nelem = val.getValue().nelements();
  IPosition shp = val.getValue().shape();
  if (nelem > 0) {
    Int nrow = shp(shp.nelements()-1);
    Int ncol = nelem/nrow;
    out.resize(nrow);
    Int k = 0;
    for (Int i=0; i<nrow; i++) {
      ostringstream oss;
      if (ncol > 1 && form2) oss << '[';
      for (Int j=0; j<ncol; j++) {
        if (j>0) {
          if (form2) oss << ", ";
          else oss << " ";
        };
        oss << MVAngle(Quantity(val.getValue()(k),
                      val.getFullUnit())).
            string(fm, prec);
        k++;
      };
      if (ncol > 1 && form2) oss << ']';
      out(i) = oss.str();
    }
  } else {
    out.resize(0);
  }
  return out;
}

Vector<String> QuantaProxy::tfreq(const Record& rec, const String& fmt,
                                  Bool form2)
{
  QuantumHolder qh = rec2qh(rec);
  Vector<String> out;
  Quantum<Vector<Double> > val =  qh.asQuantumVectorDouble();
  Int nelem = val.getValue().nelements();
  IPosition shp = val.getValue().shape();
  Vector<Double> x(val.getValue());
  Quantity y;
  Unit inun(val.getFullUnit());
  Unit outun(fmt);
  for (Int i=0; i<nelem; i++) {
    y = Quantity(x(i), inun);
    x(i)= MVFrequency(y).get(outun).getValue();
    if (nelem > 0) {
      Int nrow = shp(shp.nelements()-1);
      Int ncol = nelem/nrow;
      out.resize(nrow);
      Int k = 0;
      for (Int i=0; i<nrow; i++) {
        ostringstream oss;
        if (ncol > 1) oss << '[';
        for (Int j=0; j<ncol; j++) {
          if (j>0) oss << ", ";
          oss << x(k);
          k++;
        };
        if (ncol > 1) oss << ']';
        if (form2) oss << " " << fmt;
        out(i) = oss.str();
      };
    } else {
      out.resize(0);
    }
  }
  return out;
}

QuantumHolder QuantaProxy::rec2qh(const Record& rec)
{
  QuantumHolder qh;
  String err;
  if ( !qh.fromRecord(err, rec) ) {
    throw(AipsError(err));
  }
  return qh;
}

Record QuantaProxy::quant2rec(const Quantity& q)
{
  QuantumHolder qh(q);
  String err;
  Record rec;
  if ( !qh.toRecord(err, rec) ) {
    throw(AipsError(err));
  }
  return rec;
}

Record QuantaProxy::quant2rec(const Quantum<Vector<Double> >& q)
{
  QuantumHolder qh(q);
  String err;
  Record rec;
  if ( !qh.toRecord(err, rec) ) {
    throw(AipsError(err));
  }
  return rec;
}

Int QuantaProxy::makeForm(const Vector<String>& in, const String& mode) {
  Int res = -1;
  if (mode == "angle") {
    res = MVAngle::giveMe(mode);
    for (uInt i = 0; i<in.nelements(); i++) res |= MVAngle::giveMe(in[i]);
  } else if (mode == "time") {
    res = MVTime::giveMe(mode);
    for (uInt i = 0; i<in.nelements(); i++) res |= MVTime::giveMe(in[i]);
  }
  return res;
}
