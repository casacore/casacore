//# MeasuresProxy.cc: Proxy class object, to be used in language bindings
//# Copyright (C) 2006
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

#include <casacore/measures/Measures/MeasuresProxy.h>
#include <casacore/measures/Measures.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MCFrequency.h>
#include <casacore/measures/Measures/MCDoppler.h>
#include <casacore/measures/Measures/MCRadialVelocity.h>
#include <casacore/measures/Measures/MCBaseline.h>
#include <casacore/measures/Measures/MCuvw.h>
#include <casacore/measures/Measures/MCEarthMagnetic.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/MeasComet.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/measures/Measures/MeasureHolder.h>
#include <casacore/casa/Quanta/QuantumHolder.h>
#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/namespace.h>

MeasuresProxy::MeasuresProxy() : pcomet_p(0) {;}

MeasuresProxy::~MeasuresProxy() {
  delete pcomet_p;
}

String MeasuresProxy::getMeasureType(const Record &in) {
  //Bool b;
  String out;
  if (in.isDefined("type")) {
    out= "???";//b = GlishArray(in.get("type")).get(out);
  } else {
    out = "none";
  }
  return out;
}

Bool MeasuresProxy::doFrame(const MeasureHolder &in) {
  if (in.isMPosition() || in.isMDirection() ||
      in.isMEpoch() || in.isMRadialVelocity()) {
    frame_p.set(in.asMeasure());
    return True;
  }
  return False;
}

Bool MeasuresProxy::doFrame(const String &in) {
  try {
    delete pcomet_p; pcomet_p = 0;
    if (in.empty()) {
      pcomet_p = new MeasComet;
    } else {
      pcomet_p = new MeasComet(in);
    }
    if (!pcomet_p->ok()) {
      delete pcomet_p; pcomet_p = 0;
      return False;
    }
    frame_p.set(*pcomet_p);
  } catch (AipsError(x)) {
    return False;
  } 
  return True;
}


String MeasuresProxy::dirshow(const Record& rec)
{
  String out;
  MeasureHolder mh = rec2mh(rec);
  if (mh.isMeasure()) {
    ostringstream os;
    os << mh.asMeasure() << " " << (mh.asMeasure()).getRefString();
    out = os.str();
  } else {
    throw(AipsError("Non-measure input"));
  }
  return out;
}

// Convert measures
Bool MeasuresProxy::makeMeasure(String &error, MeasureHolder &out,
				const MeasureHolder &in, const String &outref,
				const Record &off) {
  MeasureHolder mo;
  if (off.nfields() > 0) {
    if (!mo.fromRecord(error, off)) {
      error += String("Non-measure type offset in measure conversion\n");
      return False;
    }
    mo.asMeasure().getRefPtr()->set(frame_p);
  }
  in.asMeasure().getRefPtr()->set(frame_p);
  try {
    if (in.isMEpoch()) {
      MEpoch::Ref outRef;
      MEpoch::Types tp;
      String x = outref;
      Bool raze = False;
      if (x.before(2) == "r_" || x.before(2) == "R_") {
	raze = True;
	x = x.from(2);
      }
      if (MEpoch::getType(tp, x)) {
	if (raze) outRef.setType(tp | MEpoch::RAZE);
	else outRef.setType(tp);
      } else outRef.setType(MEpoch::DEFAULT);
      outRef.set(frame_p);
      if (!mo.isEmpty()) {
	if (mo.isMEpoch()) outRef.set(mo.asMeasure());
	else {
	  error += "Non-conforming offset measure type\n";
	  return False;
	}
      }
      MEpoch::Convert mcvt(MEpoch::Convert(in.asMeasure(), outRef));
      out = MeasureHolder(mcvt());
      out.makeMV(in.nelements());
      for (uInt i=0; i<in.nelements(); i++) {
        if (!out.setMV(i, mcvt(dynamic_cast<const MVEpoch &>
                               (*in.getMV(i))).getValue())) {
	  error += "Cannot get extra measure value in DOmeasures::measures\n";
	  return False;
	}
      }
    } else if (in.isMPosition()) {
      MPosition::Ref outRef;
      MPosition::Types tp;
      if (MPosition::getType(tp, outref)) outRef.setType(tp);
      else outRef.setType(MPosition::DEFAULT);
      outRef.set(frame_p);
      if (!mo.isEmpty()) {
	if (mo.isMPosition()) outRef.set(mo.asMeasure());
	else {
	  error += "Non-conforming offset measure type\n";
	  return False;
	}
      }
      MPosition::Convert mcvt(MPosition::Convert(in.asMeasure(), outRef));
      out = MeasureHolder(mcvt());
      out.makeMV(in.nelements());
      for (uInt i=0; i<in.nelements(); i++) {
        if (!out.setMV(i, mcvt(dynamic_cast<const MVPosition &>
                               (*in.getMV(i))).getValue())) {
	  error += "Cannot get extra measure value in DOmeasures::measures\n";
	  return False;
	}
      }
    } else if (in.isMDirection()) {
      MDirection::Ref outRef;
      MDirection::Types tp;
      if (MDirection::getType(tp, outref)) outRef.setType(tp);
      else outRef.setType(MDirection::DEFAULT);
      outRef.set(frame_p);
      if (!mo.isEmpty()) {
	if (mo.isMDirection()) outRef.set(mo.asMeasure());
	else {
	  error += "Non-conforming offset measure type\n";
	  return False;
	}
      }
      MDirection::Convert mcvt(MDirection::Convert(in.asMeasure(), outRef));
      out = MeasureHolder(mcvt());
      out.makeMV(in.nelements());
      for (uInt i=0; i<in.nelements(); i++) {
	if (!out.setMV(i, mcvt(dynamic_cast<const MVDirection &>
			       (*in.getMV(i))).getValue())) {
	  error += "Cannot get extra measure value in DOmeasures::measures\n";
	  return False;
	}
      }
   } else if (in.isMFrequency()) {
      MFrequency::Ref outRef;
      MFrequency::Types tp;
      if (MFrequency::getType(tp, outref)) outRef.setType(tp);
      else outRef.setType(MFrequency::DEFAULT);
      outRef.set(frame_p);
      if (!mo.isEmpty()) {
	if (mo.isMFrequency()) outRef.set(mo.asMeasure());
	else {
	  error += "Non-conforming offset measure type\n";
	  return False;
	}
      }
      MFrequency::Convert mcvt(MFrequency::Convert(in.asMeasure(), outRef));
      out = MeasureHolder(mcvt());
      out.makeMV(in.nelements());
      for (uInt i=0; i<in.nelements(); i++) {
	if (!out.setMV(i, mcvt(dynamic_cast<const MVFrequency &>
			       (*in.getMV(i))).getValue())) {
	  error += "Cannot get extra measure value in DOmeasures::measures\n";
	  return False;
	}
      }
    } else if (in.isMDoppler()) {
      MDoppler::Ref outRef;
      MDoppler::Types tp;
      if (MDoppler::getType(tp, outref)) outRef.setType(tp);
      else outRef.setType(MDoppler::DEFAULT);
      outRef.set(frame_p);
      if (!mo.isEmpty()) {
	if (mo.isMDoppler()) outRef.set(mo.asMeasure());
	else {
	  error += "Non-conforming offset measure type\n";
	  return False;
	}
      }
      MDoppler::Convert mcvt(MDoppler::Convert(in.asMeasure(), outRef));
      out = MeasureHolder(mcvt());
      out.makeMV(in.nelements());
      for (uInt i=0; i<in.nelements(); i++) {
        if (!out.setMV(i, mcvt(dynamic_cast<const MVDoppler &>
                               (*in.getMV(i))).getValue())) {
	  error += "Cannot get extra measure value in DOmeasures::measures\n";
	  return False;
	}
      }
    } else if (in.isMRadialVelocity()) {
      MRadialVelocity::Ref outRef;
      MRadialVelocity::Types tp;
      if (MRadialVelocity::getType(tp, outref)) outRef.setType(tp);
      else outRef.setType(MRadialVelocity::DEFAULT);
      outRef.set(frame_p);
      if (!mo.isEmpty()) {
	if (mo.isMRadialVelocity()) outRef.set(mo.asMeasure());
	else {
	  error += "Non-conforming offset measure type\n";
	  return False;
	}
      }
      MRadialVelocity::Convert
	mcvt(MRadialVelocity::Convert(in.asMeasure(), outRef));
      out = MeasureHolder(mcvt());
      out.makeMV(in.nelements());
      for (uInt i=0; i<in.nelements(); i++) {
        if (!out.setMV(i, mcvt(dynamic_cast<const MVRadialVelocity &>
                               (*in.getMV(i))).getValue())) {
	  error += "Cannot get extra measure value in DOmeasures::measures\n";
	  return False;
	}
      }
    } else if (in.isMBaseline()) {
      MBaseline::Ref outRef;
      MBaseline::Types tp;
      if (MBaseline::getType(tp, outref)) outRef.setType(tp);
      else outRef.setType(MBaseline::DEFAULT);
      outRef.set(frame_p);
      if (!mo.isEmpty()) {
	if (mo.isMBaseline()) outRef.set(mo.asMeasure());
	else {
	  error += "Non-conforming offset measure type\n";
	  return False;
	}
      }
      MBaseline::Convert mcvt(MBaseline::Convert(in.asMeasure(), outRef));
      out = MeasureHolder(mcvt());
      out.makeMV(in.nelements());
      for (uInt i=0; i<in.nelements(); i++) {
        if (!out.setMV(i, mcvt(dynamic_cast<const MVBaseline &>
                               (*in.getMV(i))).getValue())) {
	  error += "Cannot get extra measure value in DOmeasures::measures\n";
	  return False;
	}
      }
    } else if (in.isMuvw()) {
      Muvw::Ref outRef;
      Muvw::Types tp;
      if (Muvw::getType(tp, outref)) outRef.setType(tp);
      else outRef.setType(Muvw::DEFAULT);
      outRef.set(frame_p);
      if (!mo.isEmpty()) {
	if (mo.isMuvw()) outRef.set(mo.asMeasure());
	else {
	  error += "Non-conforming offset measure type\n";
	  return False;
	}
      }
      Muvw::Convert mcvt(Muvw::Convert(in.asMeasure(), outRef));
      out = MeasureHolder(mcvt());
      out.makeMV(in.nelements());
      for (uInt i=0; i<in.nelements(); i++) {
        if (!out.setMV(i, mcvt(dynamic_cast<const MVuvw &>
                               (*in.getMV(i))).getValue())) {
	  error += "Cannot get extra measure value in DOmeasures::measures\n";
	  return False;
	}
      }
    } else if (in.isMEarthMagnetic()) {
      MEarthMagnetic::Ref outRef;
      MEarthMagnetic::Types tp;
      if (MEarthMagnetic::getType(tp, outref)) outRef.setType(tp);
      else outRef.setType(MEarthMagnetic::DEFAULT);
      outRef.set(frame_p);
      if (!mo.isEmpty()) {
	if (mo.isMEarthMagnetic()) outRef.set(mo.asMeasure());
	else {
	  error += "Non-conforming offset measure type\n";
	  return False;
	}
      }
      MEarthMagnetic::Convert
	mcvt(MEarthMagnetic::Convert(in.asMeasure(), outRef));
      out = MeasureHolder(mcvt());
      out.makeMV(in.nelements());
      for (uInt i=0; i<in.nelements(); i++) {
        if (!out.setMV(i, mcvt(dynamic_cast<const MVEarthMagnetic &>
                               (*in.getMV(i))).getValue())) {
	  error += "Cannot get extra measure value in DOmeasures::measures\n";
	  return False;
	}
      }
    }
    if (out.isEmpty()) {
      error += "No measure created; probably unknow measure type\n";
      return False;
    }
  } catch (AipsError (x)) {
    error += "Cannot convert due to missing frame information\n";
    return False;
  }
  return True;
}

// Make uvw from baselines
Bool MeasuresProxy::toUvw(String &error, MeasureHolder &out,
		     Vector<Double> &xyz, Vector<Double> &dot,
		     const MeasureHolder &in) {
  if (!in.isMBaseline()) {
    error += "Trying to convert non-baseline to uvw\n";
    return False;
  }
  try {
    in.asMeasure().getRefPtr()->set(frame_p);   // attach frame
    MBaseline::Convert mcvt(in.asMeasure(), MBaseline::J2000);
    const MVBaseline &bas2000 = mcvt().getValue();
    MVDirection dir2000;
    Double dec2000;
    if (!frame_p.getJ2000(dir2000) || !frame_p.getJ2000Lat(dec2000)) {
      error += "No direction in frame for uvw calculation\n";
      return False;
    }
    MVuvw uvw2000 = MVuvw(bas2000, dir2000);
    out = MeasureHolder(Muvw(uvw2000, Muvw::J2000));
    uInt nel = in.nelements() == 0 ? 1 : in.nelements();
    out.makeMV(in.nelements());
    Double sd = sin(dec2000);
    Double cd = cos(dec2000);
    dot.resize(3*nel);
    xyz.resize(3*nel);
    if (in.nelements() == 0) {
      xyz = uvw2000.getValue();
      dot[0] = -sd*xyz[1] + cd*xyz[2];
      dot[1] = +sd*xyz[0];
      dot[2] = -cd*xyz[0];
    }
    for (uInt i=0; i<3*in.nelements(); i+=3) {
      const MVuvw &mv = MVuvw(mcvt(dynamic_cast<const MVBaseline &>
				   (*in.getMV(i/3))).getValue(), dir2000);
      if (!out.setMV(i/3, mv)) {
	error += "Cannot get extra baseline value in DOmeasures::toUvw\n";
	return False;
      }
      for (uInt j=0; j<3; ++j) xyz[i+j] = mv.getValue()[j];
      dot[i+0] = -sd*xyz[i+1] + cd*xyz[i+2];
      dot[i+1] = +sd*xyz[i+0];
      dot[i+2] = -cd*xyz[i+0];
    }
    for (uInt j=0; j<3*nel; ++j) {
      dot[j] *= C::pi/180/240./1.002737909350795;
    }

  } catch (AipsError(x)) {
    error += "Cannot convert baseline to uvw: frame "
      "information missing";
    return False;
  }
  return True;
}

// Expand positions to baselines
Bool MeasuresProxy::expandIt(String &error, MeasureHolder &out,
			     Vector<Double> &xyz,
			     const MeasureHolder &in) {
  if (!in.isMuvw()) {
    error += "Trying to expand non-baseline type\n";
    return False;
  }
  const MVuvw &uvw2000 = in.asMuvw().getValue();
  if (in.nelements() < 2) {
    xyz.resize(3);
    xyz = uvw2000.getValue();
    out = MeasureHolder(Muvw(uvw2000, Muvw::J2000));
  } else {
    uInt nel = (in.nelements() * (in.nelements()-1))/2;
    xyz.resize(3*nel);
    uInt k=0;
    for (uInt i=0; i<in.nelements(); ++i) {
      for (uInt j=i+1; j<in.nelements(); ++j) {
	MVuvw mv = (dynamic_cast<const MVuvw &>(*in.getMV(j))).getValue();
	mv -= (dynamic_cast<const MVuvw &>(*in.getMV(i))).getValue();
	if (k == 0) {
	  out = MeasureHolder(Muvw(mv, Muvw::J2000));
	  out.makeMV(nel);
	}
	if (!out.setMV(k, mv)) {
	  error += "Cannot expand baseline value in DOmeasures::expand\n";
	  return False;
	}
	for (uInt j=0; j<3; ++j) xyz[3*k+j] = mv.getValue()[j];
	++k;
      }
    }
  }
  return True;
}

MeasureHolder MeasuresProxy::rec2mh(const Record& rec)
{
  MeasureHolder mh;
  String err;
  if (!mh.fromRecord(err, rec)) {
    throw AipsError(err);
  }
  return mh;
}

Record MeasuresProxy::mh2rec(const MeasureHolder& mh)
{
  Record rec;
  String err;
  if (!mh.toRecord(err, rec)) {
    throw AipsError(err);
  }
  return rec;
}

Record MeasuresProxy::measure(const Record& rec, const String& str, 
			      const Record& form)
{
  
  MeasureHolder mhout;
  const MeasureHolder& mhin = rec2mh(rec);
  String err;
  if (!makeMeasure(err, mhout, mhin, str, form)) {
    throw AipsError(err);
  }
      return mh2rec(mhout);
}

Bool MeasuresProxy::doframe(const Record& rec)
{
  /// @todo string method
  MeasureHolder mh = rec2mh(rec);
  return doFrame(mh);
}

Record MeasuresProxy::doptorv(const Record& rec, const String& str)
{
  MeasureHolder mh = rec2mh(rec);
  MeasureHolder mhout;
  MRadialVelocity::Ref outRef;
  MRadialVelocity tout;
  tout.giveMe(outRef, str);
  mhout =
    MeasureHolder(MRadialVelocity::
		  fromDoppler(mh.asMDoppler(), 
			      static_cast<MRadialVelocity::Types>
			      (outRef.getType())));
  uInt nel(mh.nelements());
  if (nel>0) {
    mhout.makeMV(nel);
    MDoppler::Convert mfcv(mh.asMDoppler(),
			   mh.asMDoppler().getRef());
    for (uInt i=0; i<nel; i++) {
      mhout.
	setMV(i, MRadialVelocity::
	      fromDoppler(mfcv(mh.getMV(i)),
			  static_cast<MRadialVelocity::Types>
			  (outRef.getType())).getValue());
    }
  }
  return mh2rec(mhout);
}
Record MeasuresProxy::doptofreq(const Record& rec, const String& str,
				const Quantity& form)
{
  MeasureHolder mh = rec2mh(rec);
  MeasureHolder mhout;
  MFrequency::Ref outRef;
  MFrequency tout;
  tout.giveMe(outRef, str);
  mhout =
    MeasureHolder(MFrequency::
		  fromDoppler(mh.asMDoppler(),
			      MVFrequency(form),
			      static_cast<MFrequency::Types>
			      (outRef.getType())));
  uInt nel(mh.nelements());
  if (nel>0) {
    mhout.makeMV(nel);
    MDoppler::Convert mfcv(mh.asMDoppler(),
			   mh.asMDoppler().getRef());
    for (uInt i=0; i<nel; i++) {
      mhout.
	setMV(i, MFrequency::
	      fromDoppler(mfcv(mh.getMV(i)),
			  MVFrequency(form),
			  static_cast<MFrequency::Types>
			  (outRef.getType())).getValue());
    }
  }
  return mh2rec(mhout);
}

Record MeasuresProxy::todop(const Record& rec, 
			    const Quantity& form)
{
  MeasureHolder mh = rec2mh(rec);
  MeasureHolder mhout;
  if (mh.isMRadialVelocity()) {
    mhout = MRadialVelocity::toDoppler(mh.asMeasure());
    uInt nel(mh.nelements());
    if (nel>0) {
      mhout.makeMV(nel);
      MRadialVelocity::Convert mfcv(mh.asMRadialVelocity(),
				    mh.asMRadialVelocity().getRef());
      for (uInt i=0; i<nel; i++) {
	mhout.setMV(i, MRadialVelocity::
		    toDoppler(mfcv(mh.getMV(i))).
		    getValue());
      }
    }
  } else if (mh.isMFrequency()) {
    mhout = MFrequency::toDoppler(mh.asMeasure(),
				   MVFrequency(form));
    uInt nel(mh.nelements());
    if (nel>0) {
      mhout.makeMV(nel);
      MFrequency::Convert mfcv(mh.asMFrequency(),
			       mh.asMFrequency().getRef());
      for (uInt i=0; i<nel; i++) {
	mhout.setMV(i, MFrequency::
		    toDoppler(mfcv(mh.getMV(i)),
			      MVFrequency(form)).
		    getValue());
      }
    }
  } else {
    throw(AipsError("todop can only convert MFrequency or MRadialVelocity"));
  }
  return mh2rec(mhout);
}

Record MeasuresProxy::torest(const Record& rec, const Record& dop)
{
  MeasureHolder val = rec2mh(rec);
  MeasureHolder arg = rec2mh(dop);
  MeasureHolder mhout;
  mhout = 
    MeasureHolder(MFrequency::toRest(val.asMFrequency(),
				     arg.asMDoppler()));
  uInt nel(val.nelements());
  if (nel != arg.nelements()) {
    throw(AipsError("Incorrect length of doppler or frequency in torest"));
  }
  if (nel>0) {
    mhout.makeMV(nel);
    MFrequency::Convert mfcv(val.asMFrequency(),
			     val.asMFrequency().getRef());
    MDoppler::Convert mdcv(arg.asMDoppler(),
			   arg.asMDoppler().getRef());
    for (uInt i=0; i<nel; i++) {
      mhout.setMV(i, MFrequency::
		  toRest(mfcv(val.getMV(i)),
			 mdcv(arg.getMV(i))).
		  getValue());
    }
  }
  return mh2rec(mhout);
}



String MeasuresProxy::vec2str(const Vector<String>& lst)
{
  String out;
  if (lst.nelements() > 0) {
    // Note in next one the const throw away, since join does not accept
    // const String src[]
    Bool deleteIt; 
    String *storage = const_cast<String *>(lst.getStorage(deleteIt));
    const String *cstorage = storage;
    out = join(storage, lst.nelements(), String(" "));
    lst.freeStorage(cstorage, deleteIt);
  }
  return out;
}

Vector<String> MeasuresProxy::obslist()
{
  return MeasTable::Observatories();
}

Vector<String> MeasuresProxy::srclist()
{
  return MeasTable::Sources();
}

Record MeasuresProxy::observatory(const String& str)
{
  MPosition obs;
  if (!MeasTable::Observatory(obs, str)) {
    throw(AipsError("Unknown observatory asked for."));
  }
  MeasureHolder mh(obs);
  return mh2rec(mh);
}

Record MeasuresProxy::source(const String& str)
{
  MDirection src;
  if (!MeasTable::Source(src, str)) {
    throw(AipsError("Unknown source asked for."));
  }
  MeasureHolder mh(src);
  return mh2rec(mh);
}

// Refmans says thsi is advanced use (Wim) only, so we ignore it
/*
  // addev
  case 10: {
    Parameter<MeasureHolder> val(parameters, valName,
				 ParameterSet::In);
    Parameter<Array<Quantum<Double> > > returnval(parameters, returnvalName,
						  ParameterSet::Out);
    if (runMethod) {
      Vector<Quantum<Double> > res =
	val().asMeasure().getData()->getXRecordValue();
      returnval().resize(IPosition());
      returnval() = res;
    }
  }
  break;
*/

Record MeasuresProxy::alltyp(const Record& rec)
{
  MeasureHolder mh = rec2mh(rec);
  Record outrec;
  Int nall, nex;
  const uInt *typ;
  const String *tall = mh.asMeasure().allTypes(nall, nex, typ);
  Vector<String> tcod(nall-nex);
  Vector<String> text(nex);
  for (Int i=0; i<nall; i++) {
    if (i<nall-nex) tcod(i) = tall[i];
    else text(i-nall+nex) = tall[i];
  }
  outrec.define(String("normal"), tcod);
  outrec.define(String("extra"), text);
  return outrec;
}


Vector<String> MeasuresProxy::linelist()
{
  return MeasTable::Lines();
}

Record MeasuresProxy::line(const String& str)
{
  MFrequency line;
  if (!MeasTable::Line(line, str)) {
    throw(AipsError("Unknown line asked for."));
  }
  MeasureHolder mh(line);
  return mh2rec(mh);
}

Quantum<Vector<Double> > MeasuresProxy::posangle(const Record& lrec, 
					       const Record& rrec)
{
  MeasureHolder mhl = rec2mh(lrec);
  MeasureHolder mhr = rec2mh(rrec);
  MDirection x(mhl.asMDirection());
  MDirection y(mhr.asMDirection());
  x.getRefPtr()->set(frame_p);
  y.getRefPtr()->set(frame_p);
  if (x.isModel()) x = MDirection::Convert(x, MDirection::DEFAULT)();
  if (y.isModel()) y = MDirection::Convert(y, MDirection::DEFAULT)();
  if (x.getRefPtr()->getType() != y.getRefPtr()->getType()) {
    y = MDirection::Convert(y, MDirection::castType
			    (x.getRefPtr()->getType()))();
  }
  return \
    Quantum<Vector<Double> >(
			     Vector<Double>(1, x.getValue().positionAngle(y.getValue(), "deg").getValue()), "deg");
}

Quantum<Vector<Double> > MeasuresProxy::separation(const Record& lrec, const Record& rrec)
{
  MeasureHolder mhl = rec2mh(lrec);
  MeasureHolder mhr = rec2mh(rrec);
  MDirection x(mhl.asMDirection());
  MDirection y(mhr.asMDirection());
  x.getRefPtr()->set(frame_p);
  y.getRefPtr()->set(frame_p);
  if (x.isModel()) x = MDirection::Convert(x, MDirection::DEFAULT)();
  if (y.isModel()) y = MDirection::Convert(y, MDirection::DEFAULT)();
  if (x.getRefPtr()->getType() != y.getRefPtr()->getType()) {
	y = MDirection::Convert(y, MDirection::castType
				(x.getRefPtr()->getType()))();
  }
  return \
    Quantum<Vector<Double> >(
			     Vector<Double>(1, x.getValue().separation(y.getValue(), "deg").getValue()), "deg");

}

Record MeasuresProxy::uvw(const Record& mhrec)
{
  Record outrec;
  MeasureHolder mhin = rec2mh(mhrec);
  MeasureHolder mhout;
  Vector<Double> res;
  Vector<Double> xres;
  String err;
  if (!toUvw(err, mhout, xres, res, mhin)) 
    throw(AipsError(err));
  Record r0;
  mhout.toRecord(err, r0);
  outrec.defineRecord("measure", r0);

  QuantumHolder qh0(Quantum<Vector<Double> >(res, "m/s"));
  QuantumHolder qh1(Quantum<Vector<Double> >(xres, "m"));
  Record r1, r2;
  qh0.toRecord(err, r1);
  qh1.toRecord(err, r2);
  outrec.defineRecord("dot", r1);
  outrec.defineRecord("xyz", r2);
  return outrec;
}

Record MeasuresProxy::expand(const Record& mhrec)
{
  Record outrec;
  MeasureHolder mhin = rec2mh(mhrec);
  MeasureHolder mhout;
  Vector<Double> xres;
  String err;
  if (!expandIt(err, mhout, xres, mhin)) 
    throw(AipsError(err));
  QuantumHolder qh0(Quantum<Vector<Double> >(xres, "m"));
  Record r0, r1;
  mhout.toRecord(err, r0);
  qh0.toRecord(err, r1);
  outrec.defineRecord("measure", r0);
  outrec.defineRecord("xyz", r1);
  return outrec;
}
/*

  // framecomet
  case 14: {
    Parameter<String> val(parameters, valName,
			  ParameterSet::In);
    Parameter<Bool> returnval(parameters, returnvalName,
			      ParameterSet::Out);
    if (runMethod) returnval() = doframe(val());
  }
  break;

  // cometname
  case 15: {
    Parameter<String> returnval(parameters, returnvalName,
				ParameterSet::Out);
    if (runMethod) {
      if (pcomet_p) returnval() = pcomet_p->getName();
      else return error("No Comet table present\n");
    }
  }
  break;

  // comettopo
  case 16: {
    Parameter<Vector<Double > > returnval(parameters, returnvalName,
					  ParameterSet::Out);
    if (runMethod) {
      if (pcomet_p && pcomet_p->getType() == MDirection::TOPO) {
	returnval() = pcomet_p->getTopo().getValue();
      } else {
	return error("No Topocentric Comet table present\n");
      }
    }
  }
  break;

  // comettype
  case 17: {
    Parameter<String> returnval(parameters, returnvalName,
				ParameterSet::Out);
    if (runMethod) {
      if (pcomet_p) {
	if (pcomet_p->getType() == MDirection::TOPO) {
	  returnval() = String("TOPO");
	} else {
	  returnval() = String("APP");
	}
      } else {
	returnval() = String("none");
      }
    }
  }
  break;
*/
