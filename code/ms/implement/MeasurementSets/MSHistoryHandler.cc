//# MSHistoryHandler: helper class to modify/access history
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003,2004
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify
//# it under the terms of the GNU General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or
//# (at your option) any later version.
//#
//# This program is distributed in the hope that it will be useful,
//# but WITHOUT ANY WARRANTY; without even the implied warranty of
//# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//# GNU General Public License for more details.
//# 
//# You should have received a copy of the GNU General Public License
//# along with this program; if not, write to the Free Software
//# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <trial/MeasurementSets/MSHistoryHandler.h>
#include <aips/Logging/LogIO.h>
#include <aips/Measures/MEpoch.h>
#include <aips/MeasurementSets/MSColumns.h>
#include <aips/ostream.h>


MSHistoryHandler::MSHistoryHandler(MeasurementSet& ms, String app){
  histTable_p = ms.history();
  msHistCol_p= new MSHistoryColumns(histTable_p);
  application_p=app;
}


MSHistoryHandler &MSHistoryHandler::operator=(MSHistoryHandler& other){

  if(this != &other){
    histTable_p=other.histTable_p;
    msHistCol_p=other.msHistCol_p;
    

  }

  return *this;
}


MSHistoryHandler::~MSHistoryHandler(){
  delete msHistCol_p;

}

void MSHistoryHandler::addMessage(MeasurementSet& ms, String message,
				  String app,
				  String cliComm, 
				  String origin){

  MSHistory histTable=ms.history();
  Int row = histTable_p.nrow();
  MSHistoryColumns msHistCol(histTable);
  histTable.addRow();
  Time date;
  MEpoch now(MVEpoch(date.modifiedJulianDay()), MEpoch::Ref(MEpoch::UTC));
  msHistCol.timeMeas().put(row, now);
  msHistCol.observationId().put(row,-1);
  msHistCol.priority().put(row,"NORMAL");
  msHistCol.origin().put(row,origin);
  msHistCol.message().put(row,message);
  msHistCol.application().put(row,app);
  Vector<String> cliseq(1);
  cliseq[0]=cliComm;
  msHistCol.cliCommand().put(row, cliseq);
  cliseq[0]="";
  msHistCol.appParams().put(row, cliseq);
}



void MSHistoryHandler::addMessage(String message, String cliComm, 
				  String origin){

  Int row = histTable_p.nrow();
  histTable_p.addRow();
  Time date;
  MEpoch now(MVEpoch(date.modifiedJulianDay()), MEpoch::Ref(MEpoch::UTC));
  msHistCol_p->timeMeas().put(row, now);
  msHistCol_p->observationId().put(row,-1);
  msHistCol_p->priority().put(row,"NORMAL");
  msHistCol_p->origin().put(row,origin);
  msHistCol_p->message().put(row,message);
  msHistCol_p->application().put(row,application_p);
  Vector<String> cliseq(1);
  cliseq[0]=cliComm;
  msHistCol_p->cliCommand().put(row, cliseq);
  cliseq[0]="";
  msHistCol_p->appParams().put(row, cliseq);
}

void MSHistoryHandler::addMessage(LogSinkInterface& sink, String cliComm){

  Int row = histTable_p.nrow();
  uInt newrows = sink.nelements();
  histTable_p.addRow(newrows);
  for (uInt k=0; k< newrows; ++k){

    
    msHistCol_p->time().put(row, sink.getTime(k));
    msHistCol_p->observationId().put(row, -1);
    msHistCol_p->priority().put(row,sink.getPriority(k));
    msHistCol_p->origin().put(row,sink.getLocation(k));
    msHistCol_p->message().put(row,sink.getMessage(k));
    msHistCol_p->application().put(row,application_p);
    Vector<String> cliseq(1);
    cliseq[0]=cliComm;
    msHistCol_p->cliCommand().put(row, cliseq); 
    cliseq[0]="";
    msHistCol_p->appParams().put(row, cliseq);
    ++row;
  }

  sink.clearLocally();


}

void MSHistoryHandler::addMessage(LogIO& os, String cliComm){

  addMessage(os.localSink(), cliComm);
  

}

void MSHistoryHandler::cliCommand(String& cliComm){

  Int row = histTable_p.nrow();
  histTable_p.addRow();
  Vector<String> cliseq(1);
  cliseq[0]=cliComm;
  msHistCol_p->cliCommand().put(row, cliseq);  
  msHistCol_p->application().put(row,application_p);
  cliseq[0]="";
  msHistCol_p->appParams().put(row, cliseq);
}

void MSHistoryHandler::cliCommand(LogIO& cliComm){

  cliCommand(cliComm.localSink());

}
void MSHistoryHandler::cliCommand(LogSinkInterface& sink){

 Int row = histTable_p.nrow();
 uInt numCliComm=sink.nelements();
 histTable_p.addRow();
 Vector<String> cliComm(numCliComm);
 for (uInt k=0; k< numCliComm; ++k){
   cliComm[k]=sink.getMessage(k);   
 }
 msHistCol_p->time().put(row, sink.getTime(0));
 msHistCol_p->observationId().put(row, -1);
 msHistCol_p->priority().put(row,sink.getPriority(0));
 msHistCol_p->origin().put(row,sink.getLocation(0));
 msHistCol_p->cliCommand().put(row, cliComm);
 msHistCol_p->application().put(row,application_p);
 Vector<String> dum(1);
 dum[0]="";
 msHistCol_p->appParams().put(row, dum);
 sink.clearLocally();
 



}
