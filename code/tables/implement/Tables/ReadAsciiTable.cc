//# AsciiFileIO.cc: Filling a table from an Ascii file
//# Copyright (C) 1993,1994,1995,1996,1997
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

#include <aips/Tables/AsciiFileIO.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TableColumn.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/String.h>
#include <aips/Exceptions/Error.h>

#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>

#include <string.h>
#include <iostream.h>
#include <fstream.h>             // needed for file IO
#include <strstream.h>           // needed for internal IO



//# Helper function 
Int readAsciiTableGetNext (const Char* string, Char* result, Int& at)
{
    Int i = 0, startedNew = 0, quoted = 0;
    char  ihave;

    for (;at<1024;at++) {
	ihave = string[at];
	if (ihave == '\'' || ihave == '\"') {
	    if (quoted) {
		quoted = 0;
		continue;
	    }else{
		quoted = 1;
		startedNew = 1;
		continue;
	    }
	}
	if (ihave == '\0') {
	    if (i > 0)
		return 0;
	    else
		return 1;
	}
	if (quoted) {
	    result[i++] = ihave;
	    continue;
	}
	if (ihave == '\t')
	    ihave = ' ';
	if (ihave == ' ' && startedNew != 0)
	    return 0;
	if (ihave != ' ')
	    startedNew = 1;
	if (startedNew)
	    result[i++] = ihave; 
    } 
    return 1;
}




void readAsciiTable (const Char* headerfile, const Char* filein, 
		     const Char* tableproto, const Char* tablename) 
{
    const Int   lineSize = 1024;
          char  string1[lineSize], string2[lineSize];
    const Int   charSize = 120;
          char  first[charSize], second[charSize];
    const Int   arraySize = 100;
          String  nameOfColumn[arraySize];
          String  typeOfColumn[arraySize];
          String  keyName;
          String  keyType;
          Int   numberOfKeys[arraySize];
          Int   array, haveKeys;


	  LogIO logger(LogOrigin("readAsciiTable", WHERE));

try {

// PART ONE
// Define the TABLE structure, i.e. define its columns.
// Create the description as scratch if no name is given.

    TableDesc td (tableproto,
		  (tableproto[0]=='\0' ? TableDesc::Scratch : TableDesc::New));

    ifstream jFile;
    jFile.open(headerfile, ios::in);
    if (! jFile) {
	logger << LogIO::SEVERE <<
	    "Cannot open " << headerfile << "for reading" << LogIO::POST;
	return;
    }

// Read the first line. It will be KEYWORDS or NAMES OF VARIABLES 

    if (!jFile.getline(string1, lineSize)) {
	logger << LogIO::SEVERE <<
	    "Cannot read the first line of " << headerfile << LogIO::POST;
	return;
    }

// If the first line shows that we have KEYWORDS skip until the
// end of keywords.

    Int lineNumber = 0;
    Int nKeys = -1;
    haveKeys = 0;
    if (strncmp(string1,".key",4) == 0) {
	haveKeys = 1;
	while (True) {

// Read the next line(s)

	    if (!jFile.getline(string1, lineSize)) {
		lineNumber++;
		logger << LogIO::SEVERE <<
		"Incomplete keyword set in line number " << lineNumber
		       << " of " << headerfile << LogIO::POST;
		return;
	    }

// If we are at END of KEYWORDS read the next line to get NAMES OF VARIABLES

	    if (strncmp(string1,".end",4) == 0) {
		if (!jFile.getline(string1, lineSize)) {
		    logger << LogIO::SEVERE <<
			"Cannot read the line following '.end' in "
			   << headerfile << LogIO::POST;
		    return;
		}
		break;
	    }

// Read the first two fields of a KEYWORD line
	    Int done3, done4, at3=0;
	    done3 = readAsciiTableGetNext (string1, first, at3); 
	    done4 = readAsciiTableGetNext (string1, second, at3); 
	    if (done3 || done4) {
		logger << LogIO::SEVERE <<
		    "Could not read line number " << lineNumber << LogIO::POST;
		return;
	    }
	    nKeys++;        

// Count the number of values for this key

	    array = 0;
	    while (True) {
		done3 = readAsciiTableGetNext (string1, first, at3); 
		if (done3)
		    break;
		array++;
	    }
	    if (array == 0) {
		logger << LogIO::SEVERE <<
		    "No keyword values on line number "
		       << lineNumber << LogIO::POST;
		return;
	    }
	    numberOfKeys[nKeys] = array;
	}
    }
 
// Now get TYPE OF VARIABLES line

    if (!jFile.getline(string2, lineSize)) {
	logger << LogIO::SEVERE <<
	    "Cannot read the TYPE OF VARIABLE line of " 
	       << headerfile << LogIO::POST;
	return;
    }
    jFile.close();


// Break up the NAME OF VARIABLES line and the TYPE OF VARIABLES line
// Place the results in the two arrays

    Int done1 = 0, done2 = 0, at1 = 0, at2 = 0, entries = 0;
    while (! done1) {
	for (int i3=0;i3<charSize; i3++) {
	    first[i3] = second[i3] = 0;
	}
	done1 = readAsciiTableGetNext (string1, first, at1);
	done2 = readAsciiTableGetNext (string2, second, at2);
	if (done1 || done2)
	    break;
	nameOfColumn[entries] = String(first);
	typeOfColumn[entries] = String(second);
	typeOfColumn[entries].upcase();
	entries++;
    }

// Create the TABLE Columns for these variables

    for (Int i5=0;i5<entries;i5++) {
	if (typeOfColumn[i5] == "I")
	    td.addColumn (ScalarColumnDesc<Int> (nameOfColumn[i5]));
	if (typeOfColumn[i5] == "R")
	    td.addColumn (ScalarColumnDesc<float> (nameOfColumn[i5]));
	if (typeOfColumn[i5] == "D")
	    td.addColumn (ScalarColumnDesc<double> (nameOfColumn[i5]));
	if (typeOfColumn[i5] == "X")
	    td.addColumn (ScalarColumnDesc<Complex> (nameOfColumn[i5]));
	if (typeOfColumn[i5] == "Z")
	    td.addColumn (ScalarColumnDesc<Complex> (nameOfColumn[i5]));
	if (typeOfColumn[i5] == "A")
	    td.addColumn (ScalarColumnDesc<String> (nameOfColumn[i5]));
    }



// PART TWO
// The TableDesc has now been created.  Start filling in the Table.
// Use the default (AipsIO) storage manager.

    SetupNewTable newtab(tablename, td, Table::New);
    Table tab(newtab);
    float tempR; Int tempI; Double tempD; Complex tempC; String tempS;
    float temp1, temp2, temp3, temp4;
 

// Read key values if we have any.

    if (haveKeys) {
	nKeys = -1;
	TableRecord& keyset = tab.rwKeywordSet();
	ifstream kFile;
	kFile.open(headerfile, ios::in);
	if (! kFile) {
	    logger << LogIO::SEVERE <<
		"Cannot reopen " << headerfile << LogIO::POST;
	    return;
	}

// Skip the first line which has only the .key keyword on it

	if (!kFile.getline(string1, lineSize)) {
	    logger << LogIO::SEVERE <<
		"Cannot reread the first line of " << headerfile << LogIO::POST;
	    return;
	}

// Get the following lines

	lineNumber = 0;
	while (True) {
	    lineNumber++;
	    nKeys++;
	    if (!kFile.getline(string1, lineSize)) {
		logger << LogIO::SEVERE <<
		    "Cannot reread line number " << lineNumber 
		     <<" of " << headerfile << LogIO::POST;
		break;
	    }
	    if (strncmp(string1,".end",4) == 0)
		break;

// Read the first two fields of the KEYWORD line

	    Int done3, done4, at3=0;
	    for (Int i2=0; i2<charSize; i2++) {
		first[i2] = second[i2] = 0;
	    }
	    done3 = readAsciiTableGetNext (string1, first, at3); 
	    done4 = readAsciiTableGetNext (string1, second, at3); 
	    keyName = String(first);
	    keyType = String(second);
	    keyType.upcase();

// Read the keyword value(s).
	    
	    Int nVals = numberOfKeys[nKeys];
	    if (keyType == "I") {
		Vector<Int> vectInt(nVals); 
		for (Int i21=0; i21<nVals; i21++) {
		    for (Int i31=0; i31<charSize;i31++)
			first[i31] = 0;
		    done3 = readAsciiTableGetNext (string1, first, at3);
		    istrstream(first, sizeof(first)) >> tempI;
		    vectInt(i21) = tempI;
		}
		if (nVals > 1)
		    keyset.define (keyName, vectInt);
		else
		    keyset.define (keyName, vectInt(0));
	    }

	    if (keyType == "R") {
		Vector<float> vectFloat(nVals); 
		for (Int i20=0; i20<nVals; i20++) {
		    for (Int i30=0; i30<charSize;i30++)
			first[i30] = 0;
		    done3 = readAsciiTableGetNext (string1, first, at3);
		    istrstream(first, sizeof(first)) >> tempR;
		    vectFloat(i20) = tempR;
		}
		if (nVals > 1)
		    keyset.define (keyName, vectFloat);
		else
		    keyset.define (keyName, vectFloat(0));
	    }

	    if (keyType == "D") {
		Vector<Double> vectDbl(nVals); 
		for (Int i20=0; i20<nVals; i20++) {
		    for (Int i30=0; i30<charSize;i30++)
			first[i30] = 0;
		    done3 = readAsciiTableGetNext (string1, first, at3);
		    istrstream(first, sizeof(first)) >> tempD;
		    vectDbl(i20) = tempD;
		}
		if (nVals > 1)
		    keyset.define (keyName, vectDbl);
		else
		    keyset.define (keyName, vectDbl(0));
	    }

	    if (keyType == "A") {
		Vector<String> vectStr(nVals); 
		for (Int i20=0; i20<nVals; i20++) {
		    for (Int i30=0; i30<charSize;i30++)
			first[i30] = 0;
		    done3 = readAsciiTableGetNext (string1, first, at3);
		    tempS = first;
		    vectStr(i20) = tempS;
		}
		if (nVals > 1)
		    keyset.define (keyName, vectStr);
		else
		    keyset.define (keyName, vectStr(0));
	    }

	    if (keyType == "X") {
		if (nVals%2 != 0) {
		    cout << "Complex keyword must have even number of values"
			 << endl;
		    return;
		}
		nVals /= 2;
		Vector<Complex> vectCX(nVals); 
		for (Int i20=0; i20<nVals; i20++) {
		    for (Int i30=0; i30<charSize;i30++)
			first[i30] = second[i30] = 0;
		    done3 = readAsciiTableGetNext (string1, first, at3);
		    done2 = readAsciiTableGetNext (string1, second, at3);
		    istrstream(first, sizeof(first)) >> temp1;
		    istrstream(second, sizeof(second)) >> temp2;
		    tempC = Complex(temp1, temp2);
		    vectCX(i20) = tempC;
		}
		if (nVals > 1)
		    keyset.define (keyName, vectCX);
		else
		    keyset.define (keyName, vectCX(0));
	    }

	    if (keyType == "Z") {
		if (nVals%2 != 0) {
		    cout << "Complex keyword must have even number of values"
			 << endl;
		    return;
		}
		nVals /= 2;
		Vector<Complex> vectCX(nVals); 
		for (Int i20=0; i20<nVals; i20++) {
		    for (Int i30=0; i30<charSize;i30++)
			first[i30] = second[i30] = 0;
		    done3 = readAsciiTableGetNext (string1, first, at3);
		    done2 = readAsciiTableGetNext (string1, second, at3);
		    istrstream(first, sizeof(first)) >> temp1;
		    istrstream(second, sizeof(second)) >> temp2;
		    temp2 *= 3.14159265/180.0; 
		    temp3 = temp1 * cos(temp2);
		    temp4 = temp1 * sin(temp2);
		    tempC = Complex(temp3, temp4);
		    vectCX(i20) = tempC;
		}
		if (nVals > 1)
		    keyset.define (keyName, vectCX);
		else
		    keyset.define (keyName, vectCX(0));
	    }
	    
	}
     
	kFile.close();
    }


// Now open the actual data file

    ifstream iFile;
    iFile.open(filein, ios::in);
    if (! iFile) {
	logger << LogIO::SEVERE <<
	    "Cannot open " << filein << "for reading" << LogIO::POST;
	return;
    }

    TableColumn* tabcol = new TableColumn[entries];
    if (tabcol == 0) {
	throw (AllocError ("readAsciiFile", entries));
    }
    for (Int i=0; i<entries; i++) {
	tabcol[i].reference (TableColumn (tab, nameOfColumn[i]));
    }
    uInt rownr = 0;

    Int skip = 0;
    while(iFile.getline(string1, lineSize)) {
	if (! skip) {

// See if we have to skip the KEYWORD and HEADER lines

	    if (strncmp(string1,".key",4) == 0) {
		while (True) {
		    iFile.getline(string1, lineSize);
		    if (strncmp(string1,".end",4) == 0) {
			iFile.getline(string1, lineSize);
			iFile.getline(string1, lineSize);
			iFile.getline(string1, lineSize);
			break;
		    }
		}
	    }
	    at1 = 0;
	    for (Int i12=0;i12<charSize;i12++)
		first[i12] = 0;
	    done1 = readAsciiTableGetNext (string1, first, at1);
	    if (strcmp(first, nameOfColumn[0].chars()) == 0){
		iFile.getline(string1, lineSize);
		iFile.getline(string1, lineSize);
	    }
	    skip = 1;
	}
	
// OK Now we have real data
	
	at1 = 0; 
	tab.addRow();
	for (Int i6=0;i6<entries;i6++) {
	    for (Int i7=0;i7<charSize; i7++)
		first[i7] =  0;
	    done1 = readAsciiTableGetNext (string1, first, at1);
	    if (done1) {
		logger << LogIO::SEVERE <<
		    "Confused about input. Last text seen was\n" <<
		    string1 << endl <<
		    "This occured at about row " << tab.nrow() <<
		    " (excluding headers). The rest of the file is ignored." <<
		    LogIO::POST;
		return;
	    }
	    if (typeOfColumn[i6] == "I") {
		istrstream(first, sizeof(first)) >> tempI;
		tabcol[i6].putScalar (rownr, tempI);
	    }
	    if (typeOfColumn[i6] == "R") {
		istrstream(first, sizeof(first)) >> tempR;
		tabcol[i6].putScalar (rownr, tempR);
	    }
	    if (typeOfColumn[i6] == "D") {
		istrstream(first, sizeof(first)) >> tempD;
		tabcol[i6].putScalar (rownr, tempD);
	    }
	    if (typeOfColumn[i6] == "X") {
		for (Int i8=0;i8<charSize; i8++)
		    second[i8] =  0;
		done2 = readAsciiTableGetNext (string1, second, at1);
		istrstream(first, sizeof(first)) >> temp1;
		istrstream(second, sizeof(second)) >> temp2;
		tempC = Complex(temp1, temp2);
		tabcol[i6].putScalar (rownr, tempC);
	    }
	    if (typeOfColumn[i6] == "Z") {
		for (Int i9=0;i9<charSize; i9++)
		    second[i9] =  0;
		done2 = readAsciiTableGetNext (string1, second, at1);
		istrstream(first, sizeof(first)) >> temp1;
		istrstream(second, sizeof(second)) >> temp2;
		temp2 *= 3.14159265/180.0; 
		temp3 = temp1 * cos(temp2); temp4 = temp1 * sin(temp2);
		tempC = Complex(temp3, temp4);
		tabcol[i6].putScalar (rownr, tempC);
	    }
	    if (typeOfColumn[i6] == "A") {
		tempS = first;
		tabcol[i6].putScalar (rownr, tempS);
	    }
	}
	rownr++;
    }

    delete [] tabcol;
    iFile.close();
}

    catch (AipsError x) {
	cout << "AsciiFileIO: " << x.getMesg() << endl;
	return;
    }
    
    end_try;
}


void readAsciiTable (const Char* filein, const Char* tableproto,
		     const Char* tablename) 
{
    readAsciiTable (filein, filein, tableproto, tablename);
}
