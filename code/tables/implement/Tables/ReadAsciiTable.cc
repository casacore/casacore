//# ReadAsciiTable.cc: Filling a table from an Ascii file
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2000
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

#include <aips/Tables/ReadAsciiTable.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TableColumn.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Regex.h>
#include <aips/Exceptions/Error.h>

#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>

#include <stdio.h>
#include <string.h>
#include <iostream.h>
#include <fstream.h>             // needed for file IO
#include <strstream.h>           // needed for internal IO



//# Helper function 
//# It gets the next value from a line and stores it in result.
//# It updates at and returns the length of the value retrieved.
//# Quotes around strings are removed
//# -1 is returned if no more values are found.
Int readAsciiTableGetNext (const Char* string, Int strlen, Char* result,
			   Int& at)
{
    Int i = 0;
    Bool found  = False;
    Bool quoted = False;
    Char ihave;

    if (string[at] == '\0') {
        return -1;
    }
    for (; at<strlen; at++) {
	ihave = string[at];
	if (ihave == '\'' || ihave == '\"') {
	    if (quoted) {
		quoted = False;
		continue;
	    }else{
		quoted = True;
		found  = True;
		continue;
	    }
	}
	if (ihave == '\0') {
	    result[i] = '\0';
	    return (found  ?  i : -1);
	}
	if (quoted) {
	    result[i++] = ihave;
	    continue;
	}
	if (ihave == '\t') {
	    ihave = ' ';
	}
	if (ihave == ' '  &&  found) {
	    result[i] = '\0';
	    return i;
	}
	if (ihave != ' ') {
	    found = True;
	}
	if (found) {
	    result[i++] = ihave; 
	}
    } 
    return -1;
}



void getTypesAsciiTable (const Char* in, Int leng,
			 Char* string1, Char* string2)
{
    Int at = 0;
    Int i = 0;
    //# When constructing str in the while loop (in the else branch),
    //# a compiler bug appeared on RH systems.
    //# Therefore assignment is used instead.
    String str;
    while (readAsciiTableGetNext (in, leng, string2, at) >= 0) {
        if (string2[0] == '\0') {
	    string1[0] = 'A';
	} else {
	    str = string2;
	    if (str.matches (RXint)) {
	        string1[0] = 'I';
	    } else if (str.matches (RXdouble)) {
	        string1[0] = 'D';
	    } else {
	        string1[0] = 'A';
	    }
	}
	string1[1] = ' ';
	string1 += 2;
	char name[16];
	i++;
	sprintf (name, " Column%i", i);
	strcpy (string2, name);
	string2 += strlen(name);
    }
    string1[0] = '\0';
    string2[0] = '\0';
}



//# Convert a string to a Bool
Bool toBoolAsciiTable (const String& str)
{
    if (str.length() == 0  ||  str == "0"  ||  str[0] == 'F'
    ||  str[0] == 'f'  || str[0] == 'N'  || str[0] == 'n') {
        return False;
    }
    return True;
}



String doReadAsciiTable (const String& headerfile, const String& filein, 
			 const String& tableproto, const String& tablename,
			 Bool autoHeader)
{
    const Int   lineSize = 32768;
          char  string1[lineSize], string2[lineSize];
          char  first[lineSize], second[lineSize];
    const Int   arraySize = 1000;
          String  nameOfColumn[arraySize];
          String  typeOfColumn[arraySize];
          String  keyName;
          String  keyType;
          Int   numberOfKeys[arraySize];
          Int   nVals, haveKeys;

	  LogIO logger(LogOrigin("readAsciiTable", WHERE));


// PART ONE
// Define the TABLE description, i.e. define its columns.
// Create the description as scratch if no name is given.

    TableDesc td (tableproto,
		  (tableproto.empty() ? TableDesc::Scratch : TableDesc::New));

    ifstream jFile;
    jFile.open(headerfile, ios::in);
    if (! jFile) {
        throw (AipsError ("Cannot open header file " + headerfile));
    }

// Read the first line. It will be KEYWORDS or NAMES OF COLUMNS

    if (!jFile.getline(string1, lineSize)) {
	throw (AipsError ("Cannot read first header line of " + headerfile));
    }

// If the first line shows that we have KEYWORDS skip until the
// end of keywords while counting the number of keywords.

    Int lineNumber = 1;
    Int nKeys = 0;
    haveKeys = 0;
    if (strncmp(string1, ".key", 4) == 0) {
	haveKeys = 1;
	while (True) {

// Read the next line(s)

	    if (!jFile.getline(string1, lineSize)) {
		throw (AipsError ("No .endkeywords line in " + headerfile));
	    }
	    lineNumber++;

// If we are at END of KEYWORDS read the next line to get NAMES OF COLUMNS

	    if (strncmp(string1, ".end", 4) == 0) {
		if (!jFile.getline(string1, lineSize)) {
		    throw (AipsError("No COLUMN NAMES line in " + headerfile));
		}
		lineNumber++;
		break;
	    }

// Read the first two fields of a KEYWORD line
	    Int done3, done4, at3=0;
	    done3 = readAsciiTableGetNext (string1, lineSize, first, at3); 
	    done4 = readAsciiTableGetNext (string1, lineSize, second, at3); 
	    if (done3<=0 || done4<=0) {
	        throw (AipsError ("No keyword name and type in line " +
				  String::toString(lineNumber)
				  + " in " + headerfile));
	    }
// Count the number of values for this key

	    nVals = 0;
	    while (readAsciiTableGetNext (string1, lineSize,
					  first, at3) >= 0) {
	        nVals++;
	    }
	    if (nVals == 0) {
	        throw (AipsError ("No keyword value(s) in line " +
				  String::toString(lineNumber)
				  + " in " + headerfile));
	    }
	    if (nKeys >= arraySize) {
	        throw (AipsError ("Too many keywords (max=" +
				  String::toString(arraySize)
				  + ") in " + headerfile));
	    }

	    numberOfKeys[nKeys] = nVals;
	    nKeys++;        
	}
    }

// Determine the types if autoheader is given.

    Int nrheader = lineNumber-1;
    if (autoHeader) {
        getTypesAsciiTable (string1, lineSize, string2, first);
	strcpy (string1, first);
    } else {

// Previous line is NAMES OF COLUMNS; now get TYPE OF COLUMNS line

        if (!jFile.getline(string2, lineSize)) {
	    throw (AipsError("No COLUMN TYPES line in " + headerfile));
	}
	nrheader += 2;
    }
    jFile.close();


// Break up the NAME OF COLUMNS line and the TYPE OF COLUMNS line
// Place the results in the two arrays.
// Also put in in a single string to be returned to the caller.

    String formStr;
    Int done1 = 0, done2 = 0, at1 = 0, at2 = 0, nrcol = 0;
    while (done1 >= 0) {
	done1 = readAsciiTableGetNext (string1, lineSize, first, at1);
	done2 = readAsciiTableGetNext (string2, lineSize, second, at2);
	if (done1>0 && done2>0) {
	    nameOfColumn[nrcol] = String(first);
	    typeOfColumn[nrcol] = String(second);
	    typeOfColumn[nrcol].upcase();
	    if (! formStr.empty()) {
	        formStr += ", ";
	    }
	    formStr += nameOfColumn[nrcol] + "=" + typeOfColumn[nrcol];
	    nrcol++;
	} else if (done1>=0 || done2>=0) {
	    throw (AipsError ("Mismatching COLUMN NAMES AND TYPES lines in "
			      + headerfile));
	}
    }

// Create the TABLE Columns for these variables

    for (Int i5=0; i5<nrcol; i5++) {
	if (typeOfColumn[i5] == "S")
	    td.addColumn (ScalarColumnDesc<Short> (nameOfColumn[i5]));
	if (typeOfColumn[i5] == "I")
	    td.addColumn (ScalarColumnDesc<Int> (nameOfColumn[i5]));
	if (typeOfColumn[i5] == "R")
	    td.addColumn (ScalarColumnDesc<Float> (nameOfColumn[i5]));
	if (typeOfColumn[i5] == "D")
	    td.addColumn (ScalarColumnDesc<Double> (nameOfColumn[i5]));
	if (typeOfColumn[i5] == "X")
	    td.addColumn (ScalarColumnDesc<Complex> (nameOfColumn[i5]));
	if (typeOfColumn[i5] == "Z")
	    td.addColumn (ScalarColumnDesc<Complex> (nameOfColumn[i5]));
	if (typeOfColumn[i5] == "DX")
	    td.addColumn (ScalarColumnDesc<DComplex> (nameOfColumn[i5]));
	if (typeOfColumn[i5] == "DZ")
	    td.addColumn (ScalarColumnDesc<DComplex> (nameOfColumn[i5]));
	if (typeOfColumn[i5] == "A")
	    td.addColumn (ScalarColumnDesc<String> (nameOfColumn[i5]));
	if (typeOfColumn[i5] == "B")
	    td.addColumn (ScalarColumnDesc<Bool> (nameOfColumn[i5]));
    }



// PART TWO
// The TableDesc has now been created.  Start filling in the Table.
// Use the default (AipsIO) storage manager.

    SetupNewTable newtab(tablename, td, Table::New);
    Table tab(newtab);
    Float tempR; Short tempSH; Int tempI; Double tempD;
    Complex tempC;
    Float temp1, temp2, temp3, temp4;
    Double temp1d, temp2d, temp3d, temp4d;
 

// Read key values if we have any.

    if (haveKeys) {
	nKeys = -1;
	TableRecord& keyset = tab.rwKeywordSet();
	ifstream kFile;
	kFile.open(headerfile, ios::in);
	if (! kFile) {
	    throw (AipsError ("Cannot reopen header file " + headerfile));
	}

// Skip the first line which has only the .key keyword on it

	if (!kFile.getline(string1, lineSize)) {
	    throw (AipsError ("Cannot reread first line in " + headerfile));
	}

// Get the following lines

	lineNumber = 1;
	while (True) {
	    lineNumber++;
	    nKeys++;
	    if (!kFile.getline(string1, lineSize)) {
	        throw (AipsError ("Cannot reread line " +
				  String::toString(lineNumber)
				  + " in " + headerfile));
	    }
	    if (strncmp(string1,".end",4) == 0)
		break;

// Read the first two fields of the KEYWORD line

	    Int done3, done4, at3=0;
	    done3 = readAsciiTableGetNext (string1, lineSize, first, at3); 
	    done4 = readAsciiTableGetNext (string1, lineSize, second, at3); 
	    keyName = String(first);
	    keyType = String(second);
	    keyType.upcase();
	    if (keyset.isDefined (keyName)) {
	      logger << LogIO::WARN <<
		"Keyword " << keyName << " skipped because defined twice in "
		     << headerfile << LogIO::POST;
	    } else {
	      

// Read the keyword value(s).
	    
	      nVals = numberOfKeys[nKeys];
	      if (keyType == "S") {
		Vector<Short> vectShort(nVals); 
		for (Int i21=0; i21<nVals; i21++) {
		    done3 = readAsciiTableGetNext (string1, lineSize,
						   first, at3);
		    istrstream(first, done3) >> tempSH;
		    vectShort(i21) = tempSH;
		}
		if (nVals > 1)
		    keyset.define (keyName, vectShort);
		else
		    keyset.define (keyName, vectShort(0));
	      }

	      if (keyType == "I") {
		Vector<Int> vectInt(nVals); 
		for (Int i21=0; i21<nVals; i21++) {
		    done3 = readAsciiTableGetNext (string1, lineSize,
						   first, at3);
		    istrstream(first, done3) >> tempI;
		    vectInt(i21) = tempI;
		}
		if (nVals > 1)
		    keyset.define (keyName, vectInt);
		else
		    keyset.define (keyName, vectInt(0));
	      }

	      if (keyType == "R") {
		Vector<Float> vectFloat(nVals); 
		for (Int i20=0; i20<nVals; i20++) {
		    done3 = readAsciiTableGetNext (string1, lineSize,
						   first, at3);
		    istrstream(first, done3) >> tempR;
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
		    done3 = readAsciiTableGetNext (string1, lineSize,
						   first, at3);
		    istrstream(first, done3) >> tempD;
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
		    done3 = readAsciiTableGetNext (string1, lineSize,
						   first, at3);
		    vectStr(i20) = first;
		}
		if (nVals > 1)
		    keyset.define (keyName, vectStr);
		else
		    keyset.define (keyName, vectStr(0));
	      }

	      if (keyType == "B") {
		Vector<Bool> vectStr(nVals); 
		for (Int i20=0; i20<nVals; i20++) {
		    done3 = readAsciiTableGetNext (string1, lineSize,
						   first, at3);
		    vectStr(i20) = toBoolAsciiTable (first);
		}
		if (nVals > 1)
		    keyset.define (keyName, vectStr);
		else
		    keyset.define (keyName, vectStr(0));
	      }

	      if (keyType == "X") {
		if (nVals%2 != 0) {
		    throw (AipsError ("Complex keyword " + keyName +
				      " in " + headerfile +
				      " must have even number of values"));
		}
		nVals /= 2;
		Vector<Complex> vectCX(nVals); 
		for (Int i20=0; i20<nVals; i20++) {
		    done3 = readAsciiTableGetNext (string1, lineSize,
						   first, at3);
		    done2 = readAsciiTableGetNext (string1, lineSize,
						   second, at3);
		    istrstream(first, done3) >> temp1;
		    istrstream(second, done2) >> temp2;
		    tempC = Complex(temp1, temp2);
		    vectCX(i20) = tempC;
		}
		if (nVals > 1)
		    keyset.define (keyName, vectCX);
		else
		    keyset.define (keyName, vectCX(0));
	      }
	      if (keyType == "DX") {
		if (nVals%2 != 0) {
		    throw (AipsError ("DComplex keyword " + keyName +
				      " in " + headerfile +
				      " must have even number of values"));
		}
		nVals /= 2;
		Vector<DComplex> vectCX(nVals); 
		for (Int i20=0; i20<nVals; i20++) {
		    done3 = readAsciiTableGetNext (string1, lineSize,
						   first, at3);
		    done2 = readAsciiTableGetNext (string1, lineSize,
						   second, at3);
		    istrstream(first, done3) >> temp1d;
		    istrstream(second, done2) >> temp2d;
		    tempC = DComplex(temp1d, temp2d);
		    vectCX(i20) = tempC;
		}
		if (nVals > 1)
		    keyset.define (keyName, vectCX);
		else
		    keyset.define (keyName, vectCX(0));
	      }

	      if (keyType == "Z") {
		if (nVals%2 != 0) {
		    throw (AipsError ("Complex keyword " + keyName +
				      " in " + headerfile +
				      " must have even number of values"));
		}
		nVals /= 2;
		Vector<Complex> vectCX(nVals); 
		for (Int i20=0; i20<nVals; i20++) {
		    done3 = readAsciiTableGetNext (string1, lineSize,
						   first, at3);
		    done2 = readAsciiTableGetNext (string1, lineSize,
						   second, at3);
		    istrstream(first, done3) >> temp1;
		    istrstream(second, done2) >> temp2;
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
	      if (keyType == "DZ") {
		if (nVals%2 != 0) {
		    throw (AipsError ("DComplex keyword " + keyName +
				      " in " + headerfile +
				      " must have even number of values"));
		}
		nVals /= 2;
		Vector<DComplex> vectCX(nVals); 
		for (Int i20=0; i20<nVals; i20++) {
		    done3 = readAsciiTableGetNext (string1, lineSize,
						   first, at3);
		    done2 = readAsciiTableGetNext (string1, lineSize,
						   second, at3);
		    istrstream(first, done3) >> temp1d;
		    istrstream(second, done2) >> temp2d;
		    temp2d *= 3.14159265/180.0; 
		    temp3d = temp1d * cos(temp2d);
		    temp4d = temp1d * sin(temp2d);
		    tempC = DComplex(temp3d, temp4d);
		    vectCX(i20) = tempC;
		}
		if (nVals > 1)
		    keyset.define (keyName, vectCX);
		else
		    keyset.define (keyName, vectCX(0));
	      }
	    }
	}
     
	kFile.close();
    }


// Now open the actual data file

    ifstream iFile;
    iFile.open(filein, ios::in);
    if (! iFile) {
        throw (AipsError ("Cannot open data file " + filein));
    }

    TableColumn* tabcol = new TableColumn[nrcol];
    if (tabcol == 0) {
	throw (AllocError ("readAsciiTable", nrcol));
    }
    for (Int i=0; i<nrcol; i++) {
	tabcol[i].reference (TableColumn (tab, nameOfColumn[i]));
    }
    uInt rownr = 0;

// Skip the header lines if headers and data are in the same file.

    if (headerfile == filein) {
        while (nrheader-- > 0) {
	    if (!iFile.getline(string1, lineSize)) {
	        throw (AipsError ("Error while skipping header lines in "
				  + filein));
	    }
	}
    }

// OK, Now we have real data
	
    while(iFile.getline(string1, lineSize)) {
	at1 = 0; 
	tab.addRow();
	for (Int i6=0; i6<nrcol; i6++) {
	    done1 = readAsciiTableGetNext (string1, lineSize, first, at1);
	    done2 = 1;
	    if (typeOfColumn[i6] == "X"  ||  typeOfColumn[i6] == "DX"
	    ||  typeOfColumn[i6] == "Z"  ||  typeOfColumn[i6] == "DZ") {
		done2 = readAsciiTableGetNext (string1, lineSize, second, at1);
	    }
	    if (done1<0  || done2<0) {
	        tab.flush();
	        throw (AipsError ("Confused about input in " + filein
				  + "\nLast text seen: " + string1
				  + "\nThis occured at about row "
				  + String::toString(tab.nrow())
				  + " (excluding headers). "
				  + "The rest of the file is ignored."));
	    }
	    if (typeOfColumn[i6] == "S") {
		istrstream(first, done1) >> tempSH;
		tabcol[i6].putScalar (rownr, tempSH);
	    }
	    if (typeOfColumn[i6] == "I") {
		istrstream(first, done1) >> tempI;
		tabcol[i6].putScalar (rownr, tempI);
	    }
	    if (typeOfColumn[i6] == "R") {
		istrstream(first, done1) >> tempR;
		tabcol[i6].putScalar (rownr, tempR);
	    }
	    if (typeOfColumn[i6] == "D") {
		istrstream(first, done1) >> tempD;
		tabcol[i6].putScalar (rownr, tempD);
	    }
	    if (typeOfColumn[i6] == "X") {
		istrstream(first, done1) >> temp1;
		istrstream(second, done2) >> temp2;
		tempC = Complex(temp1, temp2);
		tabcol[i6].putScalar (rownr, tempC);
	    }
	    if (typeOfColumn[i6] == "DX") {
		istrstream(first, done1) >> temp1d;
		istrstream(second, done2) >> temp2d;
		tempC = DComplex(temp1d, temp2d);
		tabcol[i6].putScalar (rownr, tempC);
	    }
	    if (typeOfColumn[i6] == "Z") {
		istrstream(first, done1) >> temp1;
		istrstream(second, done2) >> temp2;
		temp2 *= 3.14159265/180.0; 
		temp3 = temp1 * cos(temp2);
		temp4 = temp1 * sin(temp2);
		tempC = Complex(temp3, temp4);
		tabcol[i6].putScalar (rownr, tempC);
	    }
	    if (typeOfColumn[i6] == "DZ") {
		istrstream(first, done1) >> temp1d;
		istrstream(second, done2) >> temp2d;
		temp2d *= 3.14159265/180.0; 
		temp3d = temp1d * cos(temp2d);
		temp4d = temp1d * sin(temp2d);
		tempC = DComplex(temp3d, temp4d);
		tabcol[i6].putScalar (rownr, tempC);
	    }
	    if (typeOfColumn[i6] == "A") {
		tabcol[i6].putScalar (rownr, String(first));
	    }
	    if (typeOfColumn[i6] == "B") {
		tabcol[i6].putScalar (rownr, toBoolAsciiTable(first));
	    }
	}
	rownr++;
    }

    delete [] tabcol;
    iFile.close();
    return formStr;
}


String readAsciiTable (const String& headerfile, const String& filein, 
		       const String& tableproto, const char* tablename)
{
  return doReadAsciiTable (headerfile, filein, tableproto, String(tablename),
			   False);
}

String readAsciiTable (const String& headerfile, const String& filein, 
		       const String& tableproto, const String& tablename)
{
  return doReadAsciiTable (headerfile, filein, tableproto, tablename, False);
}

String readAsciiTable (const String& filein, const String& tableproto,
		       const String& tablename, Bool autoHeader)
{
  return doReadAsciiTable (filein, filein, tableproto, tablename, autoHeader);
}
