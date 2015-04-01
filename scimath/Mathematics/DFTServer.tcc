//# DFTServer.cc: This class contains methods for doing n-D slow Fourier transforms
//# Copyright (C) 1994,1995,1996,1999,2001
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

#ifndef SCIMATH_DFTSERVER_TCC
#define SCIMATH_DFTSERVER_TCC

#include <casacore/scimath/Mathematics/DFTServer.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> 
DFTServer<T>& DFTServer<T>::operator=(const DFTServer<T> &other)
{
	if (this == &other) return *this;

    dimension = other.dimension;
	numTime = other.numTime;
	numFreq = other.numFreq;
	crFlag = other.crFlag;
	return *this;
}

template<class T> 
DFTServer<T>::DFTServer()
//
// default constructor.  Throw an exception since we require parameters.
//
{
   throw(DFTError("DFTServer constructor: Error--required parameter missing"));
}

template<class T> 
DFTServer<T>::DFTServer(const DFTServer<T> &other)
//
//copy constructor
//
{
   dimension=other.dimension;
   numTime=other.numTime;
   numFreq=other.numFreq;
   crFlag = other.crFlag;
}


template<class T> 
DFTServer<T>::DFTServer(Array<T> &Time, Array<T> &Freq)
//
// DFTServer can fourier transform a point into a vector, a vector 
// into a point, or a vector into a vector
//
{
        
    crFlag = 0;
    IPosition shapeTime(Time.ndim());
	IPosition shapeFreq(Freq.ndim());

	shapeTime = Time.shape();
	shapeFreq = Freq.shape();

	int dimTime, dimFreq;

	switch (Time.ndim()) {
		case 1: 
			numTime = 1; 
			dimTime = shapeTime(0) - 2; 
			break;

        case 2:
			numTime = shapeTime(0);
			dimTime = shapeTime(1) - 2; 
			break;

        default:
			throw(DFTError("DFTServer constructor -- Error: time array should" 
				  " be a vector or a matrix") );
            break;
    }


	switch (Freq.ndim()) {
		case 1: 
			numFreq = 1; 
			dimFreq = shapeFreq(0) - 2; 
			break;

        case 2:
			numFreq = shapeFreq(0);
			dimFreq = shapeFreq(1) - 2; 
			break;
        default:
			throw(DFTError("DFTServer constructor -- Error: Frequency array "
						   "should be a vector or a matrix") );
            break;
    }

    if (dimTime != dimFreq) 
	   throw(DFTError("DFTServer::DFTServer - Error:  Time and Freq should"
					  " have same dimensions"));

    dimension = dimTime;

	if (dimension > 3) 
        throw(DFTError("DFTServer::dft: Error--cannot handle data points which "
				  " has more than 3 dimensions" ));
}

template<class T> 
DFTServer<T>::DFTServer(int dim, int numTimepts, int numFreqpts)
{
    crFlag = 0;
    dimension = dim;
	if (dimension > 3) 
        throw(DFTError("DFTServer::dft: Error--cannot handle data points which "
				  " has more than 3 dimensions" ));

	numTime = numTimepts;
	numFreq = numFreqpts;
}

template<class T> 
DFTServer<T>::DFTServer(IPosition &shapeTime, IPosition &shapeFreq)
{
    crFlag = 0;
    int dimTime, dimFreq;

	switch (shapeTime.nelements()) {
	case 1: 
	    dimTime = shapeTime(0)-2;
	    numTime=1;
		break;
	case 2:
		dimTime = shapeTime(1)-2;
		numTime = shapeTime(0);
        break;
	default: 
		throw(DFTError("DFTServer constructor -- Error: time array should" 
				  " be a vector or a matrix") );
        break;
    }

	switch (shapeFreq.nelements()) {
	case 1: 
	    dimFreq = shapeFreq(0)-2;
	    numFreq=1;
		break;
	case 2:
		dimFreq = shapeFreq(1)-2;
		numFreq = shapeFreq(0);
        break;
	default: 
		throw(DFTError("DFTServer constructor -- Error: frequency array should" 
				  " be a vector or a matrix") );
        break;
    }

	if (dimTime != dimFreq)
       throw(DFTError("DFTServer constructor: Error--time and UV data must"
				  "have same dimension" ));

    dimension = dimTime;

    if (dimension > 2)
        throw(DFTError("DFTServer::dft: Error--cannot handle data points which "
				  " have more than 3 dimensions" ));

}

template<class T>
DFTServer<T>::~DFTServer()
//
// destructor
//
{
}

template<class T> 
Matrix<T> DFTServer<T>::getMatrix(Array<T> &data)
{
	IPosition Shape(data.ndim());

	Shape = data.shape();

	if (data.ndim() == 1) {
            	if ( (Shape(0) - 2) != dimension)
                throw(DFTError("DFTServer::getMatrix -- Error: data has "
				               " incorrect dimension"));
	        Vector<T> vec;
		vec.reference(data);

		int cols = dimension + 2;
            	Matrix<T> mat(1,cols);
		mat.row(0) = vec;

            	return mat; 

	} else if (data.ndim() == 2) {
            	if ( (Shape(1) - 2) != dimension)
                throw(DFTError("DFTServer::getMatrix -- Error: data has "
				               " incorrect dimension"));

            	Matrix<T> mat;
            	mat.reference(data);
            	return mat; 

	} else {
            	throw(DFTError("DFTServer::dft: Error--cannot handle time data "
				   " which has more than 3 dimensions" ));
       }

}


template<class T> 
void DFTServer<T>::rcdft(Array<T> &Time, Array<T> &Freq)
//
// Time is data from time domain
// Freq is data from frequency domain
//
{

    	Matrix<T> matIn = getMatrix(Time);
    	Matrix<T> matOut = getMatrix(Freq);

    	c2c(matIn, matOut, 1);

	if (Time.ndim() == 1)
		Time = matIn.row(0);

	if (Freq.ndim() == 1)
		Freq = matOut.row(0);
}

template<class T> 
void DFTServer<T>::crdft(Array<T> &Time, Array<T> &Freq)
//
// Time is data from time domain
// Freq is data from frequency domain
//
{

	crFlag = 1;
    	Matrix<T> matIn = getMatrix(Freq);
    	Matrix<T> matOut = getMatrix(Time);

    	c2c(matOut, matIn, 0);

	if (Time.ndim() == 1)
		Time = matIn.row(0);

	if (Freq.ndim() == 1)
		Freq = matOut.row(0);
	crFlag = 0;
}

template<class T> 
void DFTServer<T>::cxdft(Array<T> &Time, Array<T> &Freq, int dir)
//
// Time is data from time domain
// Freq is data from frequency domain
// dir > 0 forward dft ( from time to frequency)
// dir <= 0 backward dft ( from frequency to time) 
//
{

    	Matrix<T> matIn = getMatrix(Time);
    	Matrix<T> matOut = getMatrix(Freq);

    	c2c(matIn, matOut, dir);

	if (Time.ndim() == 1)
		Time = matIn.row(0);

	if (Freq.ndim() == 1)
		Freq = matOut.row(0);
}

template<class T> 
void DFTServer<T>::c2c(Matrix<T> &Time, Matrix<T> &Freq, int dir)
// dir > 0 forward dft ( from time to frequency)
// dir <= 0 backward dft ( from frequency to time) 
//
{
    	T sum_real, sum_imag;

    	if (dir>0)   {          // DFT from time to frequency domain
		for( int j=0; j<=numFreq-1; j++) {
       		 	sum_real=T(0.0);
       		 	sum_imag=T(0.0);

		for( int k=0; k<=numTime-1; k++) {
			T phase = T(0.0);
            		for( int i=2; i<=(dimension+2)-1; i++) {
            			phase += (Time(k,i) * Freq(j,i));
                	}
               		phase=-(C::_2pi)*phase;
               		sum_real += (Time(k,0) * cos(phase) + Time(k,1) * cos(phase +
			   (C::pi_2)) );
               		sum_imag += (Time(k,0) * sin(phase) + Time(k,1) * sin(phase +
			   (C::pi_2)) );
    	   	}

           	Freq(j,0) = sum_real;
           	Freq(j,1) = sum_imag;
    	} 

	} else  {          // DFT from frequency domain to time domain
    		for( int j=0; j<=numTime-1; j++) {
        		sum_real=T(0.0);
        		sum_imag=T(0.0);
    	    	for( int k=0; k<=numFreq-1; k++) {
            		T phase = T(0.0);
            		for( int i=2; i<=(dimension+2)-1; i++) {
            			phase += (Time(j,i) * Freq(k,i));
                	}
               		phase=(C::_2pi)*phase;
               		sum_real+= (Freq(k,0) * cos(phase) + Freq(k,1) * cos(phase+
			   	(C::pi_2)));
			if (crFlag == 0) 
               		sum_imag+= (Freq(k,0) * sin(phase) + Freq(k,1) *
			   	sin(phase + (C::pi_2)));
    	   	}
           	Time(j,0) = sum_real/numFreq;
           	Time(j,1) = sum_imag/numFreq;
    	} 
    }
}


template<class T> 
void DFTServer<T>::showComplex(Array<T> &data)
{
	Vector<T> vec;
	Matrix<T> mat;


	switch (data.ndim()) {
 		case 1: 
 			vec.reference(data);
 			cout << "( " << vec(0)<<"," << vec(1) << endl;
 			break;
 
         case 2:
 			mat.reference(data);
 			cout  << "real components" << endl;
 			cout << mat.column(0);
 			cout << endl;
 			cout  << "Imaginary components" << endl;
 			cout << mat.column(1);
 			cout << endl;
 			break;
 
         default:
 			cerr << "in showComplex, Eorr: cannot handle data which has"
 				 << " dimension greater than 2";
             exit(0);
       }
 
}

template<class T> 
void DFTServer<T>::showReal(Array<T> &data)
{
	Vector<T> vec;
	Matrix<T> mat;

 	switch (data.ndim()) {
 
 		case 1: 
 			vec.reference(data);
			cout << vec(0);
 			cout << endl;
 			break;
 
         case 2:
 			mat.reference(data);
 			cout << mat.column(0);
 			cout << endl;
 			break;
         default:
 			cerr << "in showReal, Eorr: cannot handle data which has"
 				 << " dimension greater than 2";
             exit(0);
 
 	}	

}

} //# NAMESPACE CASACORE - END


#endif
