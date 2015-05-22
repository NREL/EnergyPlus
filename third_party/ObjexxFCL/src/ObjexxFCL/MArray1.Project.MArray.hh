#ifndef ObjexxFCL_MArray1_Project_MArray_hh_INCLUDED
#define ObjexxFCL_MArray1_Project_MArray_hh_INCLUDED

// MArray1.Project.MArray: Project-Specific MArray Methods
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2015 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// Notes:
//  These are convenience methods to provide near-Fortran syntax for MArray usage

	// Types
	template< typename M > using MMA1c = MArray1< MArray1 const, M >;
	template< typename M > using MMA1  = MArray1< MArray1, M >;
	typedef  MMA1c< bool >  MMA1c_bool;
	typedef  MMA1< bool >  MMA1_bool;
	typedef  MMA1c< int >  MMA1c_int;
	typedef  MMA1< int >  MMA1_int;
	typedef  MMA1c< double >  MMA1c_double;
	typedef  MMA1< double >  MMA1_double;
	typedef  MMA1c< std::string >  MMA1c_string;
	typedef  MMA1< std::string >  MMA1_string;

	// double const Members
	inline MMA1c_double QdotSkin() const { return ma( &T::QdotSkin ); }
	inline MMA1c_double SkinLossConvect() const { return ma( &T::SkinLossConvect ); }
	inline MMA1c_double SkinLossRadiat() const { return ma( &T::SkinLossRadiat ); }
	inline MMA1c_double x() const { return ma( &T::x ); }
	inline MMA1c_double y() const { return ma( &T::y ); }
	inline MMA1c_double z() const { return ma( &T::z ); }

	// double Members
	inline MMA1_double QdotSkin() { return ma( &T::QdotSkin ); }
	inline MMA1_double SkinLossConvect() { return ma( &T::SkinLossConvect ); }
	inline MMA1_double SkinLossRadiat() { return ma( &T::SkinLossRadiat ); }
	inline MMA1_double x() { return ma( &T::x ); }
	inline MMA1_double y() { return ma( &T::y ); }
	inline MMA1_double z() { return ma( &T::z ); }

	// string const Members
	inline MMA1c_string Name() const { return ma( &T::Name ); }

	// string Members
	inline MMA1_string Name() { return ma( &T::Name ); }

#endif // ObjexxFCL_MArray1_Project_MArray_hh_INCLUDED
