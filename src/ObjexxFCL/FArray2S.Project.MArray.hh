#ifndef ObjexxFCL_FArray2S_Project_MArray_hh_INCLUDED
#define ObjexxFCL_FArray2S_Project_MArray_hh_INCLUDED

// FArray2S.Project.MArray: Project-Specific MArray Methods
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2014 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// Notes:
//  These are convenience methods to provide near-Fortran syntax for MArray usage

	// Types
	template< typename M > using MA2Sc = MArray2< FArray2S const, M >;
	template< typename M > using MA2S  = MArray2< FArray2S, M >;
	typedef  MA2Sc< bool >  MA2Sc_bool;
	typedef  MA2S< bool >  MA2S_bool;
	typedef  MA2Sc< int >  MA2Sc_int;
	typedef  MA2S< int >  MA2S_int;
	typedef  MA2Sc< double >  MA2Sc_double;
	typedef  MA2S< double >  MA2S_double;
	typedef  MA2Sc< Fstring >  MA2Sc_Fstring;
	typedef  MA2S< Fstring >  MA2S_Fstring;

#endif // ObjexxFCL_FArray2S_Project_MArray_hh_INCLUDED
