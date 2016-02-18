#ifndef ObjexxFCL_noexcept_hh_INCLUDED
#define ObjexxFCL_noexcept_hh_INCLUDED

// NOEXCEPT Macro Definition
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.1.0
//
// Language: C++
//
// Copyright (c) 2000-2016 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

#if defined(_MSC_VER) && _MSC_VER < 1900 && !defined(__INTEL_COMPILER)
#define NOEXCEPT throw()
#else
#define NOEXCEPT noexcept
#endif

#endif // ObjexxFCL_noexcept_hh_INCLUDED
