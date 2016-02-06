#ifndef ObjexxFCL_vectorize_hh_INCLUDED
#define ObjexxFCL_vectorize_hh_INCLUDED

// Vectorization Support
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

// ObjexxFCL Headers
#include <ObjexxFCL/align.hh>

#ifndef __has_builtin
#define __has_builtin(x) 0
#endif
#if ( defined(__GNUC__) && ( ( __GNUC__ > 4 ) || ( ( __GNUC__ == 4 ) && ( __GNUC_MINOR__ >= 7 ) ) ) ) || ( defined(__clang__) && __has_builtin(__builtin_assume_aligned) )
#define ASSUME(b)
#define ASSUME_ALIGNED(p,b) p=(decltype(p))__builtin_assume_aligned(p,b)
#define ASSUME_ALIGNED_OBJEXXFCL(p) p=(decltype(p))__builtin_assume_aligned(p,OBJEXXFCL_ALIGN)
#define RESTRICT __restrict__
#elif defined(_WIN32) && ( defined(_MSC_VER) || defined(__INTEL_COMPILER) )
#define ASSUME(b) __assume(b)
#define ASSUME_ALIGNED(p,b) __assume_aligned(p,b)
#define ASSUME_ALIGNED_OBJEXXFCL(p) __assume_aligned(p,OBJEXXFCL_ALIGN)
#define RESTRICT __restrict
#else
#define ASSUME(b)
#define ASSUME_ALIGNED(p,b)
#define ASSUME_ALIGNED_OBJEXXFCL(p,b)
#define RESTRICT
#endif

#endif // ObjexxFCL_vectorize_hh_INCLUDED
