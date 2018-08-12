#ifndef ObjexxFCL_lock_guard_hh_INCLUDED
#define ObjexxFCL_lock_guard_hh_INCLUDED

// LOCK_GUARD Macro Definition
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2018 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

#ifdef OBJEXXFCL_THREADS
#include <mutex>
#define OBJEXXFCL_LOCK_GUARD(m) std::lock_guard< std::mutex > lock( m )
#else
#define OBJEXXFCL_LOCK_GUARD(m)
#endif

#endif // ObjexxFCL_lock_guard_hh_INCLUDED
