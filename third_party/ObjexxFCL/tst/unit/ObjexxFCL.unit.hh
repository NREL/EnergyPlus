// ObjexxFCL Unit Test Header
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

// Disable death tests if slow or (Windows) cause dialogs
#ifdef DISABLE_DEATH_TESTS

#undef ASSERT_DEATH
#define ASSERT_DEATH(statement, regex)

#undef ASSERT_DEBUG_DEATH
#define ASSERT_DEBUG_DEATH(statement, regex)

#undef EXPECT_DEBUG_DEATH
#define EXPECT_DEBUG_DEATH(statement, regex)

#undef EXPECT_DEATH
#define EXPECT_DEATH(statement, regex)

#endif
