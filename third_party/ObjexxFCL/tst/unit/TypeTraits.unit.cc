// ObjexxFCL::TypeTraits Unit Tests
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

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/TypeTraits.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

class B
{
public:
	virtual
	~B()
	{}
};

class D : public B
{
public:
	virtual
	~D()
	{}
};

TEST( TypeTraitsTest, TypeTests )
{
	D d;
	B b, b2;

	EXPECT_TRUE( same_type_as( b, b ) );
	EXPECT_TRUE( same_type_as( b, b2 ) );
	EXPECT_TRUE( same_type_as( b2, b ) );
	EXPECT_FALSE( same_type_as( b, d ) );
	EXPECT_FALSE( same_type_as( d, b ) );

	EXPECT_TRUE( extends_type_of( b, b ) );
	EXPECT_TRUE( extends_type_of( d, d ) );
	EXPECT_TRUE( extends_type_of( d, b ) );
	EXPECT_FALSE( extends_type_of( b, d ) );

//	B * p( &d ); // polymorphic pointer
//	EXPECT_DEBUG_DEATH( extends_type_of( d, *p ), "Assertion failed!" );

	EXPECT_TRUE( is_a< B >( d ) );
	EXPECT_TRUE( is_a< D >( d ) );
	EXPECT_TRUE( is_a< B >( b ) );
	EXPECT_FALSE( is_a< D >( b ) );
}
