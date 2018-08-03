#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace FenestrationCommon;

class MMapTest : public testing::Test {

protected:
	void SetUp() override {
	}

};

TEST_F( MMapTest, TestDouble ) {
	SCOPED_TRACE( "Begin Test: Multimap with doubles." );

	enum class a { a1, a2, a3 };
	enum class b { b1, b2, b3 };

	mmap< double, a, b > aMap;
	aMap( a::a1, b::b1 ) = 1;
	aMap( a::a2, b::b2 ) = 2;

	EXPECT_EQ( 1, aMap.at( a::a1, b::b1 ) );
	EXPECT_EQ( 2, aMap.at( a::a2, b::b2 ) );

}

TEST_F( MMapTest, TestString ) {
	SCOPED_TRACE( "Begin Test: Multimap with strings." );

	enum class A { a1, a2, a3 };
	enum class B { b1, b2, b3 };

	mmap< std::string, A, B > aMap;
	aMap( A::a1, B::b1 ) = "Value1";
	aMap( A::a2, B::b2 ) = "Value2";

	EXPECT_EQ( "Value1", aMap.at( A::a1, B::b1 ) );
	EXPECT_EQ( "Value2", aMap.at( A::a2, B::b2 ) );

}
