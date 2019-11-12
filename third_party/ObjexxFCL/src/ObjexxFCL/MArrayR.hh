#ifndef ObjexxFCL_MArrayR_hh_INCLUDED
#define ObjexxFCL_MArrayR_hh_INCLUDED

// Rank Member Array Proxy Abstract Base Class Template
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2019 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/MArray.hh>

namespace ObjexxFCL {

// Rank Member Array Proxy Abstract Base Class Template
template< class A, typename T, int Rank >
class MArrayR : public MArray< A, T >
{

private: // Types

	using Super = MArray< A, T >;

private: // Friend

	template< typename, typename, int > friend class MArrayR;

public: // Types

	using ArrayType = typename Super::ArrayType;
	using Class = typename Super::Class;
	using MPtr = typename Super::MPtr;
	using Traits = typename Super::Traits;
	using IR = typename Super::IR;

	// STL Style
	using value_type = typename Super::value_type;
	using reference = typename Super::reference;
	using const_reference = typename Super::const_reference;
	using pointer = typename Super::pointer;
	using const_pointer = typename Super::const_pointer;
	using size_type = typename Super::size_type;
	using difference_type = typename Super::difference_type;

	// C++ Style
	using Value = typename Super::Value;
	using Reference = typename Super::Reference;
	using ConstReference = typename Super::ConstReference;
	using Pointer = typename Super::Pointer;
	using ConstPointer = typename Super::ConstPointer;
	using Size = typename Super::Size;
	using Difference = typename Super::Difference;

	using Super::isize;
	using Super::l;
	using Super::u;
	using Super::size;

protected: // Types

	using Super::in_range;

	using Super::array_;
	using Super::pmem_;

protected: // Creation

	// Copy Constructor
	MArrayR( MArrayR const & a ) :
	 Super( a )
	{
		assert( a.rank() == Rank );
	}

	// Constructor
	MArrayR( A & a, T Class::* pmem ) :
	 Super( a, pmem )
	{}

protected: // Assignment

	// Copy Assignment
	MArrayR &
	operator =( MArrayR const & a ); // Disallow

public: // Inspector

	// Rank
	int
	rank() const
	{
		return Rank;
	}

}; // MArrayR

} // ObjexxFCL

#endif // ObjexxFCL_MArrayR_hh_INCLUDED
