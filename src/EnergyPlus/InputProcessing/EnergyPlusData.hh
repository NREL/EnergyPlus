// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without the U.S. Department of Energy's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#ifndef EnergyPlusData_hh_INCLUDED
#define EnergyPlusData_hh_INCLUDED

#include <string>
#include <memory>
#include <functional>
#include <unordered_map>
#include <iterator>

// ObjexxFCL Headers

// EnergyPlus Headers
#include <EnergyPlus.hh>

#include <nlohmann/json.hpp>

namespace EnergyPlus {

template< typename T >
class ObjectPtr
{
public:

	ObjectPtr() = default;
	ObjectPtr( T * rhs ) : m_data( rhs ) {}
	ObjectPtr( ObjectPtr const & rhs ) : m_data( rhs.m_data ) {}

	inline T & operator*() const { return * m_data; }
	inline T * operator->() const { return m_data; }

private:
	T * m_data = nullptr;

};

template< typename T >
class ObjectVector
{
public:

	class iterator;
	class const_iterator;

	ObjectVector() = default;

	ObjectVector( T * ptr, std::size_t size ) :
		m_data( ptr ),
		m_size( size )
	{}

	using size_type = std::size_t;
	using difference_type = std::ptrdiff_t;

	// Active Array Size
	size_type
	size() const {
		return m_size;
	}

	// Active Array Empty?
	bool
	empty() const
	{
		return ( m_size == 0u );
	}

	// Begin Iterator
	const_iterator
	begin() const {
		return const_iterator( m_data );
	}

	// Begin Iterator
	iterator
	begin() {
		return iterator( m_data );
	}

	// End Iterator
	const_iterator
	end() const {
		return ( ( m_data != nullptr ) && ( m_size != 0 ) ? const_iterator( m_data + m_size ) : const_iterator() );
	}

	// End Iterator
	iterator
	end() {
		return ( ( m_data != nullptr ) && ( m_size != 0 ) ? iterator( m_data + m_size ) : iterator() );
	}

	// Reverse Begin Iterator
	const_iterator
	rbegin() const {
		return const_reverse_iterator( ( m_data != nullptr ) && ( m_size != 0 ) ? const_iterator( m_data + m_size ) : const_iterator() );
	}

	// Reverse Begin Iterator
	iterator
	rbegin() {
		return reverse_iterator( ( m_data != nullptr ) && ( m_size != 0 ) ? iterator( m_data + m_size ) : iterator() );
	}

	// Reverse End Iterator
	const_iterator
	rend() const {
		return const_iterator( m_data );
	}

	// Reverse End Iterator
	iterator
	rend() {
		return iterator( m_data );
	}

	// // Data Pointer
	// T const *
	// data() const {
	// 	return m_data;
	// }

	// // Data Pointer
	// T *
	// data() {
	// 	return m_data;
	// }

	// array[ i ] const: Linear Subscript
	T const &
	operator []( size_type const i ) const
	{
		assert( ( i < m_size ) || ( m_size == 0 ) );
		return m_data[ i ];
	}

	// array[ i ]: Linear Subscript
	T &
	operator []( size_type const i )
	{
		assert( ( i < m_size ) || ( m_size == 0 ) );
		return m_data[ i ];
	}

private:
	size_type m_size = 0;
	T * m_data = nullptr;

};

// taken from https://stackoverflow.com/a/31886483/2358662
template< typename T >
class ObjectVector< T >::iterator : public std::iterator< std::random_access_iterator_tag, T >
{
public:
	using difference_type = typename std::iterator< std::random_access_iterator_tag, T >::difference_type;

	iterator() : m_ptr( nullptr ) {}
	iterator( T* rhs ) : m_ptr( rhs ) {}
	iterator( const iterator &rhs ) : m_ptr( rhs.m_ptr ) {}
	inline iterator& operator+=( difference_type rhs ) { m_ptr += rhs; return *this; }
	inline iterator& operator-=( difference_type rhs ) { m_ptr -= rhs; return *this; }
	inline T& operator*() const { return *m_ptr; }
	inline T* operator->() const { return m_ptr; }
	inline T& operator[]( difference_type rhs ) const { return m_ptr[ rhs ]; }

	inline iterator& operator++() { ++m_ptr; return *this; }
	inline iterator& operator--() { --m_ptr; return *this; }
	inline iterator operator++( int ) const { iterator tmp( *this ); ++m_ptr; return tmp; }
	inline iterator operator--( int ) const { iterator tmp( *this ); --m_ptr; return tmp; }
	inline difference_type operator-(const iterator& rhs) const {return iterator( m_ptr - rhs.ptr ); }
	inline iterator operator+( difference_type rhs ) const { return iterator( m_ptr+rhs ); }
	inline iterator operator-( difference_type rhs ) const { return iterator( m_ptr-rhs ); }
	friend inline iterator operator+( difference_type lhs, const iterator& rhs ) { return iterator( lhs + rhs.m_ptr ); }
	friend inline iterator operator-( difference_type lhs, const iterator& rhs ) { return iterator( lhs - rhs.m_ptr ); }

	inline bool operator==( const iterator& rhs ) const { return m_ptr == rhs.m_ptr; }
	inline bool operator!=( const iterator& rhs ) const { return m_ptr != rhs.m_ptr; }
	inline bool operator>( const iterator& rhs ) const { return m_ptr > rhs.m_ptr; }
	inline bool operator<( const iterator& rhs ) const { return m_ptr < rhs.m_ptr; }
	inline bool operator>=( const iterator& rhs ) const { return m_ptr >= rhs.m_ptr; }
	inline bool operator<=( const iterator& rhs ) const { return m_ptr <= rhs.m_ptr; }
private:
	T * m_ptr;
};

template< typename T >
class ObjectVector< T >::const_iterator : public std::iterator< std::random_access_iterator_tag, T >
{
public:
	using difference_type = typename std::iterator< std::random_access_iterator_tag, T >::difference_type;

	const_iterator() : m_ptr( nullptr ) {}
	const_iterator( T const * rhs ) : m_ptr( rhs ) {}
	const_iterator( const const_iterator &rhs ) : m_ptr( rhs.m_ptr ) {}
	inline const_iterator& operator+=( difference_type rhs ) { m_ptr += rhs; return *this; }
	inline const_iterator& operator-=( difference_type rhs ) { m_ptr -= rhs; return *this; }
	inline T const & operator*() const { return *m_ptr; }
	inline T const * operator->() const { return m_ptr; }
	inline T const & operator[]( difference_type rhs ) const { return m_ptr[ rhs ]; }

	inline const_iterator& operator++() { ++m_ptr; return *this; }
	inline const_iterator& operator--() { --m_ptr; return *this; }
	inline const_iterator operator++( int ) const { const_iterator tmp( *this ); ++m_ptr; return tmp; }
	inline const_iterator operator--( int ) const { const_iterator tmp( *this ); --m_ptr; return tmp; }
	inline difference_type operator-(const const_iterator& rhs) const {return const_iterator( m_ptr - rhs.ptr ); }
	inline const_iterator operator+( difference_type rhs ) const { return const_iterator( m_ptr+rhs ); }
	inline const_iterator operator-( difference_type rhs ) const { return const_iterator( m_ptr-rhs ); }
	friend inline const_iterator operator+( difference_type lhs, const const_iterator& rhs ) { return const_iterator( lhs + rhs.m_ptr ); }
	friend inline const_iterator operator-( difference_type lhs, const const_iterator& rhs ) { return const_iterator( lhs - rhs.m_ptr ); }

	inline bool operator==( const const_iterator& rhs ) const { return m_ptr == rhs.m_ptr; }
	inline bool operator!=( const const_iterator& rhs ) const { return m_ptr != rhs.m_ptr; }
	inline bool operator>( const const_iterator& rhs ) const { return m_ptr > rhs.m_ptr; }
	inline bool operator<( const const_iterator& rhs ) const { return m_ptr < rhs.m_ptr; }
	inline bool operator>=( const const_iterator& rhs ) const { return m_ptr >= rhs.m_ptr; }
	inline bool operator<=( const const_iterator& rhs ) const { return m_ptr <= rhs.m_ptr; }
private:
	T const * m_ptr;
};

template< class T >
class ObjectTypeData
{
public:
	using json = nlohmann::json;

	ObjectTypeData() = default;

	T *
	addObject( std::string const & objectName, json const & fields ) {
		storage.emplace_back( objectName, fields );
		nameMap.emplace( objectName, storage.size() - 1 );
		return storage.data() + storage.size() - 1;
	}

	T *
	addObject( json const & fields ) {
		assert( storage.size() <= 1 );
		storage.emplace( fields );
		return storage.data();
	}

	ObjectVector< T >
	addObjects( json const & objs ) {
		assert( objs.is_object() );

		// std::vector< T * > output;

		storage.reserve( objs.size() );
		nameMap.reserve( objs.size() );
		// output.reserve( objs.size() );

		for ( auto it = objs.begin(); it != objs.end(); ++it ) {
			auto last_index = storage.size();
			storage.emplace_back( it.key(), it.value() );
			nameMap.emplace( it.key(), last_index );
			// output.emplace_back( storage.data() + last_index );
		}
		// return output;
		return ObjectVector< T >( storage.data(), storage.size() );
	}

	T *
	objectFactory( std::string const & objectName )
	{
		if ( storage.size() == 0 ) return nullptr;
		auto const it = nameMap.find( objectName );
		if ( it == nameMap.end() ) return nullptr;
		return storage.data() + it->second;
	}

	T *
	objectFactory()
	{
		assert( storage.size() <= 1 );
		if ( storage.size() == 0 ) return nullptr;
		return storage.data();
	}

	ObjectVector< T >
	objectsFactory()
	{
		// std::vector< T * > output;
		// output.reserve( storage.size() );
		// for ( std::size_t i = 0; i < storage.size(); ++i ) {
		// 	output.emplace_back( storage.data() + i );
		// }
		// return output;
		return ObjectVector< T >( storage.data(), storage.size() );
	}

	std::size_t
	numObjects() const
	{
		return storage.size();
	}

private:
	std::vector< T > storage;
	std::unordered_map< std::string, std::size_t > nameMap;
};

class EnergyPlusData
{
public:
	using json = nlohmann::json;

	template< typename T >
	T *
	addObject( std::string const & objectName, json const & fields )
	{
		T * ptr;
		auto const it = EPData.find( T::objectTypeHash() );
		if ( it != EPData.end() ) {
			auto cast_ptr = static_cast< ObjectTypeData< T > * >( it->second.get() );
			ptr = cast_ptr->addObject( objectName, fields );
		} else {
			auto obj = new ObjectTypeData< T >();
			ptr = obj->addObject( objectName, fields );
			EPData.emplace( T::objectTypeHash(), std::move( unique_void( obj ) ) );
		}
		return ptr;
	}

	template< typename T >
	T *
	addObject( json const & fields )
	{
		T * ptr;
		auto const it = EPData.find( T::objectTypeHash() );
		if ( it != EPData.end() ) {
			auto cast_ptr = static_cast< ObjectTypeData< T > * >( it->second.get() );
			ptr = cast_ptr->addObject( fields );
		} else {
			auto obj = new ObjectTypeData< T >();
			ptr = obj->addObject( fields );
			EPData.emplace( T::objectTypeHash(), std::move( unique_void( obj ) ) );
		}
		return ptr;
	}

	template< typename T >
	ObjectVector< T >
	addObjects( json const & objs )
	{
		assert( objs.is_object() );

		ObjectVector< T > ptrs;
		auto const it = EPData.find( T::objectTypeHash() );
		if ( it != EPData.end() ) {
			auto cast_ptr = static_cast< ObjectTypeData< T > * >( it->second.get() );
			ptrs = cast_ptr->addObjects( objs );
		} else {
			auto obj = new ObjectTypeData< T >();
			ptrs = obj->addObjects( objs );
			EPData.emplace( T::objectTypeHash(), std::move( unique_void( obj ) ) );
		}
		return ptrs;
	}

	template< typename T >
	T *
	objectFactory( std::string const & objectName )
	{
		auto const it = EPData.find( T::objectTypeHash() );
		if ( it == EPData.end() ) return nullptr;
		auto cast_ptr = static_cast< ObjectTypeData< T > * >( it->second.get() );
		return cast_ptr->objectFactory( objectName );
	}

	template< typename T >
	T *
	objectFactory()
	{
		auto const it = EPData.find( T::objectTypeHash() );
		if ( it == EPData.end() ) return nullptr;
		auto cast_ptr = static_cast< ObjectTypeData< T > * >( it->second.get() );
		return cast_ptr->objectFactory();
	}

	template< typename T >
	ObjectVector< T >
	objectsFactory()
	{
		auto const it = EPData.find( T::objectTypeHash() );
		if ( it == EPData.end() ) return nullptr;
		auto cast_ptr = static_cast< ObjectTypeData< T > * >( it->second.get() );
		return cast_ptr->objectsFactory();
	}

private:
	// taken from https://stackoverflow.com/a/39288979/2358662
	using deleter_t = std::function< void( void * ) >;
	using unique_void_ptr = std::unique_ptr< void, deleter_t >;

	template< typename T >
	auto unique_void( T * ptr ) -> unique_void_ptr
	{
		auto deleter = []( void * data ) -> void {
			T * p = static_cast< T * >( data );
			delete p;
		};
		return std::unique_ptr< void, deleter_t >( ptr, deleter );
	}

	std::unordered_map< std::size_t, unique_void_ptr > EPData;

};

}

#endif // EnergyPlusData_hh_INCLUDED
